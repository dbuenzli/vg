(*---------------------------------------------------------------------------
   Copyright 2013 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

open Gg
open Vg
open Vgr.Private.Data

type cairo_primitive = Pattern : 'a Cairo.Pattern.t -> cairo_primitive

let dumb_prim = Pattern (Cairo.Pattern.create_rgba 0.0 0.0 0.0 0.0)

type gstate =     (* subset of the graphics state saved by a Cairo.save ctx *)
  { mutable g_tr : M3.t;              (* current transform without view_tr. *)
    mutable g_outline : P.outline;               (* current outline stroke. *)
    mutable g_stroke : cairo_primitive;            (* current stroke color. *)
    mutable g_fill : cairo_primitive; }              (* current fill color. *)

let init_gstate =
  { g_tr = M3.id; g_outline = P.o; g_stroke = dumb_prim; g_fill = dumb_prim }

type cmd = Set of gstate | Draw of Vgr.Private.Data.image
type state =
  { r : Vgr.Private.renderer;                    (* corresponding renderer. *)
    surface : Cairo.Surface.t;                      (* surface rendered to. *)
    ctx : Cairo.context;                           (* context of [surface]. *)
    mutable cost : int;                          (* cost counter for limit. *)
    mutable view : Gg.box2;           (* current renderable view rectangle. *)
    mutable view_tr : M3.t;                    (* view to canvas transform. *)
    mutable todo : cmd list;                        (* commands to perform. *)
    prims :                                           (* cached primitives. *)
      (Vgr.Private.Data.primitive, cairo_primitive) Hashtbl.t;
    mutable gstate : gstate; }                    (* current graphic state. *)

let save_gstate s = Set { s.gstate with g_tr = s.gstate.g_tr }
let set_gstate s g = s.gstate <- g

let partial = Vgr.Private.partial
let limit s = Vgr.Private.limit s.r
let warn s w = Vgr.Private.warn s.r w
let image i = Vgr.Private.I.of_data i

let view_rect s =           (* image view rect in current coordinate system. *)
  let tr = M3.inv s.gstate.g_tr in
  Vgr.Private.Data.of_path (P.empty >> P.rect (Box2.tr tr s.view))

let cairo_matrix xx yx xy yy x0 y0 =
  { Cairo.xx; yx; xy; yy; x0; y0 }

let cairo_matrix_of_m3 m =
  M3.(cairo_matrix (e00 m) (e10 m) (e01 m) (e11 m) (e02 m) (e12 m))

let cairo_cap = function
  | `Butt -> Cairo.BUTT
  | `Round -> Cairo.ROUND
  | `Square -> Cairo.SQUARE

let cairo_join = function
  | `Bevel -> Cairo.JOIN_BEVEL
  | `Round -> Cairo.JOIN_ROUND
  | `Miter -> Cairo.JOIN_MITER

let cairo_fill_rule = function
  | `Anz -> Cairo.WINDING
  | `Aeo -> Cairo.EVEN_ODD
  | `O _ -> assert false

let set_dashes s = function
  | None -> Cairo.set_dash s.ctx [||]
  | Some (offset, dashes) ->
      let dashes = Array.of_list dashes in
      Cairo.set_dash s.ctx ~ofs:offset dashes

let init_ctx s =
  let o = s.gstate.g_outline in
  let m = s.view_tr in
  Cairo.transform s.ctx (cairo_matrix_of_m3 m);
  Cairo.set_line_width s.ctx o.P.width;
  Cairo.set_line_cap s.ctx (cairo_cap o.P.cap);
  Cairo.set_line_join s.ctx (cairo_join o.P.join);
  Cairo.set_miter_limit s.ctx (Vgr.Private.P.miter_limit o);
  set_dashes s o.P.dashes

let push_transform s tr =
  let m = match tr with
  | Move v -> Cairo.translate s.ctx (V2.x v) (V2.y v); M3.move2 v
  | Rot a -> Cairo.rotate s.ctx a; M3.rot2 a
  | Scale sv -> Cairo.scale s.ctx (V2.x sv) (V2.y sv); M3.scale2 sv
  | Matrix m -> Cairo.transform s.ctx (cairo_matrix_of_m3 m); m
  in
  s.gstate.g_tr <- M3.mul s.gstate.g_tr m

let set_outline s o =
  if s.gstate.g_outline == o then () else
  let old = s.gstate.g_outline in
  s.gstate.g_outline <- o;
  if old.P.width <> o.P.width then (Cairo.set_line_width s.ctx o.P.width);
  if old.P.cap <> o.P.cap then (Cairo.set_line_cap s.ctx (cairo_cap o.P.cap));
  if old.P.join <> o.P.join then (Cairo.set_line_join s.ctx (cairo_join o.P.join));
  if old.P.miter_angle <> o.P.miter_angle then
    (Cairo.set_miter_limit s.ctx (Vgr.Private.P.miter_limit o));
  if old.P.dashes <> o.P.dashes then set_dashes s o.P.dashes;
  ()

let get_primitive s p = try Hashtbl.find s.prims p with
| Not_found ->
    let add_stop g (t, c) =
      Cairo.Pattern.add_color_stop_rgba g ~ofs:t
        (Color.r c) (Color.g c) (Color.b c) (Color.a c) in
    let create = function
    | Const c ->
        Pattern Color.(Cairo.Pattern.create_rgba (r c) (g c) (b c) (a c))
    | Axial (stops, pt, pt') ->
        let g = V2.(Cairo.Pattern.create_linear (x pt) (y pt) (x pt') (y pt')) in
        List.iter (add_stop g) stops; Pattern g
    | Radial (stops, f, c, r) ->
        let g = V2.(Cairo.Pattern.create_radial
                      ~x0:(x f) ~y0:(y f) ~x1:(x c) ~y1:(y c) ~r0:0.0 ~r1:r) in
        List.iter (add_stop g) stops; Pattern g
    | Raster _ -> assert false
    in
    let prim = create p in
    Hashtbl.add s.prims p prim; prim

let set_source s p =
  let p = get_primitive s p in
  if s.gstate.g_stroke != p then begin match p with
  | Pattern g -> Cairo.set_source s.ctx g
  end;
  p

let set_stroke s p = s.gstate.g_stroke <- set_source s p

let set_fill s p = s.gstate.g_fill <- set_source s p


let set_path s p =
  let rec loop last = function
  | [] -> ()
  | seg :: segs ->
      match seg with
      | `Sub pt -> P2.(Cairo.move_to s.ctx (x pt) (y pt)); loop pt segs
      | `Line pt -> P2.(Cairo.line_to s.ctx (x pt) (y pt)); loop pt segs
      | `Qcurve (c, pt) ->
          failwith "todo";
          loop pt segs
      | `Ccurve (c, c', pt) ->
          P2.(Cairo.curve_to s.ctx (x c) (y c) (x c') (y c') (x pt) (y pt));
          loop pt segs
      | `Earc (large, cw, r, a, pt) ->
          begin match Vgr.Private.P.earc_params last large cw r a pt with
          | None -> P2.(Cairo.line_to s.ctx (x pt) (y pt)); loop pt segs
          | Some (c, m, a, a') ->
              Cairo.save s.ctx;
              let c = V2.ltr (M2.inv m) c in
              M2.(Cairo.transform s.ctx (cairo_matrix (e00 m) (e10 m) (e01 m) (e11 m) 0. 0.));
              let arc = if cw then Cairo.arc else Cairo.arc_negative in
              P2.(arc s.ctx ~x:(x c) ~y:(y c) ~r:1.0 ~a1:a ~a2:a');
              Cairo.restore s.ctx;
              loop pt segs
          end
      | `Close -> Cairo.Path.close s.ctx; loop last (* we don't care *) segs
  in
  Cairo.Path.clear s.ctx;
  loop P2.o (List.rev p)

let rec r_cut s a = function
| Primitive (Raster _) -> assert false
| Primitive p ->
    begin match a with
    | `O o -> set_outline s o; set_stroke s p; Cairo.stroke s.ctx
    | `Aeo | `Anz ->
        set_fill s p;
        Cairo.set_fill_rule s.ctx (cairo_fill_rule a);
        Cairo.fill s.ctx
    end
| Tr (tr, i) ->
    Cairo.save s.ctx;
    s.todo <- (save_gstate s) :: s.todo;
    push_transform s tr;
    r_cut s a i
| Blend _ | Cut _ | Cut_glyphs _ as i ->
    let a = match a with
    | `O _ -> warn s (`Unsupported_cut (a, image i)); `Anz
    | a -> a
    in
    Cairo.save s.ctx;
    Cairo.set_fill_rule s.ctx (cairo_fill_rule a);
    Cairo.clip s.ctx;
    s.todo <- (Draw i) :: (save_gstate s) :: s.todo

let rec r_image s k r =
  if s.cost > limit s then (s.cost <- 0; partial (r_image s k) r) else
  match s.todo with
  | [] -> k r
  | Set gs :: todo ->
      Cairo.restore s.ctx;
      set_gstate s gs;
      s.todo <- todo;
      r_image s k r
  | Draw i :: todo ->
      s.cost <- s.cost + 1;
      match i with
      | Primitive _ as i ->            (* Uncut primitive, just cut to view. *)
          let p = view_rect s in
          s.todo <- (Draw (Cut (`Anz, p, i))) :: todo;
          r_image s k r
      | Cut (a, p, i) ->
          s.todo <- todo;
          set_path s p;
          r_cut s a i;
          r_image s k r
      | Cut_glyphs (a, run, i) ->
          s.todo <- todo;
          failwith "todo"
          (*r_cut_glyphs s a run i;*)
          r_image s k r
      | Blend (_, _, i, i') ->
          s.todo <- (Draw i') :: (Draw i) :: todo;
          r_image s k r
      | Tr (tr, i) ->
          Cairo.save s.ctx;
          s.todo <- (Draw i) :: (save_gstate s) :: todo;
          push_transform s tr;
          r_image s k r

let render s v k r = match v with
| `End -> k r
| `Image (size, view, i) ->
    let cw = float (Cairo.Image.get_width s.surface) in
    let ch = float (Cairo.Image.get_height s.surface) in
    (* Map view rect (bot-left coords) to surface (top-left coords) *)
    let sx = cw /. Box2.w view in
    let sy = ch /. Box2.h view in
    let dx = -. Box2.ox view *. sx in
    let dy = ch +. Box2.oy view *. sy in
    let view_tr = M3.v sx       0. dx
                       0. (-. sy)  dy
                       0.       0. 1.
    in
    s.cost <- 0;
    s.view <- view;
    s.view_tr <- view_tr;
    s.todo <- [ Draw i ];
    s.gstate <- { init_gstate with g_tr = init_gstate.g_tr }; (* copy *)
    init_ctx s;
    r_image s k r

let target surface =
  let target r _ =
    let ctx = Cairo.create surface in
    true, render { r; surface; ctx;
                   cost = 0;
                   view = Box2.empty;
                   view_tr = M3.id;
                   todo = [];
                   prims = Hashtbl.create 231;
                   gstate = init_gstate; }
  in
  Vgr.Private.create_target target
