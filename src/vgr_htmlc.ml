(*---------------------------------------------------------------------------
   Copyright (c) 2013 The vg programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Gg
open Vg
open Vgr.Private.Data
open Brr_canvas

(* Brr does not give us the immediate mode API. It was not immediatly
   clear how to do without when a r_cut goes through a transform. If
   we thread the path until fill or stroke happens we no longer have
   the right coordinate system.  Apparently it's not possible to set
   the current path via a Path2D, so we need to bind the calls to
   act on the current path. Maybe we should review the whole strategy
   so that Path2D objects can be used. *)

module C2di = struct
  let ctx = Brr_canvas.C2d.to_jv
  let fill ?(fill_rule = Brr_canvas.C2d.Fill_rule.nonzero) c =
    ignore @@ Jv.call (ctx c) "fill" [| Jv.of_jstr fill_rule |]

  let stroke c = ignore @@ Jv.call (ctx c) "stroke" [||]
  let clip ?(fill_rule = Brr_canvas.C2d.Fill_rule.nonzero) c =
    ignore @@ Jv.call (ctx c) "clip" [| Jv.of_jstr fill_rule |]

  let begin_path c = ignore @@ Jv.call (ctx c) "beginPath" [||]
  let close c = ignore @@ Jv.call (ctx c) "closePath" [||]
  let move_to c ~x ~y =
    ignore @@ Jv.call (ctx c) "moveTo" Jv.[| of_float x; of_float y |]

  let line_to c ~x ~y =
    ignore @@ Jv.call (ctx c) "lineTo" Jv.[| of_float x; of_float y |]

  let qcurve_to c ~cx ~cy ~x ~y =
    ignore @@ Jv.call (ctx c) "quadraticCurveTo"
      Jv.[| of_float cx; of_float cy; of_float x; of_float y |]

  let ccurve_to c ~cx ~cy ~cx' ~cy' ~x ~y =
    ignore @@ Jv.call (ctx c) "bezierCurveTo"
      Jv.[| of_float cx; of_float cy; of_float cx'; of_float cy';
            of_float x; of_float y|]

  let ellipse ?(anticlockwise = false) c ~cx ~cy ~rx ~ry ~rot ~start ~stop =
    ignore @@ Jv.call (ctx c) "ellipse"
      Jv.[| of_float cx; of_float cy; of_float rx; of_float ry; of_float rot;
            of_float start; of_float stop; of_bool anticlockwise |]
end


(* Renderer *)

type js_font = Jstr.t

let dumb_prim = C2d.color Jstr.empty

type gstate =        (* subset of the graphics state saved by a C2d.save (). *)
  { mutable g_tr : M3.t;              (* current transform without view_tr. *)
    mutable g_outline : P.outline;               (* current outline stroke. *)
    mutable g_stroke : C2d.style;                  (* current stroke color. *)
    mutable g_fill : C2d.style; }                    (* current fill color. *)

let init_gstate =
  { g_tr = M3.id; g_outline = P.o; g_stroke = dumb_prim; g_fill = dumb_prim }

type cmd = Set of gstate | Draw of Vgr.Private.Data.image
type state =
  { r : Vgr.Private.renderer;                    (* corresponding renderer. *)
    resize : bool;         (* [true] if canvas resizes to renderable sizes. *)
    resolution : Gg.v2;                        (* resolution of the canvas. *)
    c : Canvas.t;                            (* canvas element rendered to. *)
    ctx : C2d.t;                                  (* canvas context of [c]. *)
    mutable cost : int;                          (* cost counter for limit. *)
    mutable view : Gg.box2;           (* current renderable view rectangle. *)
    mutable view_tr : M3.t;                    (* view to canvas transform. *)
    mutable todo : cmd list;                        (* commands to perform. *)
    fonts : (Vg.font * float, js_font) Hashtbl.t;          (* cached fonts. *)
    prims :                                           (* cached primitives. *)
      (Vgr.Private.Data.primitive, C2d.style) Hashtbl.t;
    mutable gstate : gstate; }                    (* current graphic state. *)

let save_gstate s = Set { s.gstate with g_tr = s.gstate.g_tr }
let set_gstate s g = s.gstate <- g

let partial = Vgr.Private.partial
let limit s = Vgr.Private.limit s.r
let warn s w = Vgr.Private.warn s.r w
let image i = Vgr.Private.I.of_data i

let view_rect s =           (* image view rect in current coordinate system. *)
  let tr = M3.inv s.gstate.g_tr in
  Vgr.Private.Data.of_path (P.empty |> P.rect (Box2.tr tr s.view))

let sep = Jstr.v ","
let rpar = Jstr.v ")"
let css_color c =
  let srgb = Color.to_srgb c in
  let r = Float.int_of_round (Color.r srgb *. 255.) in
  let g = Float.int_of_round (Color.g srgb *. 255.) in
  let b = Float.int_of_round (Color.b srgb *. 255.) in
  let a = Color.a srgb in
  if a = 1.0
  then Jstr.(v "rgb(" + of_int r + sep + of_int g + sep + of_int b + rpar)
  else Jstr.(v "rgba(" + of_int r + sep + of_int g + sep + of_int b + sep +
             of_float a + rpar)

let cap_str =
  let butt = Jstr.v "butt" in
  let round = Jstr.v "round" in
  let square = Jstr.v "square" in
  function `Butt -> butt | `Round -> round | `Square -> square

let join_str =
  let bevel = Jstr.v "bevel" in
  let round = Jstr.v "round" in
  let miter = Jstr.v "miter" in
  function `Bevel -> bevel | `Round -> round | `Miter -> miter

let area_str =
  let nz = Jstr.v "nonzero" in
  let eo = Jstr.v "evenodd" in
  function `Anz -> nz | `Aeo -> eo | `O _ -> assert false

let get_primitive s p = try Hashtbl.find s.prims p with
| Not_found ->
    let make_stops stops = List.map (fun (s, c) -> s, css_color c) stops in
    let create = function
    | Const c -> C2d.color (css_color c)
    | Axial (stops, pt, pt') ->
        let stops = make_stops stops in
        let x0 = P2.x pt and y0 = P2.y pt and x1 = P2.x pt' and y1 = P2.y pt' in
        let g = C2d.linear_gradient s.ctx ~x0 ~y0 ~x1 ~y1 ~stops in
        C2d.gradient_style g
    | Radial (stops, f, c, r) ->
        let stops = make_stops stops in
        let x0 = P2.x f and y0 = P2.y f and x1 = P2.x c and y1 = P2.y c in
        let g = C2d.radial_gradient s.ctx ~x0 ~y0 ~r0:0. ~x1 ~y1 ~r1:r ~stops in
        C2d.gradient_style g
    | Raster _ -> assert false
    in
    let style = create p in
    Hashtbl.add s.prims p style; style

let get_font s (font, size as spec) = try Hashtbl.find s.fonts spec with
| Not_found ->
    let js_font =
      let font = { font with Font.size = size } in
      Jstr.v (Vgr.Private.Font.css_font ~unit:"px" font)
    in
    Hashtbl.add s.fonts spec js_font; js_font

let set_dashes s = function
| None -> C2d.set_line_dash s.ctx []
| Some (offset, dashes) ->
    C2d.set_line_dash_offset s.ctx offset;
    C2d.set_line_dash s.ctx dashes

let init_ctx s w h =
  let o = s.gstate.g_outline in
  let m = s.view_tr in
  (* clear canvas *)
  C2d.reset_transform s.ctx (* set_transform identity *);
  C2d.clear_rect s.ctx ~x:0. ~y:0. ~w ~h;
  (* setup base state *)
  M3.(C2d.set_transform' s.ctx
        ~a:(e00 m) ~b:(e10 m) ~c:(e01 m) ~d:(e11 m) ~e:(e02 m) ~f:(e12 m));
  C2d.set_line_width s.ctx o.P.width;
  C2d.set_line_cap s.ctx (cap_str o.P.cap);
  C2d.set_line_join s.ctx (join_str o.P.join);
  C2d.set_miter_limit s.ctx (Vgr.Private.P.miter_limit o);
  set_dashes s o.P.dashes

let push_transform s tr =
  let m = match tr with
  | Move v -> C2d.translate s.ctx ~x:(V2.x v) ~y:(V2.y v); M3.move2 v
  | Rot a -> C2d.rotate s.ctx a; M3.rot2 a
  | Scale sv -> C2d.scale s.ctx ~sx:(V2.x sv) ~sy:(V2.y sv); M3.scale2 sv
  | Matrix m ->
      M3.(C2d.transform' s.ctx
            ~a:(e00 m) ~b:(e10 m) ~c:(e01 m) ~d:(e11 m) ~e:(e02 m) ~f:(e12 m));
      m
  in
  s.gstate.g_tr <- M3.mul s.gstate.g_tr m

let set_outline s o =
  if s.gstate.g_outline == o then () else
  let old = s.gstate.g_outline in
  s.gstate.g_outline <- o;
  if old.P.width <> o.P.width then C2d.set_line_width s.ctx o.P.width;
  if old.P.cap <> o.P.cap then C2d.set_line_cap s.ctx (cap_str o.P.cap);
  if old.P.join <> o.P.join then C2d.set_line_join s.ctx (join_str o.P.join);
  if old.P.miter_angle <> o.P.miter_angle then
    (C2d.set_miter_limit s.ctx (Vgr.Private.P.miter_limit o));
  if old.P.dashes <> o.P.dashes then set_dashes s o.P.dashes;
  ()

let set_stroke s p =
  let p = get_primitive s p in
  if s.gstate.g_stroke != p
  then (C2d.set_stroke_style s.ctx p; s.gstate.g_stroke <- p)

let set_fill s p =
  let p = get_primitive s p in
  if s.gstate.g_fill != p
  then (C2d.set_fill_style s.ctx p; s.gstate.g_fill <- p)

let set_font s f = C2d.set_font s.ctx (get_font s f)
let set_path s p =
  (* N.B. We should try again to use Path2d and cache them in [s] *)
  let rec loop ctx last = function
  | [] -> ()
  | seg :: segs ->
      match seg with
      | `Sub pt -> C2di.move_to ctx ~x:(P2.x pt) ~y:(P2.y pt); loop ctx pt segs
      | `Line pt -> C2di.line_to ctx ~x:(P2.x pt) ~y:(P2.y pt); loop ctx pt segs
      | `Qcurve (c, pt) ->
          let cx = P2.x c and cy = P2.y c and x = P2.x pt and y = P2.y pt in
          C2di.qcurve_to ctx ~cx ~cy ~x ~y; loop ctx pt segs
      | `Ccurve (c, c', pt) ->
          let cx = P2.x c and cy = P2.y c and cx' = P2.x c' and cy' = P2.y c' in
          C2di.ccurve_to ctx ~cx ~cy ~cx' ~cy' ~x:(P2.x pt) ~y:(P2.y pt);
          loop ctx pt segs
      | `Earc (large, cw, rot, r, pt) ->
          begin match Vgr.Private.P.earc_params last ~large ~cw rot r pt with
          | None ->
              C2di.line_to ctx ~x:(P2.x pt) ~y:(P2.y pt); loop ctx pt segs
          | Some (c, _m, a, a') ->
              let cx = P2.x c and cy = P2.y c and rx = P2.x r and ry = P2.y r in
              C2di.ellipse ~anticlockwise:cw ctx ~cx ~cy ~rx ~ry ~rot
                ~start:a ~stop:a';
              loop ctx pt segs
          end
      | `Close -> C2di.close ctx; loop ctx last (* we don't care *) segs
  in
  C2di.begin_path s.ctx;
  loop s.ctx P2.o (List.rev p)

(* The way glyph_cuts is implemented is a little bit odd because
   current browsers are completly broken w.r.t. canvas text drawing:
   it is impossible to specify text that is smaller than 1px which
   means that the minimal size absurdly depends on the current
   transformation matrix (see http://jsfiddle.net/Fk3fc/2/).  Besides
   we also get problems by the fact that we flip the coordinate
   system.  To circumvent these we extract from the current transform
   the rotation, scaling and translation in the original,
   untransformed, coordinate system and draw the glyphs there.
   This however makes it tricky to integrate with gradients, for
   now we just disallow gradient cuts. *)

let rec r_cut_glyphs s a run = function
| Primitive (Raster _ | Axial _ | Radial _) | Tr _ | Blend _ | Cut _
| Cut_glyphs _ as i -> warn s (`Unsupported_glyph_cut (a, image i))
| Primitive p as i ->
    begin match run.text with
    | None -> warn s (`Textless_glyph_cut (image (Cut_glyphs (a, run, i))))
    | Some text ->
        let text = Jstr.v text in
        C2d.save s.ctx;
        s.todo <- (save_gstate s) :: s.todo;
        let m = M3.mul s.view_tr s.gstate.g_tr in
        let o = P2.tr m run.o in
        let font_size = V2.norm (V2.tr m (V2.v 0. run.font.Font.size)) in
        let y_scale = 1. /. V2.norm (V2.tr s.gstate.g_tr V2.oy) in
        let x_scale =
          (* we don't apply the view transform but still need to scale
             for aspect ratio distortions. *)
          let sa = (float (Canvas.w s.c)) /. (float (Canvas.h s.c)) in
          let va = Box2.w s.view /. Box2.h s.view in
          sa /. va
        in
        set_font s (run.font, font_size);
        M3.(C2d.set_transform' s.ctx
              ~a:(e00 s.gstate.g_tr *. x_scale)
              ~b:(-. e10 s.gstate.g_tr *. x_scale)
              ~c:(-. e01 s.gstate.g_tr *. y_scale)
              ~d:(e11 s.gstate.g_tr *. y_scale)
              ~e:(V2.x o) ~f:(V2.y o));
        begin match a with
        | `O o ->
            warn s (`Unsupported_glyph_cut (a, image i))
            (* some work is needed here as the outline params won't
               be in the right coordinate system *)
            (* set_outline s o; set_stroke s p;
               s.ctx ## strokeText (text, 0., 0.) *)
        | `Aeo | `Anz -> set_fill s p; C2d.fill_text s.ctx text ~x:0. ~y:0.
        end;
    end

let rec r_cut s a = function
| Primitive (Raster _) -> assert false
| Primitive p ->
    begin match a with
    | `O o -> set_outline s o; set_stroke s p; C2di.stroke s.ctx
    | `Aeo | `Anz -> set_fill s p; C2di.fill ~fill_rule:(area_str a) s.ctx
    end
| Tr (tr, i) ->
    C2d.save s.ctx;
    s.todo <- (save_gstate s) :: s.todo;
    push_transform s tr;
    r_cut s a i
| Blend _ | Cut _ | Cut_glyphs _ as i ->
    let a = match a with
    | `O _ -> warn s (`Unsupported_cut (a, image i)); `Anz
    | a -> a
    in
    C2d.save s.ctx;
    C2di.clip s.ctx ~fill_rule:(area_str a);
    s.todo <- (Draw i) :: (save_gstate s) :: s.todo

let rec r_image s k r =
  if s.cost > limit s then (s.cost <- 0; partial (r_image s k) r) else
  match s.todo with
  | [] -> Hashtbl.reset s.prims; Hashtbl.reset s.fonts; k r
  | Set gs :: todo ->
      C2d.restore s.ctx;
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
          r_cut_glyphs s a run i;
          r_image s k r
      | Blend (_, _, i, i') ->
          s.todo <- (Draw i') :: (Draw i) :: todo;
          r_image s k r
      | Tr (tr, i) ->
          C2d.save s.ctx;
          s.todo <- (Draw i) :: (save_gstate s) :: todo;
          push_transform s tr;
          r_image s k r

let render s v k r = match v with
| `End -> k r
| `Image (size, view, i) ->
    let size =
      if s.resize then begin
        let to_css_mm mag = Jstr.(of_float mag + v "mm") in
        let new_w = to_css_mm (Size2.w size) in
        let new_h = to_css_mm (Size2.h size) in
        let c = Canvas.to_el s.c in
        let cw_css = Brr.El.inline_style Brr.El.Style.width c in
        let ch_css = Brr.El.inline_style Brr.El.Style.height c in
        if not (Jstr.equal cw_css new_w)
        then Brr.El.set_inline_style Brr.El.Style.width new_w c;
        if not (Jstr.equal ch_css new_h)
        then Brr.El.set_inline_style Brr.El.Style.height new_h c;
        size
      end else begin
        let c = Canvas.to_el s.c in
        let w = Brr.El.bound_w c in (* in CSS pixels *)
        let h = Brr.El.bound_h c in (* in CSS pixels *)
        let to_mm = (2.54 /. 96.) *. 10. in
        Size2.v (w *. to_mm) (h *. to_mm)
      end
    in
    let cw = Float.round ((Size2.w size /. 1000.) *. (V2.x s.resolution)) in
    let ch = Float.round ((Size2.h size /. 1000.) *. (V2.y s.resolution)) in
    let cwi = int_of_float cw in
    let chi = int_of_float ch in
    if Canvas.w s.c <> cwi then Canvas.set_w s.c cwi;
    if Canvas.h s.c <> chi then Canvas.set_h s.c chi;
    (* Map view rect (bot-left coords) to canvas (top-left coords) *)
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
    init_ctx s cw ch;
    r_image s k r

let screen_resolution = (* in pixel per meters *)
  let device_pixel_ratio = Brr.Window.device_pixel_ratio Brr.G.window in
  let screen = (96. /. 2.54) *. 100. *. device_pixel_ratio in
  V2.v screen screen

let target ?(resize = true) ?(resolution = screen_resolution) c =
  let target r _ =
    let c = Canvas.of_jv (Obj.magic c : Jv.t) in
    let ctx = C2d.create c in
    true, render { r; c; ctx;
                   resize;
                   resolution;
                   cost = 0;
                   view = Box2.empty;
                   view_tr = M3.id;
                   todo = [];
                   fonts = Hashtbl.create 20;
                   prims = Hashtbl.create 231;
                   gstate = init_gstate; }
  in
  Vgr.Private.create_target target

(*---------------------------------------------------------------------------
   Copyright (c) 2013 The vg programmers

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
