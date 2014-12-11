(*---------------------------------------------------------------------------
   Copyright 2014 Arthur Wendling, Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(* Based on the Vgr_htmlc implementation by Daniel C. Bünzli. *)

open Gg
open Vg
open Vgr.Private.Data

type cairo_font = Font : 'a Cairo.Font_face.t -> cairo_font
type cairo_primitive = Pattern : 'a Cairo.Pattern.t -> cairo_primitive

let dumb_prim = Pattern (Cairo.Pattern.create_rgba 0.0 0.0 0.0 0.0)

type gstate =     (* subset of the graphics state saved by a Cairo.save ctx *)
  { mutable g_tr : M3.t;              (* current transform without view_tr. *)
    mutable g_outline : P.outline;               (* current outline stroke. *)
    mutable g_stroke : cairo_primitive;            (* current stroke color. *)
    mutable g_fill : cairo_primitive; }              (* current fill color. *)

let init_gstate =
  { g_tr = M3.id; g_outline = P.o; g_stroke = dumb_prim; g_fill = dumb_prim }

type cairo_backend = [ `Surface | `PDF | `PNG | `PS | `SVG ]

type cmd = Set of gstate | Draw of Vgr.Private.Data.image
type state =
  { r : Vgr.Private.renderer;                    (* corresponding renderer. *)
    backend : cairo_backend;                        (* final format target. *)
    mutable size : Size2.t;                          (* surface dimensions. *)
    surface : Cairo.Surface.t;                      (* surface rendered to. *)
    ctx : Cairo.context;                           (* context of [surface]. *)
    mutable cost : int;                          (* cost counter for limit. *)
    mutable view : Gg.box2;           (* current renderable view rectangle. *)
    mutable view_tr : M3.t;                    (* view to canvas transform. *)
    mutable todo : cmd list;                        (* commands to perform. *)
    fonts : (Vg.font, cairo_font) Hashtbl.t;               (* cached fonts. *)
    prims :                                           (* cached primitives. *)
      (Vgr.Private.Data.primitive, cairo_primitive) Hashtbl.t;
    mutable gstate : gstate; }                    (* current graphic state. *)

let save_gstate s = Set { s.gstate with g_tr = s.gstate.g_tr }
let set_gstate s g = s.gstate <- g

let partial = Vgr.Private.partial
let limit s = Vgr.Private.limit s.r
let warn s w = Vgr.Private.warn s.r w
let image i = Vgr.Private.I.of_data i

let view_rect s =          (* image view rect in current coordinate system. *)
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
  Cairo.restore s.ctx;
  Cairo.save s.ctx;
  Cairo.transform s.ctx (cairo_matrix_of_m3 m);
  Cairo.set_line_width s.ctx o.P.width;
  Cairo.set_line_cap s.ctx (cairo_cap o.P.cap);
  Cairo.set_line_join s.ctx (cairo_join o.P.join);
  Cairo.set_miter_limit s.ctx (Vgr.Private.P.miter_limit o);
  set_dashes s o.P.dashes;
  Cairo.set_operator s.ctx Cairo.CLEAR;
  let w = float (Cairo.Image.get_width s.surface) in
  let h = float (Cairo.Image.get_height s.surface) in
  Cairo.rectangle s.ctx 0. 0. w h;
  Cairo.fill s.ctx;
  Cairo.set_operator s.ctx Cairo.OVER


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
  if old.P.width <> o.P.width then
    (Cairo.set_line_width s.ctx o.P.width);
  if old.P.cap <> o.P.cap then
    (Cairo.set_line_cap s.ctx (cairo_cap o.P.cap));
  if old.P.join <> o.P.join then
    (Cairo.set_line_join s.ctx (cairo_join o.P.join));
  if old.P.miter_angle <> o.P.miter_angle then
    (Cairo.set_miter_limit s.ctx (Vgr.Private.P.miter_limit o));
  if old.P.dashes <> o.P.dashes then set_dashes s o.P.dashes;
  ()

let get_primitive s p = try Hashtbl.find s.prims p with
| Not_found ->
    let add_stop g (t, c) =
      let c = Color.to_srgb c in
      Cairo.Pattern.add_color_stop_rgba g ~ofs:t
        (V4.x c) (V4.y c) (V4.z c) (V4.w c) in
    let create = function
    | Const c ->
        let c = Color.to_srgb c in
        Pattern V4.(Cairo.Pattern.create_rgba (x c) (y c) (z c) (w c))
    | Axial (stops, pt, pt') ->
        let g = V2.(Cairo.Pattern.create_linear (x pt)  (y pt)
                                                (x pt') (y pt')) in
        List.iter (add_stop g) stops; Pattern g
    | Radial (stops, f, c, r) ->
        let g = V2.(Cairo.Pattern.create_radial
                      (x f) (y f) 0.0 (x c) (y c) r) in
        List.iter (add_stop g) stops; Pattern g
    | Raster _ -> assert false
    in
    let prim = create p in
    Hashtbl.add s.prims p prim; prim

let get_font s font = try Hashtbl.find s.fonts font with
| Not_found ->
    let cairo_font =
      let slant = match font.Font.slant with
      | `Italic -> Cairo.Italic
      | `Normal -> Cairo.Upright
      | `Oblique -> Cairo.Oblique in
      let weight = match font.Font.weight with
      | `W600 | `W700 | `W800 | `W900 -> Cairo.Bold
      | _ -> Cairo.Normal in
      Font (Cairo.Font_face.create ~family:font.Font.name slant weight)
    in
    Hashtbl.add s.fonts font cairo_font; cairo_font

let set_source s p =
  let p = get_primitive s p in
  if s.gstate.g_stroke != p then begin match p with
  | Pattern g -> Cairo.set_source s.ctx g
  end;
  p

let set_stroke s p = s.gstate.g_stroke <- set_source s p

let set_fill s p = s.gstate.g_fill <- set_source s p

let set_font s (font, size) =
  let Font f = get_font s font in
  Cairo.Font_face.set s.ctx f;
  Cairo.set_font_size s.ctx size
  (*Cairo.set_font_size s.ctx 25.0*)


let set_path s p =
  let rec loop last = function
  | [] -> ()
  | seg :: segs ->
      match seg with
      | `Sub pt -> P2.(Cairo.move_to s.ctx (x pt) (y pt)); loop pt segs
      | `Line pt -> P2.(Cairo.line_to s.ctx (x pt) (y pt)); loop pt segs
      | `Qcurve (q, pt) ->
          let x,y = Cairo.Path.get_current_point s.ctx in
          let p0 = V2.v x y in
          let c  = V2.((q + 2. * p0) / 3.) in
          let c' = V2.((pt + 2. * q) / 3.) in
          P2.(Cairo.curve_to s.ctx (x c) (y c) (x c') (y c') (x pt) (y pt));
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
              M2.(Cairo.transform s.ctx (cairo_matrix (e00 m) (e10 m)
                                                      (e01 m) (e11 m)
                                                      0.      0.));
              let arc = if cw then Cairo.arc_negative else Cairo.arc in
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

let rec r_cut_glyphs s a run i = match run.text with
| None -> warn s (`Textless_glyph_cut (image (Cut_glyphs (a, run, i))))
| Some text ->
    Cairo.save s.ctx;
    s.todo <- (save_gstate s) :: s.todo;
    let font_size = run.font.Font.size in
    set_font s (run.font, font_size);
    Cairo.Path.clear s.ctx;
    M3.(Cairo.transform s.ctx (cairo_matrix 1.0 0.0
                                            0.0 (-1.0)
                                            0.0 0.0));
    Cairo.move_to s.ctx 0. 0.;
    Cairo.Path.text s.ctx text;
    begin match a with
    | `O o ->
        set_outline s o;
        begin match i with
        | Primitive p ->
            set_stroke s p;
            Cairo.stroke s.ctx
        | _ ->
            warn s (`Unsupported_glyph_cut (a, image i))
        end
    | `Aeo | `Anz ->
        Cairo.clip s.ctx;
        M3.(Cairo.transform s.ctx (cairo_matrix 1.0 0.0
                                                0.0 (-1.0)
                                                0.0 0.0));
        s.todo <- Draw i :: s.todo
    end


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
      | Primitive _ as i ->           (* Uncut primitive, just cut to view. *)
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
          Cairo.save s.ctx;
          s.todo <- (Draw i) :: (save_gstate s) :: todo;
          push_transform s tr;
          r_image s k r

let vgr_output r str =
  let k' _ = `Ok in
  ignore (Vgr.Private.writes str 0 (String.length str) k' r)

let render s v k r = match v with
| `End ->
    if s.backend = `PNG then
      Cairo.PNG.write_to_stream s.surface (vgr_output r);
    Cairo.Surface.finish s.surface;
    Vgr.Private.flush k r
| `Image (size, view, i) ->
    let cw, ch = Size2.w s.size, Size2.h s.size in
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

let pre_render resolution backend =
  let s = ref None in
  fun v k r ->
    match !s, v with
    | Some s, _ -> render s v k r
    | None, `End -> assert false
    | None, `Image (size, view, i) ->
        let size = V2.(resolution * size) in
        let w, h = Size2.w size, Size2.h size in
        let surface = match backend with
          | `PNG ->
              Cairo.Image.(create ARGB32 (int_of_float w) (int_of_float h))
          | `PDF -> Cairo.PDF.create_for_stream (vgr_output r) w h
          | `PS -> Cairo.PS.create_for_stream (vgr_output r) w h
          | `SVG -> Cairo.SVG.create_for_stream  (vgr_output r) w h
        in
        let ctx = Cairo.create surface in
        Cairo.save ctx;
        let state =
          { r; surface; ctx; size;
            backend = (backend :> cairo_backend);
            cost = 0;
            view = Box2.empty;
            view_tr = M3.id;
            todo = [];
            fonts = Hashtbl.create 20;
            prims = Hashtbl.create 231;
            gstate = init_gstate; } in
        s := Some state;
        render state v k r

let target ?(resolution = 1.0) backend =
  let target _ _ = false, pre_render resolution backend in
  Vgr.Private.create_target target

let target_surface surface =
  let target r _ =
    let size = Size2.v (float (Cairo.Image.get_width surface))
                       (float (Cairo.Image.get_width surface)) in
    let ctx = Cairo.create surface in
    Cairo.save ctx;
    true, render { r; surface; ctx; backend = `Surface; size;
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
   Copyright 2014 Arthur Wendling, Daniel C. Bünzli.
   All rights reserved.

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions
   are met:

   1. Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

   2. Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

   3. Neither the name of Daniel C. Bünzli nor the names of
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
  ---------------------------------------------------------------------------*)
