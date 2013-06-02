(*---------------------------------------------------------------------------
   Copyright 2013 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

open Gg
open Vg
open Vgr.Private.Data

let str = Format.sprintf 
let pp = Format.fprintf 

let warn_dash = "Outline dashes unsupported in this browser"

(* JS bindings, those are not in js_of_ocaml *)

class type ctx_ext = object 
  method lineDashOffset : Js.float_prop
  method setLineDash : float Js.js_array Js.t -> unit Js.meth
  method fill : Js.js_string Js.t -> unit Js.meth
  method clip : Js.js_string Js.t -> unit Js.meth
end

let dash_support ctx = Js.Optdef.test ((Js.Unsafe.coerce ctx) ## setLineDash)

(* Renderer *)

type js_font = Js.js_string Js.t                       (* lovely isn't it ? *) 
type js_primitive = 
| Color of Js.js_string Js.t                                  (* even more. *)
| Gradient of Dom_html.canvasGradient Js.t          (* Oh! a datastructure. *)

let dumb_prim = Color (Js.string "")

type gstate =    (* Subset of the graphics state saved by a ctx ## save (). *)
  { g_alpha : float;                                     (* unused for now. *)
    g_blender : Vgr.Private.Data.blender;                (* unused for now. *)
    g_outline : P.outline; 
    g_stroke : js_primitive; 
    g_fill : js_primitive;
    g_tr : M3.t }

type cmd = Pop of gstate | Draw of Vgr.Private.Data.image
type state = 
  { r : Vgr.Private.renderer;                    (* corresponding renderer. *)
    c : Dom_html.canvasElement Js.t;         (* canvas element rendered to. *)
    ctx : Dom_html.canvasRenderingContext2D Js.t; (* canvas context of [c]. *)
    dash_support : bool;               (* [true] if [ctx] has dash support. *)
    resolution : Gg.v2;                        (* resolution of the canvas. *)
    mutable cost : int;                          (* cost counter for limit. *)
    mutable view : Gg.box2;           (* current renderable view rectangle. *)
    mutable view_tr : M3.t;                    (* view to canvas transform. *)
    mutable todo : cmd list;                        (* commands to perform. *)
    fonts :                                                (* cached fonts. *)
      (Vgr.Private.Data.font * float, js_font) Hashtbl.t;     
    prims :                                           (* cached primitives. *)
      (Vgr.Private.Data.primitive, js_primitive) Hashtbl.t;     
    mutable s_tr : M3.t;         (* current transformation without view_tr. *)
    mutable s_alpha : float;                       (* current global alpha. *)
    mutable s_blender : Vgr.Private.Data.blender; (* current blending mode. *)
    mutable s_outline : P.outline;         (* current outline stroke state. *)
    mutable s_stroke : js_primitive;               (* current stroke color. *)
    mutable s_fill : js_primitive; }                 (* current fill color. *)

let partial = Vgr.Private.partial
let limit s = Vgr.Private.limit s.r
let warn s w = Vgr.Private.warn s.r w
let image i = Vgr.Private.I.of_data i
let pop_gstate s = 
  Pop { g_alpha = s.s_alpha; g_blender = s.s_blender; g_outline = s.s_outline; 
        g_stroke = s.s_stroke; g_fill = s.s_fill; g_tr = s.s_tr }

let set_gstate s g = 
  s.s_alpha <- g.g_alpha; s.s_blender <- g.g_blender; 
  s.s_outline <- g.g_outline; s.s_stroke <- g.g_stroke; 
  s.s_fill <- g.g_fill; s.s_tr <- g.g_tr

let uncut_bounds s =
  Vgr.Private.Data.of_path (P.empty >> P.rect (Box2.tr (M3.inv s.s_tr) s.view))
  
let css_color c =                               (* w3c bureaucrats are pigs. *)
  let srgba = Color.to_srgba c in
  let r = Float.int_of_round (Color.r srgba *. 255.) in 
  let g = Float.int_of_round (Color.g srgba *. 255.) in 
  let b = Float.int_of_round (Color.b srgba *. 255.) in
  let a = Color.a srgba in
  if a = 1.0 then Js.string (str "rgb(%d,%d,%d)" r g b) else
  Js.string (str "rgba(%d,%d,%d,%g)" r g b a)

let cap_str = 
  let butt = Js.string "butt" in
  let round = Js.string "round" in
  let square = Js.string "square" in
  function `Butt -> butt | `Round -> round | `Square -> square

let join_str = 
  let bevel = Js.string "bevel" in
  let round = Js.string "round" in
  let miter = Js.string "miter" in
  function `Bevel -> bevel | `Round -> round | `Miter -> miter 

let area_str = 
  let nz = Js.string "nonzero" in 
  let eo = Js.string "evenodd" in 
  function `Anz -> nz | `Aeo -> eo | `O _ -> assert false

let get_primitive s p = try Hashtbl.find s.prims p with 
| Not_found -> 
    let add_stop g (t, c) = g ## addColorStop (t, css_color c) in 
    let create = function 
    | Const c -> Color (css_color c)
    | Axial (stops, pt, pt') ->
        let g = P2.(s.ctx ## createLinearGradient(x pt, y pt, x pt', y pt')) in
        List.iter (add_stop g) stops; Gradient g
    | Radial (stops, f, c, r) ->
        let g = P2.(s.ctx ## createRadialGradient(x f, y f, 0., x c, y c, r)) in
        List.iter (add_stop g) stops; Gradient g
    | Raster _ -> assert false
    in
    let js_prim = create p in 
    Hashtbl.add s.prims p js_prim; js_prim

let get_font s (font, size as spec) = try Hashtbl.find s.fonts spec with
| Not_found -> 
    let js_font =
      let slant = match font.slant with 
      | `Italic -> "italic" | `Normal -> "normal" | `Oblique -> "oblique" 
      in
      let weight = match font.weight with 
      | `W100 -> "100" | `W200 -> "200" | `W300 -> "300" | `W400 -> "400" 
      | `W500 -> "500" | `W600 -> "600" | `W700 -> "700" | `W800 -> "800"
      | `W900 -> "900"
      in
      let font = str "%s %s %gpx \"%s\"" slant weight size font.name in 
      Mui.Log.msg "%s" font;
      Js.string font
    in
    Hashtbl.add s.fonts spec js_font; js_font

let set_dashes ?(warning = true) s dashes = 
  if not s.dash_support then (if warning then warn s (`Other warn_dash)) else
  let ctx : ctx_ext Js.t = Js.Unsafe.coerce s.ctx in 
  match dashes with 
  | None -> ctx ## setLineDash(jsnew Js.array_empty ())
  | Some (offset, dashes) ->  
      let da = jsnew Js.array_empty () in 
      List.iteri (fun i v -> Js.array_set da i v) dashes; 
      ctx ## lineDashOffset <- offset;
      ctx ## setLineDash(da)

let init_ctx s =
  let o = s.s_outline in
  let m = s.view_tr in 
  M3.(s.ctx ## transform (e00 m, e10 m, e01 m, e11 m, e02 m, e12 m));
  s.ctx ## lineWidth <- o.P.width; 
  s.ctx ## lineCap <- cap_str o.P.cap; 
  s.ctx ## lineJoin <- join_str o.P.join; 
  s.ctx ## miterLimit <- (Vgr.Private.P.miter_limit o);
  set_dashes ~warning:false s o.P.dashes

let push_transform s = function 
| Move v -> 
    s.ctx ## translate (V2.x v, V2.y v); 
    s.s_tr <- M3.mul s.s_tr (M3.move2 v)
| Rot a -> 
    s.ctx ## rotate (a); 
    s.s_tr <- M3.mul s.s_tr (M3.rot2 a)
| Scale sv -> 
    s.ctx ## scale (V2.x sv, V2.y sv); 
    s.s_tr <- M3.mul s.s_tr (M3.scale2 sv)
| Matrix m -> 
    M3.(s.ctx ## transform (e00 m, e10 m, e01 m, e11 m, e02 m, e12 m));
    s.s_tr <- M3.mul s.s_tr m

let set_outline s o =       
  if s.s_outline == o then () else
  let old = s.s_outline in  
  s.s_outline <- o;
  if old.P.width <> o.P.width then (s.ctx ## lineWidth <- o.P.width);
  if old.P.cap <> o.P.cap then (s.ctx ## lineCap <- cap_str o.P.cap);
  if old.P.join <> o.P.join then (s.ctx ## lineJoin <- join_str o.P.join);
  if old.P.miter_angle <> o.P.miter_angle then 
    (s.ctx ## miterLimit <- Vgr.Private.P.miter_limit o);
  if old.P.dashes <> o.P.dashes then set_dashes s o.P.dashes; 
  ()

let set_stroke s p = 
  let p = get_primitive s p in 
  if s.s_stroke != p then begin match p with 
  | Color c -> s.ctx ## strokeStyle <- c; s.s_stroke <- p
  | Gradient g -> s.ctx ## strokeStyle_gradient <- g; s.s_stroke <- p
  end

let set_fill s p = 
  let p = get_primitive s p in 
  if (s.s_fill != p) then begin match p with 
  | Color c -> s.ctx ## fillStyle <- c; s.s_fill <- p
  | Gradient g -> s.ctx ## fillStyle_gradient <- g; s.s_fill <- p
  end

let set_font s f = 
  let f = get_font s f in 
  s.ctx ## font <- f

(* N.B. In the future, if browsers begin supporting them we could in r_path,
   1) construct and cache path objects 2) use ctx ## ellipse. *)

let set_path s p =
  let rec loop last = function
  | [] -> ()
  | seg :: segs -> 
      match seg with
      | `Sub pt -> P2.(s.ctx ## moveTo (x pt, y pt)); loop pt segs
      | `Line pt -> P2.(s.ctx ## lineTo (x pt, y pt)); loop pt segs
      | `Qcurve (c, pt) -> 
          P2.(s.ctx ## quadraticCurveTo (x c, y c, x pt, y pt)); 
          loop pt segs
      | `Ccurve (c, c', pt) ->
          P2.(s.ctx ## bezierCurveTo (x c, y c, x c', y c', x pt, y pt));
          loop pt segs
      | `Earc (large, cw, r, a, pt) ->
          (* TODO if r.x = r.y optimize. *)
          begin match Vgr.Private.P.earc_params last large cw r a pt with 
          | None -> P2.(s.ctx ## lineTo (x pt, y pt)); loop pt segs 
          | Some (c, m, a, a') -> 
              s.ctx ## save ();
              let c = V2.ltr (M2.inv m) c in  (* TODO avoid that *)
              M2.(s.ctx ## transform (e00 m, e10 m, e01 m, e11 m, 0., 0.));
              P2.(s.ctx ## arc (x c, y c, 1.0, a, a', (Js.bool cw))); 
              s.ctx ## restore ();
              loop pt segs
          end          
      | `Close -> s.ctx ## closePath (); loop last (* we don't care *) segs
  in
  s.ctx ## beginPath ();
  loop P2.o (List.rev p)

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
| Cut_glyphs _ as i ->
    warn s (`Unsupported_glyph_cut (a, image i))
| Primitive p as i ->
    begin match run.text with 
    | None -> warn s (`Other "No text specified in glyph cut")
    | Some text -> 
        let text = Js.string text in
        s.ctx ## save ();
        s.todo <- (pop_gstate s) :: s.todo;
        let m = M3.mul s.view_tr s.s_tr in
        let o = P2.tr m run.o in
        let font_size = V2.norm (V2.tr m (V2.v 0. run.font.size)) in
        let y_scale = 1. /. V2.norm (V2.tr s.s_tr V2.oy) in
        let x_scale =
          (* we don't apply the view transform but still need to scale
             for aspect ratio distortions. *)
          let sa = (float s.c ## width) /. (float s.c ## height) in 
          let va = Box2.w s.view /. Box2.h s.view in 
          sa /. va
        in
        set_font s (run.font, font_size);
        M3.(s.ctx ## setTransform 
              (   e00 s.s_tr *. x_scale, -. e10 s.s_tr *. x_scale,
               -. e01 s.s_tr *. y_scale,    e11 s.s_tr *. y_scale, 
                  V2.x o,                   V2.y o));
        begin match a with 
        | `O o -> 
            warn s (`Unsupported_glyph_cut (a, image i))
            (* some work is needed here as the outline params won't 
               be in the right coordinate system *)
            (* set_outline s o; set_stroke s p;
               s.ctx ## strokeText (text, 0., 0.) *)
        | `Aeo | `Anz ->
            set_fill s p; s.ctx ## fillText (text, 0., 0.)
        end;
    end
| Meta (_, i) -> r_cut_glyphs s a run i 

let rec r_cut s a = function 
| Primitive (Raster _) as i -> 
    begin match a with 
    | `O _ -> warn s (`Unsupported_cut (a, image i))
    | `Aeo | `Anz -> 
        s.ctx ## save ();
        (Js.Unsafe.coerce s.ctx : ctx_ext Js.t) ## clip (area_str a); 
        warn s (`Other "TODO raster unimplemented");
        s.ctx ## restore ()
    end
| Primitive p ->
    begin match a with 
    | `O o -> set_outline s o; set_stroke s p; s.ctx ## stroke ()
    | `Aeo | `Anz -> 
        set_fill s p; 
        (Js.Unsafe.coerce s.ctx : ctx_ext Js.t) ## fill (area_str a)
    end
| Tr (tr, i) ->
    s.ctx ## save ();
    s.todo <- (pop_gstate s) :: s.todo;
    push_transform s tr;
    r_cut s a i
| Blend _ | Cut _ | Cut_glyphs _ as i ->
    let a = match a with
    | `O _ -> warn s (`Unsupported_cut (a, image i)); `Anz
    | a -> a
    in
    s.ctx ## save ();
    (Js.Unsafe.coerce s.ctx : ctx_ext Js.t) ## clip (area_str a);
    s.todo <- (Draw i) :: (pop_gstate s) :: s.todo
| Meta (_, i) -> r_cut s a i

let rec r_image s k r =
  if s.cost > limit s then (s.cost <- 0; partial (r_image s k) r) else 
  match s.todo with
  | [] -> Hashtbl.reset s.prims; Hashtbl.reset s.fonts; k r
  | Pop gs :: todo -> 
      s.ctx ## restore ();
      set_gstate s gs;
      s.todo <- todo;
      r_image s k r
  | (Draw i) :: todo ->
      s.cost <- s.cost + 1;
      match i with
      | Primitive _ as i ->            (* Uncut primitive, just cut to view. *)
          let p = uncut_bounds s in
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
          s.ctx ## save ();
          s.todo <- (Draw i) :: (pop_gstate s) :: todo; 
          push_transform s tr; 
          r_image s k r;
      | Meta (m, i) -> 
          s.todo <- (Draw i) :: todo;
          r_image s k r

let render s v k r = match v with 
| `End -> k r
| `Image (size, view, i) -> 
    let to_css_mm = str "%gmm" in
    s.c ## style ## width <- Js.string (to_css_mm (Size2.w size)); 
    s.c ## style ## height <- Js.string (to_css_mm (Size2.h size));
    let cw = (Size2.w size /. 1000.) *. (V2.x s.resolution) in
    let ch = (Size2.h size /. 1000.) *. (V2.y s.resolution) in
    s.c ## width <- Float.int_of_round cw; 
    s.c ## height <- Float.int_of_round ch;
    (* Map view rect (bot-left coords) to canvas (top-left coords) *)
    let sx = cw /. Box2.w view in 
    let sy = ch /. Box2.h view in
    let dx = -. Box2.ox view *. sx in
    let dy = ch +. Box2.oy view *. sy in 
    let view_tr = M3.v sx      0. dx 
                      0. (-. sy)  dy
                      0.       0. 1.
    in
    s.cost <- 0;
    s.view <- view; 
    s.view_tr <- view_tr;
    s.todo <- [ Draw i ];
    s.s_tr <- M3.id; 
    s.s_blender <- `Over; 
    s.s_alpha <- 1.0; 
    s.s_outline <- P.o; 
    s.s_stroke <- dumb_prim; 
    s.s_fill <- dumb_prim;
    init_ctx s;
    r_image s k r

let ppi_300 = V2.v 11811. 11811.             (* 300 ppi in pixel per meters. *)
let target c =
  let target r _ = 
    let meta = Vgr.Private.meta r in
    let resolution = Vgm.get ~absent:ppi_300 meta Vgm.resolution in
    let ctx = c ## getContext (Dom_html._2d_) in
    true, render { r; c; ctx; 
                   dash_support = dash_support ctx; 
                   resolution; 
                   cost = 0; 
                   view = Box2.empty; 
                   view_tr = M3.id;
                   todo = [];
                   fonts = Hashtbl.create 20; 
                   prims = Hashtbl.create 231;
                   s_tr = M3.id;
                   s_blender = `Over;
                   s_alpha = 1.0; 
                   s_outline = P.o; 
                   s_stroke = dumb_prim; 
                   s_fill = dumb_prim; }
  in
  Vgr.Private.create_target target

(*---------------------------------------------------------------------------
   Copyright 2013 Daniel C. Bünzli.
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
