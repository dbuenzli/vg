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

(* JS bindings, those are not in js_of_ocaml *)

class type ctx_dashes = object 
  method lineDashOffset : Js.float_prop
  method setLineDash : float Js.js_array Js.t -> unit Js.meth
end

(* Renderer *)

type js_primitive = 
  | Color of Js.js_string Js.t 
  | Grad of Dom_html.canvasGradient Js.t

let dumb_prim = Color (Js.string "")

type gstate =    (* Subset of the graphics state saved by a ctx ## save (). *)
  { g_alpha : float;
    g_blender : I.blender;
    g_outline : P.outline; 
    g_stroke : js_primitive; 
    g_fill : js_primitive; }

type cmd = Pop of gstate | Draw of Vgr.Private.Data.image
type state = 
  { r : Vgr.Private.renderer;
    c : Dom_html.canvasElement Js.t;     (* The canvas element rendered to. *)
    ctx : Dom_html.canvasRenderingContext2D Js.t;    (* The canvas context. *)
    resolution : Gg.v2;                        (* Resolution of the canvas. *)
    timeout : float;   
    mutable cost : int;                        (* cost counter for timeout. *)
    mutable view : Gg.box2; 
    mutable todo : cmd list;                        (* commands to perform. *)
    (* Cached primitives. *)
    prims : (Vgr.Private.Data.primitive, js_primitive) Hashtbl.t;     
    (* Current graphics state. *)
    mutable s_alpha : float;
    mutable s_blender : I.blender; 
    mutable s_outline : P.outline; 
    mutable s_stroke : js_primitive; 
    mutable s_fill : js_primitive; }

let max_cost = max_int
let warn s w i = Vgr.Private.warn s.r w i

let save_gstate s = 
  Pop { g_alpha = s.s_alpha; g_blender = s.s_blender; g_outline = s.s_outline; 
        g_stroke = s.s_stroke; g_fill = s.s_fill; }

let set_gstate s g = 
  s.s_alpha <- g.g_alpha; s.s_blender <- g.g_blender; 
  s.s_outline <- g.g_outline; s.s_stroke <- g.g_stroke; 
  s.s_fill <- g.g_fill

let css_color c =                               (* w3c bureaucrats are pigs. *)
  let r = Float.int_of_round (Color.r c *. 255.) in 
  let g = Float.int_of_round (Color.g c *. 255.) in 
  let b = Float.int_of_round (Color.b c *. 255.) in
  let a = Color.a c in
  if a = 1.0 then Js.string (str "rgb(%d,%d,%d)" r g b) else
  Js.string (str "rgba(%d,%d,%d,%f)" r g b a)

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

let set_dashes ?(warning = true) s dashes = 
  if not (Js.Optdef.test ((Js.Unsafe.coerce s.ctx) ## setLineDash)) then 
    (if not warning then () else
     warn s (`Other "Outline dashes unsupported in this browser") None)
  else
  let ctx : ctx_dashes Js.t = Js.Unsafe.coerce s.ctx in 
  match dashes with 
  | None -> ctx ## setLineDash(jsnew Js.array_empty ())
  | Some (offset, dashes) ->  
      let da = jsnew Js.array_empty () in 
      List.iteri (fun i v -> Js.array_set da i v) dashes; 
      ctx ## lineDashOffset <- offset;
      ctx ## setLineDash(da); 
      ()

let init_ctx s = 
  s.ctx ## globalAlpha <- s.s_alpha;
(*  s.ctx ## globalCompositeOperation <-  TODO *)
  let o = s.s_outline in
  s.ctx ## lineWidth <- o.P.width; 
  s.ctx ## lineCap <- cap_str o.P.cap; 
  s.ctx ## lineJoin <- join_str o.P.join; 
  s.ctx ## miterLimit <- o.P.miter_angle;
  set_dashes ~warning:false s o.P.dashes

let r_outline s o =       
  if s.s_outline == o then () else
  let old = s.s_outline in  
  s.s_outline <- o;
  if old.P.width <> o.P.width then (s.ctx ## lineWidth <- o.P.width);
  if old.P.cap <> o.P.cap then (s.ctx ## lineCap <- cap_str o.P.cap);
  if old.P.join <> o.P.join then (s.ctx ## lineJoin <- join_str o.P.join);
  if old.P.miter_angle <> o.P.miter_angle then 
    (s.ctx ## miterLimit <- o.P.miter_angle);
  if old.P.dashes <> o.P.dashes then set_dashes s o.P.dashes; 
  ()

let r_transform s = function 
| Move v -> s.ctx ## translate (V2.x v, V2.y v)
| Rot a -> s.ctx ## rotate (a)
| Scale sv -> s.ctx ## scale (V2.x sv, V2.y sv)
| Matrix m -> M3.(s.ctx ## transform (e00 m, e10 m, e01 m, e11 m, e02 m, e12 m))
  
(* N.B. In the future, if browsers begin supporting them we could in r_path,
   1) construct and cache path objects 2) use ctx ## ellipse. *)

let r_path s p =
  let ctx = s.ctx in
  let rec loop last = function
  | [] -> ()
  | seg :: segs -> 
      match seg with
      | `Sub pt -> P2.(ctx ## moveTo (x pt, y pt)); loop pt segs
      | `Line pt -> P2.(ctx ## lineTo (x pt, y pt)); loop pt segs
      | `Qcurve (c, pt) -> 
          P2.(ctx ## quadraticCurveTo (x c, y c, x pt, y pt)); 
          loop pt segs
      | `Ccurve (c, c', pt) ->
          P2.(ctx ## bezierCurveTo (x c, y c, x c', y c', x pt, y pt));
          loop pt segs
      | `Earc (large, cw, r, a, pt) ->
          (* TODO if r.x = r.y optimize. *)
          begin match Vgr.Private.P.earc_params last large cw r a pt with 
          | None -> P2.(ctx ## lineTo (x pt, y pt)); loop pt segs 
          | Some (c, m, a, a') -> 
              ctx ## save ();
              let c = V2.ltr (M2.inv m) c in  (* TODO avoid that *)
              M2.(ctx ## transform (e00 m, e10 m, e01 m, e11 m, 0., 0.));
              P2.(ctx ## arc (x c, y c, 1.0, a, a', (Js.bool cw))); 
              s.ctx ## restore ();
              loop pt segs
          end          
      | `Close -> ctx ## closePath (); loop  last (* we don't care *) segs
  in
  ctx ## beginPath ();
  loop P2.o (List.rev p)

let r_primitive s p = 
  let add_stop g (t, c) = g ## addColorStop (t, css_color c) in 
  let create = function 
  | Const c -> Color (css_color c)
  | Axial (stops, pt, pt') ->
      let g = P2.(s.ctx ## createLinearGradient (x pt, y pt, x pt', y pt')) in
      List.iter (add_stop g) stops; Grad g
  | Radial (stops, f, c, r) ->
      let g = P2.(s.ctx ## createRadialGradient (x c, y c, 0., x f, y f, r)) in
      List.iter (add_stop g) stops; Grad g
  | Raster _ -> assert false
  in
  try Hashtbl.find s.prims p with 
  | Not_found ->
      let js_prim = create p in 
      Hashtbl.add s.prims p js_prim; js_prim
    
let rec r_cut s a = match s.todo with 
| [] | Pop _ :: _ -> assert false 
| (Draw i) :: todo -> 
    match i with 
    | Primitive (Raster _) -> 
        begin match a with 
        | `O _ -> warn s (`Unsupported_cut a) (Some i); s.todo <- todo;
        | `Aeo | `Anz -> 
            if a = `Aeo then warn s (`Unsupported_cut a) (Some i) else
            s.ctx ## save (); 
            s.ctx ## clip ();
            warn s (`Other "TODO raster unimplemented") (Some i);
            (* TODO s.ctx drawDraw_full raster *)
            s.ctx ## restore ();
            s.todo <- todo;
        end
    | Primitive p ->
        let p = r_primitive s p in
        begin match a with 
        | `O o -> 
            if s.s_stroke != p then begin match p with 
            | Color c -> s.ctx ## strokeStyle <- c; s.s_stroke <- p
            | Grad g -> s.ctx ## strokeStyle_gradient <- g; s.s_stroke <- p
            end;
            r_outline s o;
            s.ctx ## stroke ();
            s.todo <- todo
        | `Aeo | `Anz ->
            if a = `Aeo then warn s (`Unsupported_cut a) (Some i); 
            if s.s_fill != p then begin match p with 
            | Color c -> s.ctx ## fillStyle <- c; s.s_fill <- p
            | Grad g -> s.ctx ## fillStyle_gradient <- g; s.s_fill <- p
            end;
            s.ctx ## fill ();
            s.todo <- todo
        end
    | Meta _ -> s.todo <- todo; r_cut s a
    | Tr (tr, i) -> 
        s.ctx ## save ();
        s.todo <- (Draw i) :: (save_gstate s) :: todo; 
        r_transform s tr;
        r_cut s a;
    | i -> 
        begin match a with
        | `O _ | `Aeo -> warn s (`Unsupported_cut a) (Some i);
        | `Anz -> () 
        end;
        s.ctx ## save ();
        s.ctx ## clip ();
        s.todo <- (Draw i) :: (save_gstate s) :: todo

and r_image s k r =
  if s.cost > max_cost then Vgr.Private.partial (r_image s k) r else
  match s.todo with
  | [] -> Hashtbl.reset s.prims; k r
  | Pop gs :: todo -> 
      s.ctx ## restore ();
      set_gstate s gs;
      s.todo <- todo;
      r_image s k r
  | (Draw i) :: todo ->
      s.cost <- s.cost + 1;
      match i with
      | Primitive p -> 
          (* Uncut primitive, just cut to view. *)
          (* s.todo <- (I (Cut (`Anz, P.empty >> P.rect s.view, i))) :: todo; *)
          warn s (`Other "TODO, uncut primitive not implemented") (Some i);
          s.todo <- todo;
          r_image s k r
      | Cut (a, p, i) -> 
          s.todo <- (Draw i) :: todo;
          r_path s p;
          r_cut s a;
          r_image s k r
      | Blend (blender, alpha, i, i') -> 
          (* TODO blender and alpha *)
          s.todo <- (Draw i') :: (Draw i) :: todo;
          r_image s k r
      | Tr (tr, i) ->
          s.ctx ## save ();
          s.todo <- (Draw i) :: (save_gstate s) :: todo; 
          r_transform s tr; 
          r_image s k r;
      | Meta (m, i) -> 
          s.todo <- (Draw i) :: todo;
          r_image s k r

let render s v k r = match v with 
| `End -> k r
| `Image (size, view, i) -> 
    let to_css_mm = str "%fmm" in
    s.c ## style ## width <- Js.string (to_css_mm (Size2.w size)); 
    s.c ## style ## height <- Js.string (to_css_mm (Size2.h size));
    let cw = (Size2.w size /. 1000.) *. (V2.x s.resolution) in
    let ch = (Size2.h size /. 1000.) *. (V2.y s.resolution) in
    s.c ## width <- Float.int_of_round cw; 
    s.c ## height <- Float.int_of_round ch;
    let sx = cw /. Box2.w view in 
    let sy = ch /. Box2.h view in
    let dx = -. Box2.ox view *. sx in
    let dy = ch +. Box2.oy view *. sy in 
    s.ctx ## setTransform (sx, 0., 0., -.sy, dx, dy); (* view rect -> canvas *)
    init_ctx s;
    s.cost <- 0;
    s.view <- view; 
    s.todo <- [ Draw i ];
    r_image s k r

let alloc_state c m r = 
  let resolution = match Vgm.find m Vgm.resolution with 
  | Some r -> r | None -> V2.v 11811. 11811. (* 300 dpi *)
  in
  let ctx = c ## getContext (Dom_html._2d_) in
  { r; c; ctx; resolution; 
    timeout = 0.005; cost = 0; 
    view = Box2.empty; todo = [];
    prims = Hashtbl.create 231;
    s_blender = `Over;
    s_alpha = 1.0; 
    s_outline = P.o; 
    s_stroke = dumb_prim; 
    s_fill = dumb_prim; } 

let renderer ?warn ?(meta = Vgm.empty) c = 
  Vgr.Private.create_renderer ?warn meta `Immediate (alloc_state c) render

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
