(* This code is in the public domain.

   Minimal Vgr_cairo PNG rendering example. Compile with:

   ocamlfind ocamlopt \
    -package gg,vg,vg.cairo \
    -linkpkg -o min_cairo_png.native min_cairo_png.ml
*)

open Gg
open Vg

(* 1. Define your image *)

let aspect = 1.618
let size = Size2.v (aspect *. 100.) 100. (* mm *)
let view = Box2.v P2.o (Size2.v aspect 1.)
let image = I.const (Color.v_srgb 0.314 0.784 0.471)

(* 2. Render *)

let () =
  let res = 300. /. 0.0254 (* 300dpi in dots per meters *) in
  let fmt = `Png (Size2.v res res) in
  let warn w = Vgr.pp_warning Format.err_formatter w in
  let r = Vgr.create ~warn (Vgr_cairo.stored_target fmt) (`Channel stdout) in
  ignore (Vgr.render r (`Image (size, view, image)));
  ignore (Vgr.render r `End)
