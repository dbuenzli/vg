(* This code is in the public domain.

   Minimal Vgr_cairo examples. Compile with:

   ocamlfind ocamlc \
    -package cairo2 \
    -package gg,vg,vg.cairo \
    -linkpkg -o min_cairo.byte min_cairo.ml
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
  let warn w = Vgr.pp_warning Format.err_formatter w in
  let r = Vgr.create ~warn (Vgr_cairo.target `PS) (`Channel stdout) in
  ignore (Vgr.render r (`Image (size, view, image)));
  ignore (Vgr.render r `End)

(* 3. Render with a manually created surface *)

let () =
  let surface = Cairo.Image.(create ARGB32) 400 400 in
  let warn w = Vgr.pp_warning Format.err_formatter w in
  let r = Vgr.create ~warn (Vgr_cairo.target_surface surface) `Other in
  ignore (Vgr.render r (`Image (size, view, image)));
  ignore (Vgr.render r `End)
