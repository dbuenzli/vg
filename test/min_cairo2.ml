(* This code is in the public domain.

   Minimal Vgr_cairo2 example. Compile with:

   ocamlfind ocamlc \
    -package cairo2 \
    -package gg,vg,vg.cairo2 \
    -linkpkg -o min_cairo2.byte min_cairo2.ml
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
  let r = Vgr.create ~warn (Vgr_cairo2.target `PNG) (`Channel stdout) in
  ignore (Vgr.render r (`Image (size, view, image)));
  ignore (Vgr.render r `End)
