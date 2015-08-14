(* This code is in the public domain.

   Minimal Vgr_cairo memory buffer example. Compile with:

   ocamlfind ocamlopt -package cairo2,gg,vg,vg.cairo \
    -linkpkg -o min_cairo_mem.native min_cairo_mem.ml
*)

open Gg
open Vg

(* 1. Define your image *)

let aspect = 1.618
let size = Size2.v (aspect *. 100.) 100. (* mm *)
let view = Box2.v P2.o (Size2.v aspect 1.)
let image = I.const (Color.v_srgb 0.314 0.784 0.471)

(* 2. Render *)

let raster, stride =
  let res = 300. /. 25.4 (* 300dpi in dots per mm *) in
  let w = int_of_float (res *. Size2.w size) in
  let h = int_of_float (res *. Size2.h size) in
  let stride = Cairo.Image.(stride_for_width ARGB32 w) in
  let data = Bigarray.(Array1.create int8_unsigned c_layout (stride * h)) in
  let surface = Cairo.Image.(create_for_data8 data ARGB32 ~stride w h) in
  let ctx = Cairo.create surface in
  Cairo.scale ctx ~x:res ~y:res;
  let target = Vgr_cairo.target ctx in
  let warn w = Vgr.pp_warning Format.err_formatter w in
  let r = Vgr.create ~warn target `Other in
  ignore (Vgr.render r (`Image (size, view, image)));
  ignore (Vgr.render r `End);
  Cairo.Surface.flush surface;
  Cairo.Surface.finish surface;
  data, stride
