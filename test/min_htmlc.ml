(* This code is in the public domain.

   Minimal Vgr_htmlc example. Compile with:

   ocamlfind ocamlc \
    -package brr -package gg,vg,vg.htmlc \
    -linkpkg -o min_htmlc.byte min_htmlc.ml \
   && js_of_ocaml min_htmlc.byte
*)

open Gg
open Vg
open Brr
open Brr_canvas

(* 1. Define your image *)

let aspect = 1.618
let size = Size2.v (aspect *. 100.) 100. (* mm *)
let view = Box2.v P2.o (Size2.v aspect 1.)
let image = I.const (Color.v_srgb 0.314 0.784 0.471)

(* Browser bureaucracy. *)

let main () =
  let a = (* 2 *)
    let href = At.href (Jstr.v "#") in
    let title = At.title (Jstr.v "Download PNG file") in
    let download = At.v (Jstr.v "download") (Jstr.v "min_htmlc.png") in
    let a = El.a ~at:[href; title; download] [] in
    El.append_children (Document.body G.document) [a]; a
  in
  let c = (* 3 *)
    let c = Brr_canvas.Canvas.create [] in
    El.set_children a [Brr_canvas.Canvas.to_el c]; c
  in
  let r = Vgr.create (Vgr_htmlc.target c) `Other in   (* 4 *)
  ignore (Vgr.render r (`Image (size, view, image))); (* 5 *)
  ignore (Vgr.render r `End);
  let data = Canvas.to_data_url c |> Console.log_if_error ~use:Jstr.empty in
  El.set_at At.Name.href (Some data) a

let () = main ()
