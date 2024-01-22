(*---------------------------------------------------------------------------
   Copyright (c) 2024 The vg programmers. All rights reserved.
   SPDX-License-Identifier: CC0-1.0
  ---------------------------------------------------------------------------*)

(* Minimal Vgr_htmlc example. Compile with:
   ocamlfind ocamlc -package brr,gg,vg,vg.htmlc -linkpkg min_htmlc.ml
   js_of_ocaml -o min_htmlc.js a.out *)

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
  let cnv = Brr_canvas.Canvas.create [] (* 2 *) in
  let anchor = (* 3 *)
    let href = At.href (Jstr.v "#") in
    let title = At.title (Jstr.v "Download PNG file") in
    let download = At.v (Jstr.v "download") (Jstr.v "min_htmlc.png") in
    let a = El.a ~at:[href; title; download] [Brr_canvas.Canvas.to_el cnv] in
    El.append_children (Document.body G.document) [a]; a
  in
  let r = Vgr.create (Vgr_htmlc.target cnv) `Other in  (* 4 *)
  ignore (Vgr.render r (`Image (size, view, image))); (* 5 *)
  ignore (Vgr.render r `End);
  let data = (* 6 *)
    Canvas.to_data_url cnv |> Console.log_if_error ~use:Jstr.empty
  in
  El.set_at At.Name.href (Some data) anchor

let () = main () (* 7 *)
