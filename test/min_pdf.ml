(*---------------------------------------------------------------------------
   Copyright (c) 2024 The vg programmers. All rights reserved.
   SPDX-License-Identifier: CC0-1.0
  ---------------------------------------------------------------------------*)

(* Minimal Vgr_pdf example. Compile with:
   ocamlfind ocamlopt -package gg,vg,vg.pdf min_pdf.ml *)

open Gg
open Vg

(* 1. Define your image *)

let aspect = 1.618
let size = Size2.v (aspect *. 100.) 100. (* mm *)
let view = Box2.v P2.o (Size2.v aspect 1.)
let image = I.const (Color.v_srgb 0.314 0.784 0.471)

(* 2. Render *)

let render oc =
  let title = "Vgr_pdf minimal example" in
  let description = "Emerald Color" in
  let xmp = Vgr.xmp ~title ~description () in
  let warn w = Vgr.pp_warning Format.err_formatter w in
  let r = Vgr.create ~warn (Vgr_pdf.target ~xmp ()) (`Channel oc) in
  ignore (Vgr.render r (`Image (size, view, image)));
  ignore (Vgr.render r `End)

(* 3. Main *)

let main () = Out_channel.set_binary_mode stdout true; render stdout; 0
let () = if !Sys.interactive then () else exit (main ())
