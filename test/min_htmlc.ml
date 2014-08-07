(* This code is in the public domain.

   Minimal Vgr_htmlc example. Compile with:

   ocamlfind ocamlc \
    -package js_of_ocaml,js_of_ocaml.syntax \
    -package gg,vg,vg.htmlc \
    -syntax camlp4o -linkpkg -o min_htmlc.byte min_htmlc.ml \
   && js_of_ocaml min_htmlc.byte
*)

open Gg
open Vg

(* 1. Define your image *)

let aspect = 1.618
let size = Size2.v (aspect *. 100.) 100. (* mm *)
let view = Box2.v P2.o (Size2.v aspect 1.)
let image = I.const (Color.v_srgb 0.314 0.784 0.471)

(* Browser bureaucracy. *)

let main _ =
  let d = Dom_html.window ## document in
  let a = (* 2 *)
    let a = Dom_html.createA d in
    a ## title <- Js.string "Download PNG file";
    a ## href <- Js.string "#";
    a ## setAttribute (Js.string "download", Js.string "min_htmlc.png");
    Dom.appendChild (d ## body) a; a
  in
  let c = (* 3 *)
    let c = Dom_html.createCanvas d in
    Dom.appendChild a c; c
  in
  let r = Vgr.create (Vgr_htmlc.target c) `Other in   (* 4 *)
  ignore (Vgr.render r (`Image (size, view, image))); (* 5 *)
  ignore (Vgr.render r `End);
  a ## href <- (c ## toDataURL ()); (* 6 *)
  Js._false

let () = Dom_html.window ## onload <- Dom_html.handler main
