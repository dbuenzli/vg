(* This code is in the public domain. *)

open Gg
open Vg

(* Define your image *)

let aspect = 1.618  
let size = Size2.v (aspect *. 100.) 100. (* mm *)
let view = Box2.v P2.o (Size2.v aspect 1.)
let image = I.const (Color.v 0.314 0.784 0.471 1.)

(* Browser bureaucracy. *)

let main _ = 
  let d = Dom_html.window ## document in 
  let a = 
    let a = Dom_html.createA d in 
    a ## title <- Js.string "Download PNG file";
    a ## href <- Js.string "#"; 
    a ## setAttribute (Js.string "download", Js.string "minc.png");
    Dom.appendChild (d ## body) a; a
  in 
  let c = 
    let c = Dom_html.createCanvas d in 
    Dom.appendChild a c; c
  in 
  let r = Vgr.create (Vgr_htmlc.target c) `Other in 
  assert(Vgr.render r (`Image (size, view, image)) = `Ok); 
  a ## href <- (c ## toDataURL ());
  Js._false

let () = Dom_html.window ## onload <- Dom_html.handler main
          
  




