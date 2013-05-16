(* This code is in the public domain *) 

open Gg
open Vg


let pdf_of_usquare i = 
  let size = Size2.v 30. 30. in
  let view = Box2.unit in
  try
    let oc = open_out "/tmp/vg-basics.pdf" in
    let r = Vgr.create (Vgr_pdf.target ()) (`Channel oc) in
    try 
      ignore (Vgr.render r (`Image (size, view, i)));
      ignore (Vgr.render r `End);
      close_out oc
    with e -> close_out oc; raise e
  with Sys_error e -> prerr_endline e
