

open Mui;;


let btoa (s : Js.js_string Js.t) : Js.js_string Js.t = 
  Js.Unsafe.fun_call (Js.Unsafe.variable "btoa") [|Js.Unsafe.inject s|]

let binary_to_data_uri () = 
  (Js.string "data:application/octet-stream;base64,") ## 
  concat (Js.bytestring "\xFF\xA0\xCD")

let binary_to_data_alt () = 
  (Js.string "data:application/octet-stream,") ## 
  concat (Js.escape (Js.bytestring "\xFF\xA0\xCD\xc3\xa9"))

let txt s = Dom_html.document ## createTextNode (s)

let main () = 
  let a = Dom_html.createA Dom_html.document in 
  a ## href <- binary_to_data_alt ();
  Dom.appendChild a (txt (Js.string "Link"));
  a ## setAttribute (Js.string "download", Js.string "file.bin");
  Dom.appendChild (Dom_html.document ## body) a;
  Dom.appendChild (Dom_html.document ## body) (txt (binary_to_data_uri ()));
  Mui.Log.msg "WORKS"

let () = 
  Dom_html.window ## onload <- Dom.handler (fun _ -> main (); Js._false)
