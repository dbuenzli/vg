

open Mui;;


(*
let btoa (s : Js.js_string Js.t) : Js.js_string Js.t = 
  Js.Unsafe.fun_call (Js.Unsafe.variable "btoa") [|Js.Unsafe.inject s|]

let binary_to_data_uri () = 
  (Js.string "data:application/octet-stream;base64,") ## 
  concat (Js.bytestring "\xFF\xA0\xCD")

let binary_to_data_alt () = 
  Js.string ("data:application/octet-stream," ^ 
    (Js.to_string (Js.escape (Js.bytestring "\xFF\xA0\xCD\xc3\xa9"))))
(*
  (Js.string "data:application/octet-stream,") ## 
  concat (Js.escape (Js.bytestring "\xFF\xA0\xCD\xc3\xa9"))
*)
let txt s = Dom_html.document ## createTextNode (s)

*)

(* let rec g n k = if n < 0 then Mui.Log.msg "end" else k (n - 1) g *)
(*  let a = Dom_html.createA Dom_html.document in 
  a ## href <- binary_to_data_alt ();
  Dom.appendChild a (txt (Js.string "Link"));
  a ## setAttribute (Js.string "download", Js.string "file.bin");
  Dom.appendChild (Dom_html.document ## body) a;
  Dom.appendChild (Dom_html.document ## body) (txt (binary_to_data_uri ()));
*)


let stop _ = Firebug.console ## log (Js.string "STOP")
let cont k n = k n
let rec f k n = if n < 0 then k n else (cont (f k) (n - 1))

let main () = f stop 2600

let () = Dom_html.window ## onload <- Dom.handler (fun _ -> main (); Js._false)

(* 

function partial(fn) {
  var slice = Array.prototype.slice;
  var args = slice.call(arguments, 1);
  return function() { 
   return fn.apply(this, args.concat(slice.call(arguments, 0))); 
  };
}

function stop (v) { console.log("STOP"); }
function cont (k, n) { k (n); }
function f (k, n) 
{ 
   if (n < 0) { return k (n); } else { return cont(partial(f,k), n - 1); } 
}
*)
