(*---------------------------------------------------------------------------
   Copyright (c) %%COPYRIGHTYEAR%%, Daniel C. Bünzli. All rights reserved.
   Distributed under a BSD license, see license at the end of the file.
   %%PROJECTNAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

open Gg;;
open Vg;;

let exec = Filename.basename Sys.executable_name
let tmp_dir  = Filename.temp_dir_name
let str = Printf.sprintf
let pr_err s = Printf.eprintf "%s:%s\n" exec s
let apply f x ~finally y = 
  let res = try f x with exn -> finally y; raise exn in
  finally y;
  res

(* Pretty printing test info  *)

let pp_blank_line pp () = Format.fprintf pp "@\n@\n"
let rec pp_list psep pv pp = function
  | v :: l -> pv pp v; if (l <> []) then (psep pp (); pp_list psep pv pp l)
  | [] -> ()

let pp_paragraphs pp paragraphs = 
  let pp_words = pp_list Format.pp_print_space Format.pp_print_string in
  let pp_paras = pp_list pp_blank_line pp_words in
  Format.pp_open_box pp 0;
  pp_paras pp paragraphs;
  Format.pp_close_box pp ()

let pp_test_info pp t = 
  let pr pp fmt = Format.fprintf pp fmt in 
  let pp_text pp s = pp_paragraphs pp (Test.paragraphs s) in
  Format.pp_open_box pp 0;
  pr pp "%s@\n" t.Test.name;
  pr pp "  * "; pp_text pp t.Test.info;
  if t.Test.note <> "" then (pr pp "@\n  * "; pp_text pp t.Test.note);
  Format.pp_close_box pp ()

let print_info tests = 
  let sep pp () = Format.fprintf pp "@\n@\n" in
  pp_list sep pp_test_info Format.std_formatter tests;
  Format.printf "@\n@?"


(* Backend output *)


let backend_list () = "pdf, svg, printf" (* TODO *)

let main = 
  let usage = 
    str "Usage: %s <options>\n\
         Without any options selects all tests, all backends and outputs\n\
         the result in %s.\n\
	 Options:" exec tmp_dir
  in
  let list () = 
    let l = ref [] in
    l, fun v -> l := v :: !l   
  in
  let names, add_name = list () in 
  let prefixes, add_prefix = list () in 
  let keywords, add_keyword = list () in 
  let backends, add_backend = list () in 
  let info = ref false in 
  let dest = ref tmp_dir in 
  let options = [
    "-n", Arg.String add_name,
    "<name>, selects the test <name>.";
    "-p", Arg.String add_prefix, 
    "<prefix>, selects any test whose name matches <prefix>.";
    "-k", Arg.String add_keyword, 
    "<keyword>, selects any test whose name, info or note matches <keyword>.";
    "-b", Arg.String add_backend, 
    (str "<backend>, selects backend <backend>.\n\
          <backend> should be one of %s." (backend_list ()));
    "-i", Arg.Set info, 
    "outputs info about selected tests on stdout (no backend generation).";
    "-d", Arg.Set_string dest,
    (str "<dir>, directory in which files are output (defaults to %s)." 
       Filename.temp_dir_name); ]
  in
  Arg.parse options (fun _ -> raise (Arg.Bad "unknown argument")) usage;
  let tests = match !names, !prefixes, !keywords with 
  | [], [], [] -> Test.find ~prefixes:[""] ()               (* all tests. *)
  | names, prefixes, keywords -> Test.find ~names ~prefixes ~keywords () 
  in
  if !info then print_info tests else
  failwith "unimplemented"
;;


(*

type backend  = [ `PDF | `SVG12 | `Printf ]

type backend = 
    { bname : string; 
      output : Suite.test -> unit; 
      finish : Suite.test -> unit }

let backends = ref []
let backend ~name ~output ~finish = 
  let b = { bname = name; output = output; finish = finish } in
  backends := b :: !backends

let output_to_file ~ext outf t = 
  let fname = str "%s%s" t.Suite.name ext in
  let oc = open_out_bin fname in
  let out = outf oc t in
  apply out (Lazy.force t.Suite.img) ~finally:close_out oc

let () = 
  let out_pdf oc t i = 
    Vgo.output_pdf 
      ~title:t.Suite.name 
      ~creator:"vgtest"
      (Vgo.output_of_channel oc) [ (t.Suite.size, t.Suite.view, i) ]
  in
  backend 
    ~name: "pdf"
    ~output: (output_to_file ~ext:".pdf" out_pdf)
    ~finish: (fun t -> ())

let () = 
  let out_svg oc t i = 
    Vgo.output_svg 
      ~title:t.Suite.name 
      (Vgo.output_of_channel oc) (t.Suite.size, t.Suite.view, i) 
  in
  backend 
    ~name: "svg"
    ~output: (output_to_file ~ext:".svg" out_svg)
    ~finish: (fun t -> ())
	
let () = 
  let out_debug oc _ i = I.print (Format.formatter_of_out_channel oc) i in
  backend
    ~name: "debug"
    ~output: (output_to_file ~ext:".debug" out_debug)
    ~finish: (fun t -> ())

let make_test dest t b = 
  pr "%s:%s%!" t.Suite.name b.bname;
  try b.output t; pr ": success\n" with
  | Sys_error e | Failure e | Vgo.Unsupported e -> pr ": failed: %s\n" e

let process dest info bname todo =
  let find ~kind finder l l' = 
    let find acc n = 
      try (List.find (finder n) l') :: acc with
      | Not_found -> pr_err (str "unknown %s : %s" kind n); acc
    in
    List.fold_left find [] l
  in
  let todo = if todo = [] then tests 
  else find ~kind:"test" (fun n t -> t.Suite.name = n) todo tests
  in
  let backs = if bname = "" then !backends 
  else find ~kind:"backend" (fun n b -> b.bname = n) [ bname ] !backends
  in
  if not info then List.iter (fun t -> List.iter (make_test dest t) backs) todo
  else  
    let pr_test_info t = pr "%s: %s\n" t.Suite.name t.Suite.description in
    List.iter pr_test_info tests
*)

  

(*---------------------------------------------------------------------------
   Copyright (c) %%COPYRIGHTYEAR%%, Daniel C. Bünzli
   All rights reserved.

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions
   are met:
     
   1. Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

   2. Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

   3. Neither the name of the Daniel C. Bünzli nor the names of
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
  ---------------------------------------------------------------------------*)
