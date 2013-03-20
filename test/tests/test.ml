(*---------------------------------------------------------------------------
   Copyright %%COPYRIGHT%%. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

include Testing;;
include Paths;;

let find ?(names = []) ?(prefixes = []) ?(keywords = []) () = 
  let nomatch = "\000" in            (* assumes \000 never appears in input. *)
  let any l = if l = [] then nomatch else String.concat "\\|" l in
  let names = any (List.rev_map Str.quote names) in 
  let prefixes = any (List.rev_map (fun s -> (Str.quote s) ^ ".*") prefixes) in
  let keywords = 
    any (List.rev_map (fun s -> ".*" ^ (Str.quote s) ^ ".*") keywords) 
  in
  let r_name = Str.regexp_case_fold (any [names; prefixes; keywords]) in
  let r_info = Str.regexp_case_fold keywords in 
  let r_note = r_info in
  let matches t = 
    let m r s = Str.string_match r s 0 && Str.matched_string s = s in
    (m r_name t.name) || (m r_info t.info) || (m r_note t.note)
  in 
  let add_match _ t acc = if matches t then t :: acc else acc in 
  let compare t t' = compare t.name t'.name in
  List.sort compare (Hashtbl.fold add_match tests []) 

let r_paragraphs = Str.regexp "[ \t]*\n[ \t]*\n[ \n\t]*" 
let r_words = Str.regexp "[ \t]+\\|[ \t]*\n[ \t]*"
let paragraphs s = 
  let paragraphs = Str.split r_paragraphs s in
  List.map (Str.split r_words) paragraphs

(*---------------------------------------------------------------------------
   Copyright %%COPYRIGHT%%
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

   3. Neither the name of Daniel C. BÃ¼nzli nor the names of
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
