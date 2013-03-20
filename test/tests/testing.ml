(*---------------------------------------------------------------------------
   Copyright %%COPYRIGHT%%. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

open Gg;;
open Vg;;

let err_unique_name n = "A test named '" ^ n ^ "' already exists." 

let ( & ) f x = f x
type t = 
    { name : string;          (* Test identifier. *) 
      info : string;          (* Short description of the image *)
      note : string;          (* Details to look for. *)
      size : Gg.size2;        (* Output surface size. *)
      view : Gg.box2;         (* View rectangle. *)
      image : Vg.image Lazy.t (* Image. *) }

let tests = Hashtbl.create 200 

let test name ~info ?(note = "") ~size ~view image = 
  let name = String.lowercase name in
  try ignore (Hashtbl.find tests name); invalid_arg (err_unique_name name) with
  | Not_found ->
      Hashtbl.add tests name 
	{ name = name; info = info; note = note; view = view; size = size;
	  image = image }

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
