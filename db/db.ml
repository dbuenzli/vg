(*---------------------------------------------------------------------------
   Copyright 2013 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

open Gg;;
open Vg;;

let str = Printf.sprintf
let err_id id = str "An image with id `%s' already exists" id

type author = string * string
type image =
  { id : string;
    title : string;
    author : author;
    tags : string list;
    note : string option;
    size : Gg.size2;
    view : Gg.box2;
    image : Gg.box2 -> Vg.image; }

let images = Hashtbl.create 537
let image id ~title ~author ?(tags = []) ?note ~size ~view image =
  let id = String.lowercase id in
  try ignore (Hashtbl.find images id); invalid_arg (err_id id) with
  | Not_found ->
      Hashtbl.add images id
        { id; author; title; note; tags; size; view; image; }

let mem id = Hashtbl.mem images id
let find id = try Some (Hashtbl.find images id) with Not_found -> None
let prefixed s p =
  let ls = String.length s in
  let lp = String.length p in
  if lp > ls then false else
  try
    for i = 0 to lp - 1 do if s.[i] <> p.[i] then raise Exit; done;
    true
  with Exit -> false

let search ?(ids = []) ?(prefixes = []) ?(tags = []) () =
  let matches i =
    List.mem i.id ids || List.exists (prefixed i.id) prefixes ||
    List.exists (fun t -> List.mem t tags) i.tags
  in
  let select _ i acc = if matches i then i :: acc else acc in
  let compare i i' = compare i.id i'.id in
  List.sort compare (Hashtbl.fold select images [])

let all () = search ~prefixes:[""] ()

let indexes () =
  let add _ i (ids, tags) =
    let ids = i.id :: ids in
    let add_tag tags t = if List.mem t tags then tags else t :: tags in
    let tags = List.fold_left add_tag tags i.tags in
    ids, tags
  in
  let ids, tags = Hashtbl.fold add images ([],[]) in
  List.sort compare ids, List.sort compare tags

let xmp ~create_date ~creator_tool i =
  Vgr.xmp ~title:i.title ~authors:[fst i.author] ~subjects:i.tags
    ?description:i.note ~creator_tool ~create_date ()

let renderable i = i.size, i.view, i.image i.view
let find_loc id locs =
  let rec loop found = function
  | [] -> found
  | (id', loc) :: locs ->
      if id = id' then Some loc else
      if prefixed id id' then loop (Some loc) locs else
      loop found locs
  in
  loop None locs

(* Authors *)

let dbuenzli = "Daniel Bünzli", "http://erratique.ch"

(*---------------------------------------------------------------------------
   Copyright 2013 Daniel C. Bünzli.
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

   3. Neither the name of Daniel C. Bünzli nor the names of
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
