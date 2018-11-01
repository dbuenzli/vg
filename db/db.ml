(*---------------------------------------------------------------------------
   Copyright (c) 2013 The vg programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Gg;;
open Vg;;

let str = Printf.sprintf
let err_id id = str "An image with id `%s' already exists" id

type author = string * string
type image =
  { id : string;
    loc : string * int;
    title : string;
    author : author;
    tags : string list;
    note : string option;
    size : Gg.size2;
    view : Gg.box2;
    image : Gg.box2 -> Vg.image; }

let images = Hashtbl.create 537
let image id loc ~title ~author ?(tags = []) ?note ~size ~view image =
  let file, line, _, _ = loc in
  let id = String.lowercase_ascii id in
  try ignore (Hashtbl.find images id); invalid_arg (err_id id) with
  | Not_found ->
      Hashtbl.add images id
        { id; loc = file, line; author; title; note; tags; size; view; image; }

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

(* Authors *)

let dbuenzli = "Daniel Bünzli", "http://erratique.ch"

(*---------------------------------------------------------------------------
   Copyright (c) 2013 The vg programmers

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
