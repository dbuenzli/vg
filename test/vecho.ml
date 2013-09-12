(*---------------------------------------------------------------------------
   Copyright 2013 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

open Gg
open Vg

let str = Printf.sprintf
let otfm_err_str err =
  Format.fprintf Format.str_formatter "%a" Otfm.pp_error err; 
  Format.flush_str_formatter ()

let string_of_file inf =
  try
    let ic = if inf = "-" then stdin else open_in_bin inf in
    let close ic = if inf <> "-" then close_in ic else () in
    let buf_size = 65536 in
    let b = Buffer.create buf_size in 
    let s = String.create buf_size in 
    try
      while true do
        let c = input ic s 0 buf_size in 
        if c = 0 then raise Exit else
        Buffer.add_substring b s 0 c
      done;
      assert false
    with
    | Exit -> close ic; `Ok (Buffer.contents b)
    | Failure _ -> close ic; `Error (str "%s: input file too large" inf)
    | Sys_error e -> close ic; `Error (str "%s: %s" inf e)
  with
  | Sys_error e -> `Error (str "%s: %s" inf e)
                     
(* Font information *) 

module Int = struct type t = Vg.glyph let compare = compare end
module Gmap = Map.Make (Int) (* glyph maps *) 
module Cmap = Gmap           (* uchar maps *) 

type otf_info = 
  { i_otf : string;                                      (* The font bytes. *)
    i_cmap : int Cmap.t;           (* Maps unicode scalar values to glyphs. *)
    i_advs : int Gmap.t;             (* Maps glyph to advances in em space. *) 
    i_units_per_em : int; }                      (* Number of units per em. *) 
  
let add_adv acc g adv _ = Gmap.add g adv acc
let add_cmap acc kind (u0, u1) g =
  let acc = ref acc in
  begin match kind with 
  | `Glyph_range ->
      for i = 0 to (u1 - u0) do acc := Cmap.add (u0 + i) (g + i) !acc done;
  | `Glyph -> 
      for u = u0 to u1 do acc := Cmap.add u g !acc done
  end;
  !acc
      
let font_info font = match font with
| None -> `Ok ("Courier", None)
| Some inf -> 
    match string_of_file inf with 
    | `Error _ as e -> e 
    | `Ok i_otf ->
        let ( >>= ) x f = match x with 
        | `Error e -> `Error (str "%s: %s" inf (otfm_err_str e))
        | `Ok v -> f v 
        in
        let d = Otfm.decoder (`String i_otf) in
        Otfm.postscript_name d          >>= fun name ->
        Otfm.head d                     >>= fun head -> 
        Otfm.cmap d add_cmap Cmap.empty >>= fun (_, i_cmap) ->
        Otfm.hmtx d add_adv Gmap.empty  >>= fun i_advs ->
        let name = match name with None -> "Unknown" | Some n -> n in
        let i_units_per_em = head.Otfm.head_units_per_em in
        `Ok (name, Some { i_otf; i_cmap; i_advs; i_units_per_em })
          
(* Text layout *) 

let fixed_layout text =
  let units_per_em = 1000. in
  let add_adv acc _ = function 
  | `Malformed _ -> acc | `Uchar _ -> acc + 600 (* Courier's advance *)
  in
  [], [], (float (Uutf.String.fold_utf_8 add_adv 0 text)) /. units_per_em

let otf_layout info text =
  let get_adv g = try Gmap.find g info.i_advs with Not_found -> 0 in
  let get_glyph u = try Cmap.find u info.i_cmap with Not_found -> 0 in
  let rec add_glyph (gs, adv, len as acc) i = function 
  | `Malformed _ -> add_glyph acc i (`Uchar Uutf.u_rep)
  | `Uchar u ->
      let g = get_glyph u in
      (g :: gs, [], len + (get_adv g))
  in
  let gs, advs, lens = Uutf.String.fold_utf_8 add_glyph ([], [], 0) text in 
  (gs, advs, (float lens) /. (float info.i_units_per_em))
  
(* Text rendering *)

let renderable (fname, info) size text = 
  let glyphs_rev, _, len = match info with 
  | None -> fixed_layout text
  | Some info -> otf_layout info text
  in
  let font = { Font.name = fname; slant = `Normal; weight = `W400; size } in
  let i = 
    I.const (Color.black) >>
    I.cut_glyphs ~text font (List.rev glyphs_rev) >>
    I.move V2.(0.5 * (v size size))
  in
  let size = Size2.v (len *. size +. size) (2. *. size) in
  let view = Box2.v P2.o size in 
  `Image (size, view, i)

let font_resolver = function 
| name, None -> `Ok (fun _ -> `Fixed) 
| name, Some info -> 
    match Vgr_pdf.otf_font info.i_otf with 
    | `Error _ as e -> e 
    | `Otf _ as otf -> `Ok (fun _ -> otf)
        
let echo font size msg = match font_info font with
| `Error _ as e -> e
| `Ok font_info ->
    match font_resolver font_info with 
    | `Error e -> `Error (otfm_err_str e)
    | `Ok font -> 
        let renderable = renderable font_info size msg in
        let r = Vgr.create (Vgr_pdf.target ~font ()) (`Channel stdout) in
        ignore (Vgr.render r renderable); 
        ignore (Vgr.render r `End); 
        `Ok

(* Command line *)

let exec = Filename.basename Sys.executable_name
let main () = 
  let usage = Printf.sprintf 
      "Usage: %s [OPTION]... [STRING]... \n\
       Writes UTF-8 encoded strings to a PDF document on stdout.\n\
       Options:" exec
  in
  let font = ref "" in
  let size = ref 20. in
  let msg = Buffer.create 255 in
  let add_string s = Buffer.add_string msg s; Buffer.add_char msg ' ' in
  let options = [ 
    "-f", (Arg.Set_string font), " FILE, specify the OpenType font file to use";
    "-s", (Arg.Set_float size), " SIZE, specify the font size (in mm)"
  ]
  in
  Arg.parse (Arg.align options) add_string usage;
  let msg = 
    let l = Buffer.length msg in
    Buffer.sub msg 0 (if l > 0 then l - 1 else 0) (* rem. last ' ' *) 
  in
  let font = match !font with "" -> None | f -> Some f in
  match echo font !size msg with 
  | `Error e -> Format.eprintf "%s: %s@." exec e; exit 1 
  | `Ok  -> exit 0
  
let () = main ()

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
