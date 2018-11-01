(*---------------------------------------------------------------------------
   Copyright (c) 2013 The vg programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(* Generic infrastructure for rendering Vg's image db on stored destinations. *)

open Gg
open Vg

let str = Printf.sprintf
let pp = Format.fprintf
let pp_dur ppf dur = pp ppf "%.2fms" (dur *. 1000.)
let pp_str = Format.pp_print_string
let rec pp_list ?(pp_sep = Format.pp_print_cut) pp_v ppf = function
| [] -> ()
| v :: vs ->
    pp_v ppf v; if vs <> [] then (pp_sep ppf (); pp_list ~pp_sep pp_v ppf vs)

let pp_text ppf s =          (* hint spaces and new lines with Format's funs *)
  let len = String.length s in
  let left = ref 0 in
  let right = ref 0 in
  let flush () =
    pp_str ppf (String.sub s !left (!right - !left));
    incr right; left := !right;
  in
  while (!right <> len) do
    if s.[!right] = '\n' then (flush (); Format.pp_force_newline ppf ()) else
    if s.[!right] = ' ' then (flush (); Format.pp_print_space ppf ()) else
    incr right
  done;
  if !left <> len then flush ()

let exec = Filename.basename Sys.executable_name
let log_msg fmt = Format.eprintf ("%s: " ^^ fmt ^^ "@?") exec
let log fmt = Format.eprintf (fmt ^^ "@?")
let duration f x =
  let start = Unix.gettimeofday () in
  f x; Unix.gettimeofday () -. start

(* Metadata *)

let xmp is =
  let create_date = Unix.gettimeofday () in
  let creator_tool = exec in
  match is with
  | [i] -> Db.xmp ~create_date ~creator_tool i
  | _ -> Vgr.xmp ~create_date ~creator_tool ()

(* Render *)

let unix_buffer_size = 65536                      (* UNIX_BUFFER_SIZE 4.0.0 *)
let rec unix_write fd s j l =
  let rec write fd s j l = try Unix.single_write fd s j l with
  | Unix.Unix_error (Unix.EINTR, _, _) -> write fd s j l
  in
  let wc = write fd s j l in
  if wc < l then unix_write fd s (j + wc) (l - wc) else ()

let string_to_channel use_unix oc s =
  if use_unix
  then unix_write (Unix.descr_of_out_channel oc) s 0 (Bytes.length s)
  else output_bytes oc s

let rec render_unix fd s r v = match Vgr.render r v with
| `Ok -> ()
| `Partial ->
    unix_write fd s 0 (Bytes.length s - Vgr.Manual.dst_rem r);
    Vgr.Manual.dst r s 0 (Bytes.length s);
    render_unix fd s r `Await

let rec render_imgs render r = function
| [] -> ignore (render r `End)
| i :: is ->
    ignore (render r (`Image (Db.renderable i)));
    render_imgs render r is

let render_with_channel fn renderer imgs =
  let oc = open_out fn in
  let r = renderer (`Channel oc) imgs in
  try
    render_imgs Vgr.render r imgs;
    close_out oc
  with e -> close_out oc; raise e

let render_with_buffer buf use_unix fn renderer imgs =
  let oc = open_out fn in
  let r = renderer (`Buffer buf) imgs in
  try
    render_imgs Vgr.render r imgs;
    string_to_channel use_unix oc (Buffer.to_bytes buf);
    close_out oc;
  with e -> close_out oc; raise e

let render_with_unix s fn renderer imgs =
  let fd = Unix.(openfile fn [O_WRONLY] 0) in
  let r = renderer `Manual imgs in
  try
    Vgr.Manual.dst r s 0 (Bytes.length s);
    render_imgs (render_unix fd s) r imgs;
    Unix.close fd;
  with e -> Unix.close fd; raise e

let render sout use_unix usize dir ftype pack renderer imgs =
  let render =
    if sout then render_with_buffer (Buffer.create usize) use_unix else
    if use_unix then render_with_unix (Bytes.create usize) else
    render_with_channel
  in
  let render_to_file fn img = try
    log "Writing %s @?" fn;
    let dur = duration (render fn renderer) img in
    log "(%a) [DONE]@." pp_dur dur;
  with
  | Sys_error e -> log "[FAIL]@."; log_msg "%s@." e; exit 1
  | Unix.Unix_error (e, _, v) ->
      log "[FAIL]@."; log_msg "%s: %s@." (Unix.error_message e) v; exit 1
  in
  let fname id = Filename.concat dir (str "%s.%s" id (fst ftype)) in
  match pack with
  | None -> List.iter (fun i -> render_to_file (fname i.Db.id) [i]) imgs
  | Some pack when not (snd ftype) ->
      log "Sorry cannot -pack the %s format." (fst ftype); exit 1
  | Some pack ->
      render_to_file (fname pack) imgs

(* Dump textual representation. *)

let dump dir ftype i = try
  let fn = Filename.concat dir (str "%s.%s.dump" i.Db.id ftype) in
  let oc = open_out fn in
  let ppf = Format.formatter_of_out_channel oc in
  try
    log "Writing %s @?" fn;
    let dur = duration (fun () -> (I.pp ppf) (i.Db.image i.Db.view)) () in
    log "(%a) [DONE]@." pp_dur dur;
    close_out oc
  with e -> log "[FAIL]@."; close_out oc; raise e
with Sys_error e -> log_msg "%s@." e; exit 1

(* Image info *)

let pp_image_info ppf i =
  let pp_comma ppf () = pp ppf ",@ " in
  let pp_tags ppf = function
  | [] -> ()
  | ts -> pp ppf " @[<1>[%a]@]" (pp_list ~pp_sep:pp_comma pp_str) i.Db.tags
  in
  let pp_opt_text_field fn ppf = function
  | None -> ()
  | Some fv -> pp ppf "%s: @[%a@]@," fn pp_text fv
  in
  pp ppf "* @[<v>%s%a@,@," i.Db.id pp_tags i.Db.tags;
  pp ppf "@[%a@]@," pp_text i.Db.title;
  pp ppf "@[%a, %s@]@," pp_text (fst i.Db.author) (snd i.Db.author);
  pp_opt_text_field "note" ppf i.Db.note;
  pp ppf "@]"

(* Command line *)

let main_multiformats rname ftypes renderer =
  let usage = Printf.sprintf
      "Usage: %s [OPTION]... [ID1] [ID2]...\n\
      \ Renders images of the Vg image database to %s files.\n\
      \ Without any selector and ID specified renders all images.\n\
      Options:" exec rname
  in
  let ftype = ref (List.hd ftypes) in
  let set_ftype fmt = ftype := List.find (fun (f, _) -> f = fmt) ftypes in
  let cmd = ref `Image_render in
  let set_cmd v () = cmd := v in
  let list () = let l = ref [] in (l, fun v -> l := v :: !l) in
  let ids, add_id = list () in
  let prefixes, add_prefix = list () in
  let tags, add_tag = list () in
  let pack = ref None in
  let dir = ref "/tmp" in
  let sout = ref false in
  let use_unix = ref false in
  let usize = ref unix_buffer_size in
  let nat s r v = if v > 0 then r := v else log "%s must be > 0, ignored\n" s in
  let options =
    begin match ftypes with
    | [] | [_] -> []
    | _ ->
        [ "-format", Arg.Symbol (List.map fst ftypes, set_ftype),
          Printf.sprintf "Selects the image format (default: %s)" (fst !ftype) ]
    end @ [
    "-dump", Arg.Unit (set_cmd `Image_dump),
    (str " Output a textual internal representation");
    "-p", Arg.String add_prefix,
    "<prefix> Selects any image whose id matches <prefix>, repeatable";
    "-t", Arg.String add_tag,
    "<tag> Selects any images tagged by <tag>, repeatable";
    "-ids", Arg.Unit (set_cmd `List_ids),
    " Output the selected image ids on stdout";
    "-tags", Arg.Unit (set_cmd `List_tags),
    " Output the tags of the selected images on stdout";
    "-i", Arg.Unit (set_cmd `Image_info),
    " Output info about selected images on stdout";
    "-pack", Arg.String (fun fn -> pack := Some fn),
    (str "<file> Pack the selected images in the single <file> (if supported)");
    "-d", Arg.Set_string dir,
    (str "<dir> directory in which files are output (defaults to `%s')" !dir);
    "-sout", Arg.Set sout,
    " Render to a string and output the string";
    "-unix", Arg.Set use_unix,
    " Use Unix IO";
    "-usize", Arg.Int (nat "-usize" usize),
    "<int> Unix IO buffer sizes in bytes"; ]
  in
  Arg.parse (Arg.align options) add_id usage;
  let imgs = match !ids, !prefixes, !tags with
  | [], [], [] -> Db.search ~prefixes:[""] () (* all images *)
  | ids, prefixes, tags -> Db.search ~ids ~prefixes ~tags ()
  in
  match !cmd with
  | `Image_render ->
      let renderer = renderer !ftype in
      let render = render !sout !use_unix !usize !dir !ftype !pack renderer in
      let dur = duration render imgs in
      log "Wrote %d images in %a.@." (List.length imgs) pp_dur dur
  | `Image_dump ->
      let dur = duration (List.iter (dump !dir (fst !ftype))) imgs in
      log "Wrote %d images in %a.@." (List.length imgs) pp_dur dur
  | `Image_info ->
      pp Format.std_formatter "@[<v>%a@]@." (pp_list pp_image_info) imgs
  | `List_ids ->
      List.iter (fun i -> print_endline i.Db.id) imgs
  | `List_tags ->
      let add_tag acc t = if List.mem t acc then acc else t :: acc in
      let add_tags acc i = List.fold_left add_tag acc i.Db.tags in
      let tags = List.fold_left add_tags [] imgs in
      List.iter print_endline (List.sort compare tags)

let main rname ftype ~pack renderer =
  main_multiformats rname [ftype, pack] (fun _ -> renderer)

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
