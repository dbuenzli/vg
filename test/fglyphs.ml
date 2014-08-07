(*---------------------------------------------------------------------------
   Copyright 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(* Renders a font's glyphs to a PDF file (does not use Vg's glyph API). *)

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

module Int = struct type t = int let compare = compare end
module Gmap = Map.Make (Int) (* glyph id maps *)

type otf_info =
  { i_otf : string;                                      (* The font bytes. *)
    i_name : string;                                    (* PostScript name. *)
    i_units_per_em : int;                        (* Number of units per em. *)
    i_bbox : int * int * int * int;                 (* glyphs bounding box. *)
    i_glyph_count : int;                                    (* glyph count. *)
    i_outlines : Otfm.glyph_descr Gmap.t; }  (* maps glyph ids to outlines. *)

let decode_outlines gcount d =
  let ( >>= ) x f = match x with `Error _ as e -> e | `Ok v -> f v in
  let rec loop i acc =
    if i < 0 then `Ok acc else
    Otfm.loca d i
    >>= function
    | None -> loop (i - 1) acc
    | Some loc -> Otfm.glyf d loc >>= fun o -> loop (i - 1) (Gmap.add i o acc)
  in
  loop (gcount - 1) Gmap.empty

let font_info inf = match string_of_file inf with
| `Error _ as e -> e
| `Ok i_otf ->
    let ( >>= ) x f = match x with
    | `Error e -> `Error (str "%s: %s" inf (otfm_err_str e))
    | `Ok v -> f v
    in
    let d = Otfm.decoder (`String i_otf) in
    Otfm.postscript_name d
    >>= fun name -> Otfm.head d
    >>= fun headt -> Otfm.glyph_count d
    >>= fun i_glyph_count -> decode_outlines i_glyph_count d
    >>= fun i_outlines ->
    let i_name = match name with None -> "Unknown" | Some n -> n in
    let i_units_per_em = headt.Otfm.head_units_per_em in
    let i_bbox = Otfm.(headt.head_xmin, headt.head_ymin,
                       headt.head_xmax, headt.head_ymax)
    in
    `Ok ({ i_otf; i_name; i_units_per_em; i_glyph_count; i_bbox; i_outlines })

let font_bbox fi fsize =
  let u_to_em = float fi.i_units_per_em in
  let size v = (fsize *. (float v)) /. u_to_em in
  let size_pt x y = P2.v (size x) (size y) in
  let minx, miny, maxx, maxy = fi.i_bbox in
  Box2.of_pts (size_pt minx miny) (size_pt maxx maxy)

let add_contours tr size contours acc =
  let add_contour acc contour =
    if contour = [] then acc else
    let pt = match tr with
    | None -> fun x y -> P2.v (size x) (size y)
    | Some ((dx,dy), None) -> fun x y -> P2.v (size (x + dx)) (size (y + dy))
    | Some ((dx,dy), Some (a, b, c, d)) ->
            let m2 = M2.v a c b d in
            fun x y ->
              let x, y = V2.to_tuple (V2.ltr m2 (P2.v (float x) (float y))) in
              (* TODO maybe we should avoid going through ints again.
                 In any case the spec is very underspecified on the order
                 these things should happen. But this seems to yield
                 correct results. *)
              P2.v
                (size (Float.int_of_round x + dx))
                (size (Float.int_of_round y + dy))
    in
    let find_start acc = match contour with
    | (true, px, py as p) :: pts ->
        [p], p, acc >> P.sub (pt px py), pts
    | (false, cx, cy as c) :: (true, px, py as p) :: pts ->
        [c; p], p, acc >> P.sub (pt px py), pts
    | (false, cx, cy as c) :: ((false, cx', cy') :: _ as pts) ->
        let px = (cx + cx') / 2 in
        let py = (cy + cy') / 2 in
        let p = (true, px, py) in
        [c; p], p, acc >> P.sub (pt px py), pts
    | pts -> (* degenerate *)
        [true, 0, 0], (true, 0, 0), acc >> P.sub P2.o, pts
    in
    let rec add_pts ends (last_on, lx, ly) acc = function
    | (false, cx, cy as last) :: pts ->
        if last_on then add_pts ends last acc pts else
        let px = (lx + cx) / 2 in
        let py = (ly + cy) / 2 in
        let acc' = acc >> P.qcurve (pt lx ly) (pt px py) in
        add_pts ends last acc' pts
    | (true, px, py as last) :: pts ->
        let seg =
          if last_on then P.line (pt px py) else
          P.qcurve (pt lx ly) (pt px py)
        in
        add_pts ends last (acc >> seg) pts
    | [] ->
        if last_on then begin match ends with
        | [(true, px, py)] ->
            acc
            >> P.line (pt px py)
            >> P.close
        | [(false, cx, cy); (true, px, py)] ->
            acc
            >> P.qcurve (pt cx cy) (pt px py)
            >> P.close
        | _ -> assert false
        end else begin match ends with
        | [(true, px, py)] ->
            acc
            >> P.qcurve (pt lx ly) (pt px py)
            >> P.close
        | [(false, cx, cy); (true, px, py)] ->
            let nx = (lx + cx) / 2 in
            let ny = (ly + cy) / 2 in
            acc
            >> P.qcurve (pt lx ly) (pt nx ny)
            >> P.qcurve (pt cx cy) (pt px py)
            >> P.close
        | _ -> assert false
        end
    in
    let ends, last, acc, pts = find_start acc in
    add_pts ends last acc pts
  in
  List.fold_left add_contour acc contours

let glyph_path fi i fsize =
  let u_to_em = float fi.i_units_per_em in
  let size v = (fsize *. (float v)) /. u_to_em in
  try match fst (Gmap.find i fi.i_outlines) with
  | `Simple contours -> add_contours None size contours P.empty
  | `Composite cs ->
      let rec add_composites acc = function
      | [] -> acc
      | (gid, dv, m) :: cs ->
          try match fst (Gmap.find gid fi.i_outlines) with
          | `Simple contours ->
              let acc' = add_contours (Some (dv, m)) size contours acc in
              add_composites acc' cs
          | `Composite cs ->
              (* TODO forbid recursive composite for now *)
              Printf.eprintf "Warning: unhandled recursive composite\n%!";
              add_composites acc cs
          with
          | Not_found -> add_composites acc cs
      in
      add_composites P.empty cs
  with Not_found -> P.empty

let renderable fi fsize cols nobb =
  let gcount = fi.i_glyph_count in
  let rows = (gcount / cols) + 1 in
  let bbox = font_bbox fi fsize in
  let pad = V2.(0.5 * Box2.size bbox) in
  let dglyph = V2.(Box2.size bbox + pad) in
  let margin = V2.(2. * Box2.size bbox) in
  let grid = V2.v (float cols) (float rows) in
  let gmax = V2.(grid - Size2.unit) in
  let size = V2.((mul grid (Box2.size bbox)) + (mul gmax pad) + 2. * margin) in
  let pos i =
    let idx = V2.v (float (i mod cols)) (float (i / cols)) in
    V2.(margin + (mul idx dglyph))
  in
  let black = I.const Color.black in
  let bbox =
    if nobb then I.void else
    let area = `O { P.o with P.width = 0.01 *. fsize } in
    black >> I.cut ~area (P.empty >> P.rect bbox)
  in
  let rec add_glyphs acc i =
    if i = gcount then acc else
    let glyph = glyph_path fi i fsize in
    let glyph = bbox >> I.blend (black >> I.cut glyph) >> I.move (pos i) in
    add_glyphs (acc >> I.blend glyph) (i + 1)
  in
  let i = add_glyphs (I.const Color.white) 0 in
  let view = Box2.v P2.o size in
  `Image (size, view, i)

let sample font size cols nobb = match font_info font with
| `Error _ as e -> e
| `Ok font_info ->
    let renderable = renderable font_info size cols nobb in
    let r = Vgr.create (Vgr_pdf.target ()) (`Channel stdout) in
    ignore (Vgr.render r renderable);
    ignore (Vgr.render r `End);
    `Ok ()

(* Command line *)

let exec = Filename.basename Sys.executable_name
let main () =
  let usage = Printf.sprintf
      "Usage: %s [OPTION]... [FONTFILE] \n\
       Renders glyph outlines to a PDF document on stdout.\n\
       Options:" exec
  in
  let font = ref None in
  let size = ref 5. in
  let cols = ref 10 in
  let nobb = ref false in
  let set_font s =
    if !font = None then font := Some s else
    raise (Arg.Bad "only a single font can be specified" )
  in
  let options = [
    "-s", (Arg.Set_float size), " SIZE, specify the font size (in mm)";
    "-c", (Arg.Set_int cols), " NUM, specify number of columns";
    "-nobb", (Arg.Set nobb), " don't print bounding boxes"; ]
  in
  Arg.parse (Arg.align options) set_font usage;
  match !font with
  | None -> Format.eprintf "%s: need to specify a font file" exec; exit 1
  | Some font ->
      match sample font !size !cols !nobb with
      | `Error e -> Format.eprintf "%s: %s@." exec e; exit 1
      | `Ok () -> exit 0

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
