(*---------------------------------------------------------------------------
   Copyright (c) 2013 The vg programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Gg;;
open Vg;;

(* Tests the generic PDF font resolutions. *)

let supported_uchars =
  [ `Range (0x0000, 0x007F); `Range (0x00A0, 0x00FF);
    `Cp 0x20AC; `Cp 0x201A; `Cp 0x0192; `Cp 0x201E; `Cp 0x2026; `Cp 0x2020;
    `Cp 0x2021; `Cp 0x02C6; `Cp 0x2030; `Cp 0x0160; `Cp 0x2039; `Cp 0x0152;
    `Cp 0x017D; `Cp 0x2018; `Cp 0x2019; `Cp 0x201C; `Cp 0x201D; `Cp 0x2022;
    `Cp 0x2013; `Cp 0x2014; `Cp 0x02DC; `Cp 0x2122; `Cp 0x0161; `Cp 0x203A;
    `Cp 0x0153; `Cp 0x017E; `Cp 0x0178; ]

let foldi_uchars f acc =
  let rec loop i acc = function
  | [] -> acc
  | `Cp u :: us -> loop (i + 1) (f i acc u) us
  | `Range (l, h) :: us ->
      let acc = ref acc in
      for u = l to h do acc := f (i + u - l) !acc u done;
      loop (i + h - l + 1) !acc us
  in
  loop 0 acc supported_uchars

let utf8 u =
  let b = Buffer.create 4 in
  Uutf.Buffer.add_utf_8 b (Uchar.of_int u); Buffer.contents b

let glyph_chart font =
  let black = I.const Color.black in
  let gray = I.const (Color.gray 0.75) in
  let cage = Box2.v (P2.v (-0.2) (-0.2)) (Size2.v 0.8 0.8) in
  let cage = P.empty |> P.rect cage in
  let area = `O { P.o with P.width = 0.01 } in
  let glyph_box i acc u =
    let y = 15. -. float (i / 16) in
    let x = float (i mod 16) in
    let glyph =
      I.cut_glyphs ~text:(utf8 u) font [] black |>
      I.blend (I.cut ~area cage gray) |>
      I.move (P2.v x y)
    in
    acc |> I.blend glyph
  in
  foldi_uchars glyph_box I.void |>
  I.move (P2.v 2. 2.)

let font ?(bold = false) ?(slant = `Normal) name size =
  let weight = if bold then `W700 else `W400 in
  { Font.name = name; size; slant; weight; }

let size = Size2.v 130. 130.
let view = Box2.v P2.o (P2.v 19.5 19.5)
let tags = [ "glyph" ]
let fsize = 0.42

(* Helvetica *)

let helvetica = "Helvetica";;

Db.image "glyph-pdf-sans" __POS__ ~author:Db.dbuenzli
  ~title:"Glyph chart for PDF `Sans font resolution" ~tags ~size ~view
  begin fun view -> glyph_chart (font helvetica fsize) end;;

Db.image "glyph-pdf-sans-bf" __POS__ ~author:Db.dbuenzli
  ~title:"Glyphs of PDF `Sans bold font resolution" ~tags ~size ~view
  begin fun view -> glyph_chart (font ~bold:true helvetica fsize) end;;

let slant = `Oblique;;

Db.image "glyph-pdf-sans-obl" __POS__ ~author:Db.dbuenzli
  ~title:"Glyph chart for PDF `Sans oblique font resolution" ~tags ~size ~view
  begin fun view -> glyph_chart (font ~slant helvetica fsize) end;;

Db.image "glyph-pdf-sans-obl-bf" __POS__ ~author:Db.dbuenzli
  ~title:"Glyphs of PDF `Sans oblique bold font resolution" ~tags ~size ~view
  begin fun view -> glyph_chart (font ~slant ~bold:true helvetica fsize) end;;

(* Times *)

let times = "Times";;

Db.image "glyph-pdf-serif" __POS__ ~author:Db.dbuenzli
  ~title:"Glyph chart for PDF `Serif font resolution" ~tags ~size ~view
  begin fun view -> glyph_chart (font times fsize) end;;

Db.image "glyph-pdf-serif-bf" __POS__ ~author:Db.dbuenzli
  ~title:"Glyphs of PDF `Serif bold font resolution" ~tags ~size ~view
  begin fun view -> glyph_chart (font ~bold:true times fsize) end;;

let slant = `Italic;;

Db.image "glyph-pdf-serif-obl" __POS__ ~author:Db.dbuenzli
  ~title:"Glyph chart for PDF `Serif italic font resolution" ~tags ~size ~view
  begin fun view -> glyph_chart (font ~slant times fsize) end;;

Db.image "glyph-pdf-serif-obl-bf" __POS__ ~author:Db.dbuenzli
  ~title:"Glyphs of PDF `Serif italic bold font resolution" ~tags ~size ~view
  begin fun view -> glyph_chart (font ~slant ~bold:true times fsize) end;;

(* Courier *)

let courier = "Courier";;

Db.image "glyph-pdf-fixed" __POS__ ~author:Db.dbuenzli
  ~title:"Glyph chart for PDF `Fixed font resolution" ~tags ~size ~view
  begin fun view -> glyph_chart (font courier fsize) end;;

Db.image "glyph-pdf-fixed-bf" __POS__ ~author:Db.dbuenzli
  ~title:"Glyphs of PDF `Fixed bold font resolution" ~tags ~size ~view
  begin fun view -> glyph_chart (font ~bold:true courier fsize) end;;

let slant = `Italic;;

Db.image "glyph-pdf-fixed-obl" __POS__ ~author:Db.dbuenzli
  ~title:"Glyph chart for PDF `Fixed italic font resolution" ~tags ~size ~view
  begin fun view -> glyph_chart (font ~slant courier fsize) end;;

Db.image "glyph-pdf-fixed-obl-bf" __POS__ ~author:Db.dbuenzli
  ~title:"Glyphs of PDF `Fixed italic bold font resolution" ~tags ~size ~view
  begin fun view -> glyph_chart (font ~slant ~bold:true courier fsize) end;;


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
