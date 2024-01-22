(*---------------------------------------------------------------------------
   Copyright (c) 2014 The vg programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Gg
open Vg

include Db_contents

let dpi300 = let s = 300. /. 0.0254 in Size2.v s s

let formats =
  [ ("png", false), `Png dpi300;
    ("pdf", true), `Pdf;
    ("ps", true), `Ps;
    ("svg", false), `Svg; ]

let renderer fmt dst _ =
  let cairo_fmt = List.assoc fmt formats in
  Vgr.create (Vgr_cairo.stored_target cairo_fmt) dst

let ftypes = List.map fst formats
let () = Rstored.main_multiformats "PNG, PDF, PS or SVG" ftypes renderer
