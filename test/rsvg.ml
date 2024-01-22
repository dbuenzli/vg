(*---------------------------------------------------------------------------
   Copyright (c) 2013 The vg programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Gg
open Vg

include Db_contents

let renderer dst is = Vgr.create (Vgr_svg.target ~xmp:(Rstored.xmp is) ()) dst

let () = Rstored.main "SVG" "svg" ~pack:false renderer
