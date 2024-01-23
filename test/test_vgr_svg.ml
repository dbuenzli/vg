(*---------------------------------------------------------------------------
   Copyright (c) 2013 The vg programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Gg
open Vg

include Db_contents

let renderer dst is =
  Vgr.create (Vgr_svg.target ~xmp:(Test_vgr_stored.xmp is) ()) dst

let () = Test_vgr_stored.main "SVG" "svg" ~pack:false renderer
