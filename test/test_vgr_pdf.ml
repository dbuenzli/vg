(*---------------------------------------------------------------------------
   Copyright (c) 2013 The vg programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Renders the Vg image database to PDF. *)

open Vg
include Db_contents

let renderer dst is =
  let open_sans_xbold = match Vgr_pdf.otf_font Open_sans.extra_bold with
  | Error e -> Format.eprintf "%a" Otfm.pp_error e; `Sans
  | Ok otf -> otf
  in
  let font f = match f.Font.name, f.Font.weight with
  | "Open Sans", `W800 -> open_sans_xbold
  | _ -> Vgr_pdf.font f
  in
  Vgr.create (Vgr_pdf.target ~font ~xmp:(Test_vgr_stored.xmp is) ()) dst

let () = Test_vgr_stored.main "PDF" "pdf" ~pack:true renderer
