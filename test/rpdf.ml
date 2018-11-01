(*---------------------------------------------------------------------------
   Copyright (c) 2013 The vg programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
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
  Vgr.create (Vgr_pdf.target ~font ~xmp:(Rstored.xmp is) ()) dst

let () = Rstored.main "PDF" "pdf" ~pack:true renderer

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
