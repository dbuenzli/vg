(*---------------------------------------------------------------------------
   Copyright (c) 2013 The vg programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Gg
open Vg
;;

(** Test images for colors. *)

Db.image "color-ramps" __POS__ ~author:Db.dbuenzli
  ~title:"Primary and grayscale ramps"
  ~tags:["color"]
  ~note:"From 0 to 1 by 0.1 increments in sRGB space. From right to left \
         top to bottom, red, green, blue, gray."
  ~size:(Size2.v 100. 100.)
  ~view:(Box2.v P2.o (Size2.v 2.2 2.2))
  begin fun _ ->
    let levels = [ 0.0; 0.1; 0.2; 0.3; 0.4; 0.5; 0.6; 0.7; 0.8; 0.9; 1.0 ] in
    let sq = P.empty |> P.rect (Box2.v P2.o (Size2.v 1.1 1.1)) in
    let bars color =
      let bar l = I.const (color l) |> I.cut sq |> I.move (P2.v l 0.) in
      let add_bar acc l = acc |> I.blend (bar l) in
      List.fold_left add_bar I.void levels
    in
    (bars (fun l -> Color.v_srgb l 0. 0.) |> I.move (P2.v 0.0 1.1)) |> I.blend
    (bars (fun l -> Color.v_srgb 0. l 0.) |> I.move (P2.v 1.1 1.1)) |> I.blend
    (bars (fun l -> Color.v_srgb 0. 0. l) |> I.move (P2.v 0.0 0.0)) |> I.blend
    (bars (fun l -> Color.v_srgb l  l  l) |> I.move (P2.v 1.1 0.0))
  end

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
