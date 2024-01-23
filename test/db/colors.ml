(*---------------------------------------------------------------------------
   Copyright (c) 2013 The vg programmers. All rights reserved.
   SPDX-License-Identifier: ISC
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
