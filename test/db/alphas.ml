(*---------------------------------------------------------------------------
   Copyright (c) 2013 The vg programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Gg
open Vg
;;

(** Test images for alpha blending. *)

Db.image "alpha-spots" __POS__ ~author:Db.dbuenzli
  ~title:"Alpha spots"
  ~tags:["alpha"]
  ~note:"Spots with 0.75 alpha composed in various order. Left to right,
         top to bottom, back most color first: rgb, rbg, grb, gbr, brg, bgr."
  ~size:(Size2.v 70. 100.)
  ~view:(Box2.v P2.o (Size2.v 0.7 1.0))
  begin fun _ ->
   let a = Float.pi_div_2 in
   let da = Float.two_pi /. 3. in
   let dotp = P.empty |> P.circle P2.o 0.08 in
   let dot c da = I.const c |> I.cut dotp |> I.move (V2.polar 0.05 (a +. da)) in
   let r = dot (Color.v_srgb 0.608 0.067 0.118 ~a:0.75) da in
   let g = dot (Color.v_srgb 0.314 0.784 0.471 ~a:0.75) 0. in
   let b = dot (Color.v_srgb 0.000 0.439 0.722 ~a:0.75) (-. da) in
   let triplet a b c = a |> I.blend b |> I.blend c in
   let triplet_row y a b c =
     let fst = triplet a b c |> I.move (P2.v 0.2 y) in
     let snd = triplet a c b |> I.move (P2.v 0.5 y) in
     fst |> I.blend snd
   in
   (triplet_row 0.8 r g b) |> I.blend
   (triplet_row 0.5 g r b) |> I.blend
   (triplet_row 0.2 b r g)
  end
