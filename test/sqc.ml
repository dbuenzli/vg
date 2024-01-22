(*---------------------------------------------------------------------------
   Copyright (c) 2013 The vg programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Illusion taken from http://bl.ocks.org/mbostock/1386444
   For now won't run smoothly on most machines/browsers. *)

open Gg
open Vg
open Brr

(* Illusion *)

type ring = { radius : float; speed : float; }
let rings =
  let r = 65. in
  [ { radius = 1. *. r; speed =  30. };
    { radius = 2. *. r; speed =  20. };
    { radius = 3. *. r; speed =  10. };
    { radius = 4. *. r; speed = -10. };
    { radius = 5. *. r; speed = -20. };
    { radius = 6. *. r; speed = -30. }; ]

let sq_width = 16.
let sq_path = P.empty |> P.rect (Box2.v_mid P2.o (Size2.v sq_width sq_width))
let sq_outline = `O { P.o with P.width = 2.5 }
let white_square = I.const Color.white |> I.cut ~area:sq_outline sq_path
let black_square = I.const Color.black |> I.cut ~area:sq_outline sq_path
let background = I.const (Color.gray 0.53)

let ring dt r =
  let rot = Float.rad_of_deg (r.speed *. dt) in
  let white_square = white_square |> I.rot rot in
  let black_square = black_square |> I.rot rot in
  let n = Float.two_pi *. r.radius /. sq_width *. (sqrt 0.5) in
  let k = Float.two_pi /. n in
  let radius = V2.v 0. r.radius in
   let rec squares acc n =
    if n = 0 then acc else
    let sq = if n mod 2 = 0 then white_square else black_square in
    let acc' = acc |> I.blend (sq |> I.move radius |> I.rot ((float n) *. k)) in
    squares acc' (n - 1)
  in
  squares I.void (truncate n) |> I.rot rot

let image ~dt =
  let add_ring acc r = acc |> I.blend (ring dt r) in
  let rings = List.fold_left add_ring I.void rings in
  background |> I.blend rings |> I.scale (V2.v 0.6 0.6)

let size = Size2.v 150. 150.
let view = Box2.v_mid P2.o (Size2.v 550. 550.)

let main () =
  let c = Brr_canvas.Canvas.create [] in
  let r = Vgr.create (Vgr_htmlc.target c) `Other in
  let rec animate now =
    let i = image ~dt:(now /. 1000.) in
    ignore (Vgr.render r (`Image (size, view, i)));
    ignore (G.request_animation_frame animate)
  in
  El.append_children (Document.body G.document) [Brr_canvas.Canvas.to_el c];
  ignore (G.request_animation_frame animate)

let () = main ()
