(*---------------------------------------------------------------------------
   Copyright (c) 2013 The vg programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Gg
open Vg
;;

(** Test images for gradients. *)

Db.image "gradient-axial" __POS__ ~author:Db.dbuenzli
  ~title:"Black to red, red to white, axial gradient"
  ~tags:["gradient"]
  ~size:(Size2.v 60. 20.)
  ~view:(Box2.v P2.o (Size2.v 2. 1.))
  begin fun _ ->
    let r = P.empty |> P.rect (Box2.v P2.o (Size2.v 2. 1.)) in
    let stops = [0.0, Color.black; 0.5, Color.red; 1.0, Color.white] in
    I.cut r (I.axial stops P2.o (P2.v 2. 0.))
  end;

Db.image "gradient-radial" __POS__ ~author:Db.dbuenzli
  ~title:"White to red, red to black, radial gradients"
  ~tags:["gradient"]
  ~note:"Focus is clockwise from top left: at the center, at 45° half way to \
         the circle edge, 45° just inside the circle, 120° half way to the \
         circle edge."
  ~size:(Size2.v 60. 60.)
  ~view:(Box2.v P2.o (Size2.v 2. 2.))
  begin fun _ ->
    let radial x y f  =
      let r = P.empty |> P.rect (Box2.v P2.o (Size2.v 1. 1.)) in
      let stops = [0.0, Color.white; 0.5, Color.red; 1.0, Color.black] in
      let c = P2.v 0.5 0.5 in
      I.cut r (I.radial stops ~f c 0.5) |> I.move (P2.v x y)
    in
    let d45 = Float.pi_div_4 in
    let d120 = 2. *. Float.pi /. 3. in
    let f0 = P2.v 0.5 0.5 in
    let f1 = V2.(f0 + 0.25 * P2.v (cos d45) (sin d45)) in
    let f2 = V2.(f0 + 0.499 * P2.v (cos d45) (sin d45)) in
    let f3 = V2.(f0 + 0.25 * P2.v (cos d120) (sin d120)) in
    radial 0. 1. f0 |>
    I.blend (radial 1. 1. f1) |>
    I.blend (radial 1. 0. f2) |>
    I.blend (radial 0. 0. f3)
  end;

Db.image "gradient-axial-move" __POS__ ~author:Db.dbuenzli
  ~title:"Move axial gradient and outline cut"
  ~tags:["gradient"]
  ~note:"Left, the circle is inscribed in the unit square and the gradient \
         is black at the center. On the right a circle outline is cut but due
         to the parameters used the result should be the same as on the left."
  ~size:(Size2.v 60. 30.)
  ~view:(Box2.v P2.o (Size2.v 2. 1.))
  begin fun _ ->
    let c = P2.v 0.5 0.5 in
    let r = 0.5 in
    let stops = [ 0., Color.red; 0.5, Color.black; 1.0, Color.red ] in
    let axial = I.axial stops (V2.v (-0.5) 0.) (V2.v 0.5 0.) in
    let left =
      let circle = P.empty |> P.circle c r in
      axial |> I.move c |> I.cut circle
    in
    let right =
      let circle' = P.empty |> P.circle c 0.25 in
      let area = `O { P.o with P.width = 0.5 } in
      axial |> I.move c |> I.cut ~area circle' |> I.move (V2.v 1.0 0.)
    in
    I.blend left right
  end;

Db.image "gradient-radial-move" __POS__ ~author:Db.dbuenzli
  ~title:"Move radial gradient and outline cut"
  ~tags:["gradient"]
  ~note:"On the left the circle cut and the radial gradient are inscribed in \
         the unit square. On the right a circle outline is cut but due to the \
         the parameters used the result should be the same as on the left."
  ~size:(Size2.v 60. 30.)
  ~view:(Box2.v P2.o (Size2.v 2. 1.))
  begin fun _ ->
    let c = P2.v 0.5 0.5 in
    let r = 0.5 in
    let stops = [ 0., Color.red; 1.0, Color.black ] in
    let radial = I.radial stops P2.o r in
    let left =
      let circle = P.empty |> P.circle c r in
      radial |> I.move c |> I.cut circle
    in
    let right =
      let circle' = P.empty |> P.circle c 0.25 in
      let area = `O { P.o with P.width = 0.5 } in
      radial |> I.move c |> I.cut ~area circle' |> I.move (V2.v 1.0 0.)
    in
    I.blend left right
  end;

Db.image "gradient-scaling" __POS__ ~author:Db.dbuenzli
  ~title:"Gradients and scaled ones side-by-side"
  ~tags:["gradient"]
  ~note:"In the right column the gradient on the left is scaled by
         (1/2, 1/3)."
  ~size:(Size2.v 60. 60.)
  ~view:(Box2.v (P2.v ~-.0.1 ~-.0.1) (Size2.v 1.2 1.2))
  begin fun _ ->
    let y = Color.v_srgb 1.000 0.827 0.000 in
    let b = Color.v_srgb 0.000 0.529 0.741 in
    let r = Color.v_srgb 0.769 0.008 0.200 in
    let stops = [ 0.0, r; 0.5, b; 1.0, y] in
    let axial = I.axial stops P2.o (P2.v 0.45 0.) in
    let radial = I.radial stops ~f:(P2.v 0.25 0.25) (P2.v 0.5 0.5) 0.5 in
    let scaled i = i |> I.scale (Size2.v 0.5 0.333) in
    let r = P.empty |> P.rect (Box2.v (P2.v 0. 0.) (Size2.v 0.45 0.45)) in
    let square ~at i = i |> I.cut r |> I.move at in
    square ~at:(P2.v 0.0 0.55) axial |>
    I.blend (square ~at:(P2.v 0.55 0.55) (scaled axial)) |>
    I.blend (square ~at:(P2.v 0.0 0.0) radial) |>
    I.blend (square ~at:(P2.v 0.55 0.0) (scaled radial))
  end;


Db.image "gradient-rgb-squares" __POS__ ~author:Db.dbuenzli
  ~title:"Shaded red, green and blue squares"
  ~tags:["gradient"]
  ~size:(Size2.v 50. 50.)
  ~view:(Box2.v P2.o (Size2.v 4. 4.))
  begin fun _ ->
    let w = 2. in
    let r = Box2.v P2.o (Size2.v w w) in
    let p = P.empty |> P.rect r in
    let shade c = I.axial [0., c; 1., Color.void] V2.ox V2.oy in
    let sq ~at c = shade c |> I.scale (Box2.size r) |> I.cut p |> I.move at in
    sq ~at:P2.o Color.red |>
    I.blend (sq ~at:(P2.v w 0.) Color.green) |>
    I.blend (sq ~at:(P2.v w w) Color.blue)
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
