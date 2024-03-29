(*---------------------------------------------------------------------------
   Copyright (c) 2013 The vg programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Gg
open Vg
;;

(** Sierpiński Arrowhead curve
    http://mathworld.wolfram.com/SierpinskiArrowheadCurve.html *)

Db.image "arrowhead" __POS__ ~author:Db.dbuenzli
  ~title:"Sierpiński Arrowhead curve levels 0-9"
  ~tags:["fractal"; "image"]
  ~note:(Printf.sprintf "Last curve made of %g segments" (3. ** (float 9)))
  ~size:(Size2.v 120. 255.)
  ~view:(Box2.v P2.o (Size2.v 2. 4.25))
  begin fun _ ->
    let arrowhead_path i len =
      let angle = Float.pi /. 3. in
      let rec loop i len sign turn p =
        if i = 0 then p |> P.line ~rel:true V2.(polar len turn) else
        p |>
        loop (i - 1) (len /. 2.) (-. sign) (turn +. sign *. angle) |>
        loop (i - 1) (len /. 2.) sign turn |>
        loop (i - 1) (len /. 2.) (-. sign) (turn -. sign *. angle)
      in
      P.empty |> loop i len 1. 0.
    in
    let area = `O { P.o with P.width = 0.005 } in
    let gray = I.const (Color.gray 0.2) in
    let acc = ref I.void in
    for i = 0 to 9 do
      let x = float (i mod 2) +. 0.1 in
      let y = 0.85 *. float (i / 2) +. 0.1 in
      acc :=
        gray |> I.cut ~area (arrowhead_path i 0.8) |> I.move (V2.v x y) |>
        I.blend !acc
    done;
    !acc
  end;
