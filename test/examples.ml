(*---------------------------------------------------------------------------
   Copyright (c) 2024 The vg programmers. All rights reserved.
   SPDX-License-Identifier: CC0-1.0
  ---------------------------------------------------------------------------*)

open Gg
open Vg

let gray = I.const (Color.gray 0.5)

let svg_of_unit_square i =
  try
    Out_channel.with_open_bin "/tmp/vg-tutorial.svg" @@ fun oc ->
    let size = Size2.v 30. 30. (* mm *) in
    let view = Box2.unit in
    let r = Vgr.create (Vgr_svg.target ()) (`Channel oc) in
    ignore (Vgr.render r (`Image (size, view, i)));
    ignore (Vgr.render r `End);
  with Sys_error e -> prerr_endline e

let () = svg_of_unit_square gray


let circle = P.empty |> P.circle (P2.v 0.5 0.5) 0.4
let gray_circle = I.cut circle gray

let circle_outline =
  let area = `O { P.o with P.width = 0.04 } in
  let blue = I.const (Color.v_srgb 0.000 0.439 0.722) in
  I.cut ~area circle blue

let dot = I.blend circle_outline gray_circle
let dot = gray_circle |> I.blend circle_outline

let scatter_plot pts pt_width =
  let dot =
    let circle = P.empty |> P.circle P2.o (0.5 *. pt_width) in
     I.const (Color.v_srgb 0.000 0.439 0.722) |> I.cut circle
  in
  let mark pt = dot |> I.move pt in
  let blend_mark acc pt = acc |> I.blend (mark pt) in
  List.fold_left blend_mark I.void pts

let subs =
  let p =
    let rel = true in
    P.empty |>
    P.sub (P2.v 0.1 0.5) |>
    P.line (P2.v 0.3 0.5) |>
    P.qcurve ~rel (P2.v 0.2 0.5) (P2.v 0.2 0.0) |>
    P.ccurve ~rel (P2.v 0.0 (-. 0.5)) (P2.v 0.1 (-. 0.5)) (P2.v 0.3 0.0) |>
    P.earc ~rel (Size2.v 0.1 0.2) (P2.v 0.15 0.0) |>
    P.sub (P2.v 0.18 0.26) |>
    P.qcurve ~rel (P2.v (0.01) (-0.1)) (P2.v 0.1 (-. 0.05)) |>
    P.close |>
    P.sub (P2.v 0.65 0.8) |>
    P.line ~rel (P2.v 0.1 (-. 0.05))
  in
  let area = `O { P.o with P.width = 0.01 } in
  I.const (Color.v_srgb 0.000 0.439 0.722) |> I.cut ~area p
