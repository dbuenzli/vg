(*---------------------------------------------------------------------------
   Copyright (c) 2013 The vg programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Gg
open Vg
;;

(** Images for the documentation. *)

Db.image "doc-gray-square" __POS__ ~author:Db.dbuenzli
  ~title:"Unit square area in gray"
  ~tags:["doc";]
  ~size:(Size2.v 30. 30.)
  ~view:Box2.unit
  ~note:"Gray indeed."
  begin fun _ ->
    I.const (Color.gray 0.5)
  end;

Db.image "doc-gray-circle" __POS__ ~author:Db.dbuenzli
  ~title:"Gray circle centered in the unit square"
  ~tags:["doc";]
  ~size:(Size2.v 30. 30.)
  ~view:Box2.unit
  ~note:"Indeed, gray circle."
  begin fun _ ->
    let circle = P.empty |> P.circle (P2.v 0.5 0.5) 0.4 in
    let gray = I.const (Color.gray 0.5) in
    let gray_circle = I.cut circle gray in
    gray_circle
  end;

Db.image "doc-circle-outline" __POS__ ~author:Db.dbuenzli
  ~title:"Blue circle outline centered in the unit square"
  ~tags:["doc";]
  ~size:(Size2.v 30. 30.)
  ~view:Box2.unit
  begin fun _ ->
    let circle = P.empty |> P.circle (P2.v 0.5 0.5) 0.4 in
    let circle_outline =
      let area = `O { P.o with P.width = 0.04 } in
      let blue = I.const (Color.v_srgb 0.000 0.439 0.722) in
      I.cut ~area circle blue
    in
    circle_outline
  end;

Db.image "doc-dot" __POS__ ~author:Db.dbuenzli
  ~title:"Outlined gray circle centered in the unit square"
  ~tags:["doc";]
  ~size:(Size2.v 30. 30.)
  ~view:Box2.unit
  begin fun _ ->
    let circle = P.empty |> P.circle (P2.v 0.5 0.5) 0.4 in
    let area = `O { P.o with P.width = 0.04 } in
    let gray = I.const (Color.gray 0.65) in
    let blue = I.const (Color.v_srgb 0.000 0.439 0.722) in
    let gray_circle = I.cut circle gray in
    let circle_outline = I.cut ~area circle blue in
    let dot = I.blend circle_outline gray_circle in
    dot
  end;

Db.image "doc-scatter-plot" __POS__ ~author:Db.dbuenzli
  ~title:"Scatter plot"
  ~tags:["doc"]
  ~note:"800 points with coordinates on independent normal distributions."
  ~size:(Size2.v 40. 40.)
  ~view:Box2.unit
  begin fun _ ->
    let scatter_plot pts pt_width =
      let dot =
        let circle = P.empty |> P.circle P2.o (0.5 *. pt_width) in
        I.const (Color.v_srgb 0.000 0.439 0.722) |> I.cut circle
      in
      let mark pt = dot |> I.move pt in
      let blend_mark acc pt = acc |> I.blend (mark pt) in
      List.fold_left blend_mark I.void pts
    in
    let normal_pts count =
      let s = Random.State.make [|18278|] in
      let rand = Float.srandom s in
      let normal_pt () =                           (* Box-Muller transform. *)
        let u1 = rand ~len:1.0 () in
        let u2 = rand ~len:1.0 () in
        let z0 = sqrt (-. 2. *. log u1) *. cos (Float.two_pi *. u2) in
        let z1 = sqrt (-. 2. *. log u1) *. sin (Float.two_pi *. u2) in
        P2.v z0 z1
      in
      let acc = ref [] in
      for i = 1 to count do
        acc := V2.((P2.v 0.5 0.5) + 0.125 * normal_pt ()) :: !acc
      done;
      !acc
    in
    scatter_plot (normal_pts 800) 0.01
  end;

Db.image "doc-subpaths" __POS__ ~author:Db.dbuenzli
  ~title:"Subpaths"
  ~tags:["doc"]
  ~note:"Illustrates subpaths construction."
  ~size:(Size2.v 30. 30.)
  ~view:Box2.unit
  begin fun _ ->
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
  end
;;

let directed_pentagram arrow area r =
  let arrow p0 p1 =             (* arrow at the beginning of the line p0p1. *)
    let l = V2.(p1 - p0) in
    let angle = V2.angle l in
    let loc = V2.(p0 + 0.2 * l) in
    I.const Color.black |> I.cut arrow |> I.rot angle |> I.move loc
  in
  let a = Float.pi_div_2 in                     (* points of the pentagram. *)
  let da = Float.two_pi /. 5. in
  let p0 = V2.polar r a in
  let p1 = V2.polar r (a -. 2. *. da) in
  let p2 = V2.polar r (a +. da) in
  let p3 = V2.polar r (a -. da) in
  let p4 = V2.polar r (a +. 2. *. da) in
  let pentagram =          (* http://mathworld.wolfram.com/StarPolygon.html *)
    P.(empty |> sub p0 |> line p1 |> line p2 |> line p3 |> line p4 |> close)
  in
  let lines = `O { P.o with P.width = 0.01 } in
  I.const (Color.gray 0.8) |> I.cut ~area pentagram |>
  I.blend (I.const Color.black |> I.cut ~area:lines pentagram) |>
  I.blend (arrow p0 p1) |> I.blend (arrow p1 p2) |> I.blend (arrow p2 p3) |>
  I.blend (arrow p3 p4) |> I.blend (arrow p4 p0)

let directed_annulus arrow ~rev area r =
  let arrow ?(rev = false) r a =       (* arrow at polar coordinate (r, a). *)
    let angle = a +. (if rev then -. Float.pi_div_2 else Float.pi_div_2) in
    let loc = V2.polar r a in
    I.const Color.black |> I.cut arrow |> I.rot angle |> I.move loc
  in
  let arrows ?(rev = false) r =
    arrow ~rev r 0. |>
    I.blend (arrow ~rev r (Float.pi_div_2)) |>
    I.blend (arrow ~rev r (2. *. Float.pi_div_2)) |>
    I.blend (arrow ~rev r (-. Float.pi_div_2))
  in
  let circle ?(rev = false) r =
    let c = P.empty |> P.circle P2.o r in
    if rev then (* flip *) P.tr (M3.scale2 (V2.v (- 1.) 1.)) c else c
  in
  let outer = r in
  let inner = r *. 0.6 in
  let annulus = P.append (circle outer) (circle ~rev inner) in
  let outline = `O { P.o with P.width = 0.01 } in
  I.const (Color.gray 0.8) |> I.cut ~area annulus |>
  I.blend (I.const Color.black |> I.cut ~area:outline annulus) |>
  I.blend (arrows outer) |>
  I.blend (arrows ~rev inner)

let area_rule_examples area =
  let arrow =
    let a = Float.two_pi /. 3. in
    let pt a = V2.polar 0.032 a in
    P.(empty |> sub (pt 0.) |> line (pt (-. a)) |> line (pt a) |> close)
  in
  let pentagram = directed_pentagram arrow area 0.4 in
  let annulus   = directed_annulus arrow ~rev:false area 0.3 in
  let annulus_r = directed_annulus arrow ~rev:true  area 0.3 in
  let y = 0.46 in
  I.const Color.white |>
  I.blend pentagram |> I.move (V2.v 0.5 y) |>
  I.blend (annulus   |> I.move (V2.v 1.5 y)) |>
  I.blend (annulus_r |> I.move (V2.v 2.5 y))
;;

Db.image "doc-anz" __POS__ ~author:Db.dbuenzli
  ~title:"Non-zero winding area rule"
  ~tags:["doc"]
  ~note:"Illustrates the non-zero winding area rule."
  ~size:(Size2.v 90. 30.)
  ~view:(Box2.v P2.o (Size2.v 3.0 1.0))
  begin fun _ -> area_rule_examples `Anz end;

Db.image "doc-aeo" __POS__ ~author:Db.dbuenzli
  ~title:"Even-odd winding area rule"
  ~tags:["doc"]
  ~note:"Illustrates the even-odd winding area rule."
  ~size:(Size2.v 90. 30.)
  ~view:(Box2.v P2.o (Size2.v 3.0 1.0))
  begin fun _ ->
    area_rule_examples `Aeo
  end;

Db.image "doc-caps" __POS__ ~author:Db.dbuenzli
  ~title:"Path caps"
  ~tags:["doc"]
  ~note:"Illustrates path cap styles. From left to right: \
         `Butt, `Round and `Square."
  ~size:(Size2.v 90. 20.)
  ~view:(Box2.v P2.o (Size2.v 3.0 (3.0 /. 4.5)))
  begin fun _ ->
    let gray = I.const (Color.gray 0.3) in
    let white = I.const Color.white in
    let line = P.(empty |> sub (P2.v 0.25 0.333) |> line (P2.v 0.75 0.333)) in
    let line x cap =
      let outline = I.cut ~area:(`O { P.o with P.width = 0.2; cap}) line gray in
      let data = I.cut ~area:(`O { P.o with P.width = 0.01 }) line white in
      outline |> I.blend data |> I.move (P2.v x 0.)
    in
    (line 0. `Butt) |> I.blend (line 1.0 `Round) |> I.blend (line 2. `Square)
  end;

Db.image "doc-joins" __POS__ ~author:Db.dbuenzli
  ~title:"Path joins"
  ~tags:["doc"]
  ~note:"Illustrates path join styles. From left to right: \
         `Miter, `Round and `Bevel."
  ~size:(Size2.v 90. 30.)
  ~view:(Box2.v P2.o (Size2.v 3.0 1.0))
  begin fun _ ->
    let gray = I.const (Color.gray 0.3) in
    let white = I.const Color.white in
    let wedge =
      P.empty |>
      P.sub (P2.v 0.2 0.) |> P.line (P2.v 0.5 0.5) |> P.line (P2.v 0.8 0.)
    in
    let path x join =
      let area = (`O { P.o with P.width = 0.2; join }) in
      let outline = I.cut ~area wedge gray in
      let data = I.cut ~area:(`O { P.o with P.width = 0.01 }) wedge white in
      outline |> I.blend data |> I.move (P2.v x 0.2)
    in
    (path 0. `Miter) |> I.blend (path 1. `Round) |> I.blend (path 2. `Bevel)
  end;

Db.image "doc-earcs" __POS__ ~author:Db.dbuenzli
  ~title:"Elliptical arcs"
  ~tags:["doc"; "path"]
  ~note:"Illustrates elliptical arc parameters. In red, elliptical arc \
         from left point to right point. Top row is ~large:false. \
         Left column is ~cw:false."
  ~size:(Size2.v 75. 45.)
  ~view:(Box2.v P2.o (Size2.v 7.5 4.5))
  begin fun _ ->
    let angle = Float.rad_of_deg 0. in
    let r = Size2.v 1.0 0.5 in
    let p0 = P2.v 0. (Size2.h r) in
    let p1 = P2.v (Size2.w r) 0.0 in
    let square = P.empty |> P.rect (Box2.v_mid P2.o (Size2.v 0.1 0.1)) in
    let mark pt = I.const (Color.gray 0.1) |> I.cut square |> I.move pt in
    let ellipses =
      let area = `O { P.o with P.width = 0.02 } in
      let els =
        P.empty |> P.ellipse ~angle P2.o r |> P.ellipse ~angle V2.(p0 + p1) r
      in
      I.const (Color.gray 0.5) |> I.cut ~area els
    in
    let solution x y sol =
      ellipses |> I.blend sol |> I.blend (mark p0) |> I.blend (mark p1) |>
      I.move (P2.v x y)
    in
    let arc ~large ~cw =
      let a = P.(empty |> sub p0 |> earc ~large ~cw ~angle r  p1) in
      I.const Color.red |> I.cut ~area:(`O { P.o with P.width = 0.02 }) a
    in
    let l, r, t, b = 1.5, 5.0, 3.0, 1.0 in
    (arc ~large:false ~cw:false |> solution l t) |> I.blend
    (arc ~large:false ~cw:true  |> solution r t) |> I.blend
    (arc ~large:true  ~cw:false |> solution l b) |> I.blend
    (arc ~large:true  ~cw:true  |> solution r b)
  end;
