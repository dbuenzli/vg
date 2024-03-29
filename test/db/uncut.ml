(*---------------------------------------------------------------------------
   Copyright (c) 2013 The vg programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Gg
open Vg

(** Test images for uncut image primitives. *)

let emerald = Color.v_srgb 0.314 0.784 0.471
;;

Db.image "uncut-const" __POS__ ~author:Db.dbuenzli
  ~title:"Uncut constant emerald image"
  ~tags:["uncut"]
  ~note:"Constant emerald color over the rectangle."
  ~size:(Size2.v 120. 60.)
  ~view:(Box2.v P2.o (P2.v 2. 1.))
  begin fun _ ->
    I.const emerald
  end;

Db.image "uncut-const-tr" __POS__ ~author:Db.dbuenzli
  ~title:"Uncut transformed constant image"
  ~tags:["uncut"]
  ~note:"Constant emerald color over the rectangle."
  ~size:(Size2.v 120. 60.)
  ~view:(Box2.v P2.o (P2.v 2. 1.))
  begin fun _ ->
    I.const emerald |>
    I.move (V2.v 1. 1.) |> I.scale (V2.v 3. 4.) |> I.blend I.void |>
    I.move (V2.v 0.25 0.0)
  end;

Db.image "uncut-axial" __POS__ ~author:Db.dbuenzli
  ~title:"Uncut axial gradient image"
  ~tags:["uncut"; "gradient"]
  ~note:"From left to right: black to emerald axial gradient."
  ~size:(Size2.v 120. 60.)
  ~view:(Box2.v P2.o (Size2.v 2. 1.))
  begin fun _ ->
    let stops = [0.0, Color.black; 1.0, emerald] in
    I.axial stops P2.o (P2.v 2. 0.)
  end;

Db.image "uncut-axial-tr" __POS__ ~author:Db.dbuenzli
  ~title:"Uncut transformed axial gradient image"
  ~tags:["uncut"; "gradient"]
  ~note:"From left to right: black to emerald axial gradient."
  ~size:(Size2.v 120. 60.)
  ~view:(Box2.v P2.o (Size2.v 2. 1.))
  begin fun _ ->
    let stops = [0.0, Color.black; 1.0, emerald] in
    I.axial stops (P2.v (-1.) (0.)) (P2.v 3. 0.) |>
    I.move (V2.v 0.5 0.0) |> I.scale (V2.v 0.5 1.0) |> I.blend I.void |>
    I.move (V2.v 0.25 0.0)
  end;

Db.image "uncut-radial" __POS__ ~author:Db.dbuenzli
  ~title:"Uncut radial gradient image"
  ~tags:["uncut"; "gradient"]
  ~note:"Centered, from inwards to outwards: black to emerald radial gradient."
  ~size:(Size2.v 120. 60.)
  ~view:(Box2.v P2.o (Size2.v 2. 1.))
  begin fun _ ->
    let stops = [0.0, Color.black; 1.0, emerald] in
    I.radial stops (P2.v 1.0 0.5) 1.0
  end;

Db.image "uncut-radial-tr" __POS__ ~author:Db.dbuenzli
  ~title:"Uncut transformed radial gradient image"
  ~tags:["uncut"; "gradient"]
  ~note:"Centered, from inwards to outwards: black to emerald radial gradient."
  ~size:(Size2.v 120. 60.)
  ~view:(Box2.v P2.o (Size2.v 2. 1.))
  begin fun _ ->
    let stops = [0.0, Color.black; 1.0, emerald] in
    I.radial stops P2.o 2.0 |>
    I.move (V2.v 0.5 0.5) |> I.scale (V2.v 0.5 0.5) |> I.blend I.void |>
    I.move (V2.v 0.75 0.25)
  end;
