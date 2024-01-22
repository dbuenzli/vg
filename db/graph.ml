(*---------------------------------------------------------------------------
   Copyright (c) 2013 The vg programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Gg
open Vg
;;

Db.image "graph" __POS__ ~author:Db.dbuenzli
  ~title:"Graph drawing with combinators"
  ~tags:["graph"; "image"; ]
  ~size:(Size2.v 120. 60.)
  ~view:(Box2.v P2.o (Size2.v 120. 60.))
    begin fun _ ->
      let r = Random.State.make [|1557|] in
      let rpt () = P2.v
          (Float.srandom ~min:6. ~len:108. r ())
          (Float.srandom ~min:6. ~len:48.  r ())
      in
      let rec rpts n acc = if n = 0 then acc else rpts (n-1) (rpt ():: acc) in
      let ( ++ ) = I.blend in
      let node_shape = P.empty |> P.circle P2.o 2.0 in
      let node pt =
        let area = `O { P.o with P.width = 0.5 } in
        let i =
          (I.const (Color.gray 0.9) |> I.cut node_shape) ++
          (I.const (Color.gray 0.3) |> I.cut ~area node_shape) |>
          I.move pt
        in
        i, pt
      in
      let nodes = List.map node (rpts 1500 []) in
      List.fold_left (fun acc n -> I.blend acc (fst n)) I.void nodes
    end;
