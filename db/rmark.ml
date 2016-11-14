(*---------------------------------------------------------------------------
   Copyright (c) 2013 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Gg
open Vg
;;

(* Images with random marks. *)

let mark_count = 1500
let note = Printf.sprintf "%d marks." mark_count
let size = Size2.v 120. 60.
let view = Box2.v P2.o (Size2.v 120. 60.)
let tags = ["image"]

let random_marks m =
  let r = Random.State.make [|1557|] in
  let rx = Float.srandom r ~min:6. ~len:108. in
  let ry = Float.srandom r ~min:6. ~len:48.  in
  let rpt () = V2.v (rx ()) (ry ()) in
  let rec rpts n acc = if n = 0 then acc else rpts (n-1) (rpt ():: acc) in
  let mark pt =
    let area = `O { P.o with P.width = 0.25 } in
    (I.const (Color.gray 0.9) >> I.cut m) >> I.blend
    (I.const (Color.gray 0.3) >> I.cut ~area m) >>
    I.move pt
  in
  let nodes = List.map mark (rpts mark_count []) in
  List.fold_left I.blend I.void nodes
;;

Db.image "rmark-dots" ~author:Db.dbuenzli
  ~title:"Random dot mark"
  ~tags ~note ~size ~view
  begin fun _ ->
    random_marks (P.empty >> P.circle P2.o 2.1)
  end;

Db.image "rmark-ticks" ~author:Db.dbuenzli
  ~title:"Random line mark"
  ~tags ~note ~size ~view
  begin fun _ ->
    random_marks (P.empty >> P.line (P2.v 0.5 1.1))
  end;

Db.image "rmark-qcurve" ~author:Db.dbuenzli
  ~title:"Random quadratic mark"
  ~tags ~note ~size ~view
  begin fun _ ->
    random_marks
      (P.empty >> P.qcurve (P2.v 1.0 1.5) (P2.v 1.0 0.0))
  end;

Db.image "rmark-ccurve" ~author:Db.dbuenzli
  ~title:"Random cubic mark"
  ~tags ~note ~size ~view
  begin fun _ ->
    random_marks
      (P.empty >> P.ccurve (P2.v 0.5 1.0) (P2.v 1.0 1.5) (P2.v 1.0 0.0))
  end;



(*---------------------------------------------------------------------------
   Copyright (c) 2013 Daniel C. Bünzli

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
