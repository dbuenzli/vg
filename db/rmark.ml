(*---------------------------------------------------------------------------
   Copyright 2013 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
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
   Copyright 2013 Daniel C. Bünzli.
   All rights reserved.

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions
   are met:

   1. Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

   2. Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

   3. Neither the name of Daniel C. Bünzli nor the names of
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
  ---------------------------------------------------------------------------*)
