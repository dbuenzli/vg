(*---------------------------------------------------------------------------
   Copyright 2013 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

open Gg
open Vg


let author = "Daniel C. Bünzli <daniel.buenzl i@erratique.ch>"
type node = Vg.image * Gg.p2
;;

Db.image "graph" ~author 
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
      let node_shape = P.empty >> P.circle P2.o 2.0 in
      let node pt = 
        let area = `O { P.o with P.width = 0.5 } in
        let i = 
          (I.const (Color.gray 0.9) >> I.cut node_shape) ++ 
          (I.const (Color.gray 0.3) >> I.cut ~area node_shape) >>
          I.move pt
        in
        i, pt
      in
      let nodes = List.map node (rpts 1500 []) in 
      List.fold_left (fun acc n -> I.blend acc (fst n)) I.void nodes
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
