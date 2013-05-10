(*---------------------------------------------------------------------------
   Copyright 2013 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

open Gg
open Vg

(** Test image for colors. *)

let author = "Daniel C. Bünzli <daniel.buenzl i@erratique.ch>"
;;

Db.image "color-rgb-squares" ~author
  ~title:"Red green and blue squares"
  ~tags:["color"]
  ~size:(Size2.v 50. 50.) 
  ~view:(Box2.v P2.o (Size2.v 40. 40.))
  begin fun _ -> 
    let r = P.empty >> P.rect (Box2.v P2.o (Size2.v 20. 20.)) in
    let square ~at c = I.const c >> I.cut r >> I.move at in 
    square ~at:P2.o Color.red >> 
    I.blend (square ~at:(P2.v 20. 0.0) Color.green) >>
    I.blend (square ~at:(P2.v 20. 20.) Color.blue)
  end;


Db.image "color-gray-ramp" ~author
  ~title:"Gray at every 0.1 intensity"
  ~tags:["color"]
  ~size:(Size2.v 50. 50.) 
  ~view:Box2.unit
  begin fun _ -> 
    let r = P.empty >> P.rect (Box2.v P2.o (Size2.v 0.15 1.0)) in 
    let blot ~at c = I.const c >> I.cut r >> I.move at in
    let rec scale i acc =
      if i > 10 then acc else
      let fi = float i in
      let c = Color.gray (fi *. 0.1) in
      let at = P2.v (fi *. 0.1) 0. in
      scale (i + 1) (acc >> I.blend (blot ~at c))
    in
    scale 0 I.void
  end


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
