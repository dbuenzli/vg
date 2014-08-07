(*---------------------------------------------------------------------------
   Copyright 2013 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

open Gg
open Vg

(** Test images for non-primitive cuts *)

let annulus =
  let circle r = P.empty >> P.circle (P2.v 0.5 0.5) r in
  let outer = 0.3 in
  let inner = outer *. 0.6 in
  P.append (circle outer) (circle inner)

let unit_dots count dot_width =
  let s = Random.State.make [|1922278|] in
  let rand = Float.srandom s in
  let dot =
    let circle = P.empty >> P.circle P2.o (0.5 *. dot_width) in
    I.const Color.black >> I.cut circle
  in
  let acc = ref I.void in
  for i = 1 to count do
    let x = rand ~min:0.2 ~len:0.6 () in
    let y = rand ~min:0.2 ~len:0.6 () in
    acc := I.blend !acc (dot >> I.move (P2.v x y))
  done;
  !acc

let dotted_region area =
  let outline = `O { P.o with P.width = 0.001 } in
  I.cut ~area annulus (unit_dots 800 0.01) >>
  I.blend (I.cut ~area:outline annulus (I.const (Color.gray 0.75)))
;;

Db.image "npcut-aeo" ~author:Db.dbuenzli
  ~title:"Even-odd area, non primitive cut"
  ~tags:["cut"]
  ~note:"Ring with dots."
  ~size:(Size2.v 60. 60.)
  ~view:Box2.unit
  begin fun _ -> dotted_region `Aeo end;

Db.image "npcut-anz" ~author:Db.dbuenzli
  ~title:"Non-zero winding area, non primitive cut"
  ~tags:["cut"]
  ~note:"Circle with dots."
  ~size:(Size2.v 60. 60.)
  ~view:Box2.unit
  begin fun _ -> dotted_region `Anz end;

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
