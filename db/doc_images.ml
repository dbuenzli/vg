(*---------------------------------------------------------------------------
   Copyright 2013 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

open Gg
open Vg

(** Images from the documentation. *)

let author = "Daniel C. Bünzli <daniel.buenzl i@erratique.ch>";;

Db.image "doc-gray-square" ~author
  ~title:"Unit square area in gray"
  ~tags:["doc";]
  ~size:(Size2.v 30. 30.)
  ~view:Box2.unit
  ~note:"Gray indeed."
  begin fun _ ->
    let gray = I.const (Color.gray 0.5) in
    gray >> I.cut (P.empty >> P.rect Box2.unit) (* TODO remove cut *)
  end;

Db.image "doc-gray-circle" ~author
  ~title:"Gray circle centered in the unit square."
  ~tags:["doc";]
  ~size:(Size2.v 30. 30.)
  ~view:Box2.unit
  ~note:"Indeed, gray circle."
  begin fun _ ->
    let circle = P.empty >> P.circle (P2.v 0.5 0.5) 0.4 in 
    let gray = I.const (Color.gray 0.5) in
    let gray_circle = I.cut circle gray in 
    gray_circle
  end;

Db.image "doc-circle-outline" ~author
  ~title:"Black circle outline centered in the unit square."
  ~tags:["doc";]
  ~size:(Size2.v 30. 30.)
  ~view:Box2.unit
  begin fun _ ->
    let circle = P.empty >> P.circle (P2.v 0.5 0.5) 0.4 in 
    let circle_outline = 
      let area = `O { P.o with P.width = 0.04 } in 
      let black = I.const Color.black in 
      I.cut ~area circle black 
    in
    circle_outline
  end;


Db.image "doc-dot" ~author
  ~title:"Outlined gray circle centered in the unit square."
  ~tags:["doc";]
  ~size:(Size2.v 30. 30.)
  ~view:Box2.unit
  begin fun _ ->
    let circle = P.empty >> P.circle (P2.v 0.5 0.5) 0.4 in 
    let area = `O { P.o with P.width = 0.04 } in 
    let gray = I.const (Color.gray 0.5) in
    let black = I.const Color.black in 
    let gray_circle = I.cut circle gray in
    let circle_outline = I.cut ~area circle black in
    let dot = I.blend circle_outline gray_circle in 
    dot
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
