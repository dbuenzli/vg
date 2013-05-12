(*---------------------------------------------------------------------------
   Copyright 2013 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

open Gg
open Vg
;;

(** Test images for alpha blending. *)

Db.image "alpha-squares" ~author:Db.dbuenzli
  ~title:"Blue, yellow and green squares overlapping"
  ~tags:["alpha"]
  ~size:(Size2.v 60. 60.)
  ~view:(Box2.v (P2.v (-0.1) (-0.1)) (Size2.v 1.2 1.2))
  begin fun _ -> 
    let rr = P.empty >> P.rect (Box2.v (P2.v 0.1 0.3) (Size2.v 0.4 0.6)) in
    let rg = P.empty >> P.rect (Box2.v (P2.v 0.25 0.) (Size2.v 0.4 0.6)) in
    let rb = P.empty >> P.rect (Box2.v (P2.v 0.4 0.4) (Size2.v 0.6 0.4)) in
    let r = I.const (Color.v 1. 0. 0. 0.5) >> I.cut rr in 
    let g = I.const (Color.v 0. 1. 0. 0.5) >> I.cut rg in
    let b = I.const (Color.v 0. 0. 1. 0.5) >> I.cut rb in 
    r >> I.blend g >> I.blend b
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



