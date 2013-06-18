(*---------------------------------------------------------------------------
   Copyright 2013 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

open Gg
open Vg
;;

(** Test images for colors. *)

Db.image "color-ramps" ~author:Db.dbuenzli
  ~title:"Primary and grayscale ramps"
  ~tags:["color"]
  ~note:"From 0 to 1 by 0.1 increments in sRGB space. From right to left \ 
         top to bottom, red, green, blue, gray."
  ~size:(Size2.v 100. 100.)
  ~view:(Box2.v P2.o (Size2.v 2.2 2.2))
  begin fun _ -> 
    let levels = [ 0.0; 0.1; 0.2; 0.3; 0.4; 0.5; 0.6; 0.7; 0.8; 0.9; 1.0 ] in
    let sq = P.empty >> P.rect (Box2.v P2.o (Size2.v 1.1 1.1)) in 
    let bars color = 
      let bar l = I.const (color l) >> I.cut sq >> I.move (P2.v l 0.) in
      let add_bar acc l = acc >> I.blend (bar l) in
      List.fold_left add_bar I.void levels
    in 
    (bars (fun l -> Color.v_srgb l 0. 0.) >> I.move (P2.v 0.0 1.1)) >> I.blend
    (bars (fun l -> Color.v_srgb 0. l 0.) >> I.move (P2.v 1.1 1.1)) >> I.blend
    (bars (fun l -> Color.v_srgb 0. 0. l) >> I.move (P2.v 0.0 0.0)) >> I.blend
    (bars (fun l -> Color.v_srgb l  l  l) >> I.move (P2.v 1.1 0.0))
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
