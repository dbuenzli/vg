(*---------------------------------------------------------------------------
   Copyright 2013 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

open Gg
open Vg

(** Test images for gradients. *)

let author = "Daniel C. Bünzli <daniel.buenzl i@erratique.ch>"
;;

Db.image "gradient-axial" ~author 
  ~title:"Black to red, red to white, axial gradient"
  ~tags:["gradient"]
  ~size:(Size2.v 60. 20.)
  ~view:(Box2.v P2.o (Size2.v 2. 1.))
  begin fun _ -> 
    let r = P.empty >> P.rect (Box2.v P2.o (Size2.v 2. 1.)) in 
    let stops = [0.0, Color.black; 0.5, Color.red; 1.0, Color.white] in 
    I.cut r (I.axial stops P2.o (P2.v 2. 0.))
  end;

Db.image "gradient-scaling" ~author
  ~title:"Gradients and scaled ones side-by-side"
  ~tags:["gradient"]
  ~note:"In the right column the gradient on the left is scaled by 
         (1/2, 1/3)."
  ~size:(Size2.v 60. 60.)
  ~view:(Box2.v (P2.v ~-.0.1 ~-.0.1) (Size2.v 1.2 1.2))
  begin fun _ -> 
    let r = P.empty >> P.rect (Box2.v (P2.v 0. 0.) (Size2.v 0.45 0.45)) in
    let stops = [ 0.0, Color.red; 0.5, Color.green; 1.0, Color.blue ] in
    let axial = I.axial stops P2.o (P2.v 0.45 0.) in
    let radial = I.radial stops ~f:(P2.v 0.25 0.25) (P2.v 0.5 0.5) 0.5 in
    let scaled i = i >> I.scale (Size2.v 0.5 0.333) in
    let square ~at i = i >> I.cut r >> I.move at in
    square ~at:(P2.v 0.0 0.55) axial >> 
    I.blend (square ~at:(P2.v 0.55 0.55) (scaled axial)) >> 
    I.blend (square ~at:(P2.v 0.0 0.0) radial) >> 
    I.blend (square ~at:(P2.v 0.55 0.0) (scaled radial))
  end;

Db.image "gradient-rgb-squares" ~author
  ~title:"Shaded red, green and blue squares"
  ~tags:["gradient"]
  ~size:(Size2.v 50. 50.)
  ~view:(Box2.v P2.o (Size2.v 4. 4.))
  begin fun _ -> 
    let w = 2. in
    let r = Box2.v P2.o (Size2.v w w) in
    let p = P.empty >> P.rect r in
    let shade c = I.axial [0., c; 1., Color.void] V2.ox V2.oy in
    let sq ~at c = shade c >> I.scale (Box2.size r) >> I.cut p >> I.move at in
    sq ~at:P2.o Color.red >> 
    I.blend (sq ~at:(P2.v w 0.) Color.green) >>
    I.blend (sq ~at:(P2.v w w) Color.blue)
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
