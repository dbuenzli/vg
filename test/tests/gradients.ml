(*---------------------------------------------------------------------------
   Copyright %%COPYRIGHT%%. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

open Gg;;
open Vg;;

let suite = Suite.create () 
let test = Suite.test suite 

let () = test
    ~name: "gradient-scaling"
    ~description: "Gradient scaling test."
    ~size: (size 0.24 0.24)
    ~view: (rect (pt (-0.1) (-0.1)) (size 1.2 1.2))
    & lazy begin 
      let square = P.empty >> P.rect (Box2.v (P2.v 0. 0.) (Size2.v 1. 1.)) in
      let stops = [ 0.0, Color.red; 0.5, Color.green; 1.0, C.blue ] in
      let axial = I.axial V2.o V2.ox stops in
      let radial = I.radial ~f:(P2.v 0.25 0.25) (P2.v 0.5 0.5) 0.5 stops in
      let scaled i = i >> I.scale (Size2.v 0.5 1.0) in
      let gradient_scale = size 0.5 1. in
      let framed x y i = 
        I.cut square i >> I.scale (Size2.v 0.4 0.4) >> I.move (P2.v x y)
      in
      framed 0.0 0.0 radial 
      I.blend (framed 0.0 0.5 (scaled radial)) >>
      I.blend (framed 0.5 0.0 axial) >> 
      I.blend (framed 0.5 0.5 (scaled axial))
  end


let () = test
    ~name: "gradient-rgb-squares"
    ~description: "Shaded red, green and blue squares."
    ~size: (size 0.1 0.1)
    ~view: (rect V2.o (size 40. 40.))
    & lazy begin 
      let shade c = 
	let stops = [ (0., c); (1., C.blacka 0.) ] in
	I.axial (pt 0. 1.) (pt 1. 0.) stops 
      in
      let rect = P.empty >> P.rect (rect V2.o (size 20. 20.)) in 
      let sq pt col = 
	I.transl pt & 
	I.cloth (I.scale (size 20. 20.) & shade col) &
	I.acut rect
      in
      sq Pt.o C.r ++ sq (pt 20. 0.) C.g ++ sq (pt 20. 20.) C.b
    end

let () = test
    ~name: "gradient-rgb-squares-2"
    ~description: "Shaded red, green and blue squares."
    ~size: (size 0.1 0.1)
    ~view: (rect V2.o (size 20. 20.))
    & lazy begin 
      let shade c = 
	let stops = [ (0., c); (1., C.blacka 0.) ] in
	I.axial (pt 0. 1.) (pt 1. 0.) stops 
      in
      let rect = P.empty >> P.rect (rect V2.o (size 1. 1.)) in 
      let sq pt col = I.transl pt & I.cloth (shade col) & I.acut rect in
      sq Pt.o C.r ++ sq (pt 1. 0.) C.g ++ sq (pt 1. 1.) C.b
    end


(*---------------------------------------------------------------------------
   Copyright %%COPYRIGHT%%
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







