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
    ~name: "picts-clock"
    ~description: "Clock frame."
    ~size: (Size2.v 0.1 0.1)
    ~view: (Box2.v P2.o (Size2.v 1. 1.))
    & lazy begin 
      let center = P2.v 0.5 0.5 in
      let radius = 0.4 in
      let width = 0.02 in
      let circle = P.empty >> P.circle center radius in
      let hour_tick h = 
	let dir = 
	  let a = (float_of_int h) *. (pi /. 6.) in
	  V2.v (cos a) (sin a)
	in    
	let inset, width = (* quarters are thicker *)
	  if h mod 3 = 0 then -0.15 *. radius, width else 
	  -0.1 *. radius, 0.75 *. width 
	in
	let path = 	  
	  P.empty >> 
	  P.start (V2.add center V2.(radius * dir)) >>
	  P.line ~rel:true (V2.smul inset dir)
	in
	I.cutter (`Width width) & I.ocut path 
      in
      let rec ticks h i = 
	if h = 12 then i else ticks (succ h) (i ++ hour_tick h)
      in
      I.cutter_l [ `Width width; `Cap `Round ] & ticks 0 (I.ocut circle)
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
