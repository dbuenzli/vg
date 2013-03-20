(*---------------------------------------------------------------------------
   Copyright %%COPYRIGHT%%. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

open Gg;;
open Vg;;

let str = Printf.sprintf 

let arrowhead_path i = 
  let size_factor = 1. /. (1. +. 2. *. cos pi_div_3) in
  let rec aux i flip angle size p = 
    if i = 0 then 
      p >> P.line ~rel:true (v2 (size *. cos angle) (size *. sin angle))
    else
      let delta = if flip then -. pi_div_3 else pi_div_3 in 
      let nflip = not flip in 
            let size' = size *. size_factor in 
            let i' = i - 1 in
            p >>
            aux i' nflip (angle +. delta) size' >>
            aux i' flip angle size' >>
            aux i' nflip (angle -. delta) size'
  in
  aux i false 0. 1. P.empty 


(* let p = arrowhead_path 8 *)

let osize = size 10. 10.

let p = P.empty >> 
  P.start (pt 3. (-1.)) >> P.ccurve (pt (-.2.) (-. 1.5)) (pt 1.5 (-.0.5)) (pt 2. 3.)

let p' = P.empty >> P.start (pt 5. 5.) >>
  P.earc ~large:false ~cw:true (v2 1. 2.) 0. (pt 4. 6.)

let rectp r = P.empty >> P.rect r
let frame_path ?control p = 
  let pframe = P.bounds ?control p in 
  I.cloth (I.mono C.b) (I.ocut (rectp pframe)) ++
    I.cloth (I.mono C.black) (I.ocut p)
  
let i = 
  let frame = I.cloth (I.mono C.r) (I.ocut (rectp (rect V2.o osize))) in
  let contents = 
    frame_path ~control:false p ++ frame_path ~control:false p' 
  in
  I.cutter (`Width 0.03) (frame ++ contents)


let () = 
  let view = Rect.inset (rect V2.o osize) (Pt.neg (V2.smul 0.1 osize)) in
  let dest = V2.smul 0.01 (Rect.size view) in 
  let oc = open_out_bin "/tmp/bla.pdf" in
  Vgo.output_pdf ~title:"linearize" (Vgo.output_of_channel oc)
    [ (dest, view, i) ]


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

   3. Neither the name of Daniel C. BÃ¼nzli nor the names of
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





