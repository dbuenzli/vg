(*---------------------------------------------------------------------------
   Copyright 2013 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(* Illusion taken from http://bl.ocks.org/mbostock/1386444 
   For now won't run smoothly on most machines/browsers. *)

open Gg
open Vg

(* [animate] is reusable, it hides browser bureaucracy and jsoo *)

let animate ~size ~view next acc = 
  let now () = Js.to_float (jsnew Js.date_now () ## getTime ()) /. 1000. in
  let start = now () in
  let d = Dom_html.window ## document in
  let c = Dom_html.createCanvas d in
  let r = Vgr.create (Vgr_htmlc.target c) `Other in
  let acc = ref acc in
  let rec loop () =
    let i, acc' = next ~start ~now:(now ()) !acc in
    assert (Vgr.render r (`Image (size, view, i)) = `Ok);
    acc := acc';
    Dom_html._requestAnimationFrame (Js.wrap_callback loop)
  in
  let start _ = 
    Dom.appendChild d ## body c;    
    Dom_html._requestAnimationFrame (Js.wrap_callback loop);
    Js._false
  in
  Dom_html.window ## onload <- Dom_html.handler start

(* Illusion *)

type ring = { radius : float; speed : float; }
let rings =
  let r = 65.in
  [ { radius = 1. *. r; speed =  30. };
    { radius = 2. *. r; speed =  20. };
    { radius = 3. *. r; speed =  10. };
    { radius = 4. *. r; speed = -10. };
    { radius = 5. *. r; speed = -20. };
    { radius = 6. *. r; speed = -30. }; ]

let sq_width = 16.
let sq_path = P.empty >> P.rect (Box2.v_mid P2.o (Size2.v sq_width sq_width))
let sq_outline = `O { P.o with P.width = 2.5 }
let white_square = I.const Color.white >> I.cut ~area:sq_outline sq_path
let black_square = I.const Color.black >> I.cut ~area:sq_outline sq_path
let background = I.const (Color.gray 0.53)

let ring dt r =
  let rot = Float.rad_of_deg (r.speed *. dt) in
  let white_square = white_square >> I.rot rot in 
  let black_square = black_square >> I.rot rot in 
  let n = Float.two_pi *. r.radius /. sq_width *. (sqrt 0.5) in 
  let k = Float.two_pi /. n in
  let radius = V2.v 0. r.radius in
   let rec squares acc n = 
    if n = 0 then acc else
    let sq = if n mod 2 = 0 then white_square else black_square in
    let acc' = acc >> I.blend (sq >> I.move radius >> I.rot ((float n) *. k)) in
    squares acc' (n - 1)
  in 
  squares I.void (truncate n) >> I.rot rot

let image ~start ~now () = 
  let dt = now -. start in 
  let add_ring acc r = acc >> I.blend (ring dt r) in
  let rings = List.fold_left add_ring I.void rings in 
  let next = background >> I.blend rings >> I.scale (V2.v 0.6 0.6) in 
  next, ()

let size = Size2.v 150. 150.
let view = Box2.v_mid P2.o (Size2.v 550. 550.)

let () = animate ~size ~view image ()

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
