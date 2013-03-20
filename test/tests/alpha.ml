(*---------------------------------------------------------------------------
   Copyright %%COPYRIGHT%%. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

open Gg;;
open Vg;;
open Test;;

(*

let trans_layer () = 
  let r = Path.empty >> Path.rect (rect (pt 0.0 0.3) (size 0.5 0.7)) in
  let r' = Path.empty >> Path.rect (rect (pt 0.25 0.0) (size 0.4 0.6)) in
  let r'' = Path.empty >> Path.rect (rect (pt 0.4 0.4) (size 0.5 0.4)) in
  let blue = Si.mono (color 0. 0. 1. 1.) in
  let yellow = Si.mono (color 1. 1. 0.5 0.5) in
  let green = Si.mono (color 0.5 1. 0.5 0.5) in
  Si.fill ~i:blue r <+>
  Si.fill ~i:yellow r' <+>
  Si.fill ~i:green r'' 

let trans_layer () = 
  let r = Path.empty >> Path.rect (rect (pt 0.0 0.3) (size 0.5 0.7)) in
  let r' = Path.empty >> Path.rect (rect (pt 0.25 0.0) (size 0.4 0.6)) in
  let r'' = Path.empty >> Path.rect (rect (pt 0.4 0.4) (size 0.5 0.4)) in
  let blue = Si.mono (color 0. 0. 1. 1.) in
  let yellow = Si.mono (color 1. 1. 0.5 1.) in
  let green = Si.mono (color 0.5 1. 0.5 1.) in
  Si.fill ~i:blue r <+>
  Si.fill ~i:yellow r' <+>
  Si.fill ~i:green r'' 



let () = Test.register {
  name = "translayer";
  description = "Transparent layer";
  dsize = size 0.12 0.12;
  uview = rect (pt (-0.1) (-0.1)) (size 1.2 1.2);
  img = lazy (trans_layer ()) }

*)  


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




