(*---------------------------------------------------------------------------
   Copyright 2013 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

open Gg
open Vg
;;

(** Café wall illusion. 
    http://mathworld.wolfram.com/CafeWallIllusion.html *)

Db.image "cafe-wall" ~author:Db.dbuenzli
  ~title:"Café Wall Illusion"
  ~tags:["image"; "dashes"; "illusion"]
  ~note:"Also known as Münsterberg illusion. The gray lines are parallel."
  ~size:(Size2.v 115. 65.)
  ~view:(Box2.v P2.o (Size2.v 2.3 1.3))
  begin fun _ -> 
    let line = P.empty >> P.line (P2.v 2. 0.) in
    let border = 
      let area = `O { P.o with P.width = 0.005 } in
      I.const (Color.gray 0.5) >> I.cut ~area line 
    in
    let bricks offset =
      let hwidth = 0.05 in
      let dashes = Some (offset, [0.2]) in
      let area = `O { P.o with P.width = 2. *. hwidth; dashes; } in
      I.const (Color.black) >> I.cut ~area line >> I.move (V2.v 0. hwidth) >>
      I.blend border
    in
    let blend_row acc (y, offset) = 
      acc >> I.blend ((bricks offset) >> I.blend border >> I.move (V2.v 0. y))
    in
    let rows = [0.0, 0.36; 0.1, 0.00; 0.2, 0.36; 0.3, 0.32; 0.4, 0.28; 
                0.5, 0.32; 0.6, 0.36; 0.7, 0.00; 0.8, 0.36; 0.9, 0.32; ] 
    in
    I.const Color.white >> 
    I.blend (List.fold_left blend_row I.void rows) >>
    I.blend (border >> I.move (V2.v 0. 1.)) >> 
    I.move (V2.v 0.15 0.15)
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
