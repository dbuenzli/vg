(*---------------------------------------------------------------------------
   Copyright 2013 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

open Gg
open Vg
;;

(** Test images for glyphs. *)

let open_sans_xbold = 
  { Font.name = "Open Sans"; size = 1.0; weight = `W800; slant = `Normal}

(* Glyphs for the string "Revolt!" as per cmap of Open_sans.extra_bold *)
let glyphs = [ 53; 72; 89; 82; 79; 87; 4 ]
;;
  
Db.image "glyph-revolt" ~author:Db.dbuenzli
  ~title:"Revolt in black"
  ~tags:["glyph"]
  ~note:"Black characters “Revolt!”, approximatively centered \
         in the image."
  ~size:(Size2.v 135. 45.)
  ~view:(Box2.v P2.o (Size2.v 3. 1.))
  begin fun _ -> 
    let font = { open_sans_xbold with Font.size = 0.7 } in
    let text = "Revolt!" in 
    I.const Color.black >> I.cut_glyphs ~text font glyphs >> 
    I.move (V2.v 0.23 0.25)
  end;

Db.image "glyph-revolt-outline" ~author:Db.dbuenzli
  ~title:"Revolt outline in black"
  ~tags:["glyph"]
  ~note:"Black outlined characters “Revolt!”, approximatively centered \
         in the image with bevel path joins."
  ~size:(Size2.v 135. 45.)
  ~view:(Box2.v P2.o (Size2.v 3. 1.))
  begin fun _ -> 
    let font = { open_sans_xbold with Font.size = 0.7 } in
    let area = `O { P.o with P.width = 0.03; join = `Bevel } in
    let text = "Revolt!" in 
    I.const Color.black >> I.cut_glyphs ~area ~text font glyphs >> 
    I.move (V2.v 0.23 0.25)
  end;

Db.image "glyph-aspect" ~author:Db.dbuenzli
  ~title:"Glyph aspect"
  ~tags:["glyph"]
  ~note:"The character should read “R”, without distortion."
  ~size:(Size2.v 25. 50.)
  ~view:(Box2.v P2.o (Size2.v 2. 1.))
  begin fun _ -> 
    let font = { open_sans_xbold with Font.size = 0.5 } in
    let text = "R" in 
    let sq = P.empty >> P.rect (Box2.v (P2.v 0. 0.75) (P2.v 0.25 0.25)) in 
    I.const Color.black >> I.cut sq >>
    I.blend (I.const Color.black >> I.cut_glyphs ~text font [53;]) >> 
    I.scale (V2.v 4.0 1.0)
  end;

Db.image "glyph-multi" ~author:Db.dbuenzli
  ~title:"Multiple revolts"
  ~tags:["glyph"]
  ~note:"Rectangle filled with revolts rotated by 30°."
  ~size:(Size2.v 108. 135.)
  ~view:(Box2.v P2.o (P2.v 0.8 1.0))
  begin fun view -> 
    let font = { open_sans_xbold with Font.size = 0.025 } in
    let text = "Revolt!" in
    let angle = Float.rad_of_deg 30. in
    let revolt pos = 
      I.const Color.black >> I.cut_glyphs ~text font glyphs >> 
      I.move pos
    in
    let next max dv pt = 
      if V2.x pt < V2.x max then Some (V2.v (V2.x pt +. V2.x dv) (V2.y pt)) else
      let y = V2.y pt +. V2.y dv in 
      if y > V2.y max then None else Some (V2.v 0. y)
    in
    let max = V2.v 1.3 1.3 in
    let dv = V2.v 0.11 0.03 in
    let rec blend_revolt acc = function
    | None -> acc 
    | Some pt -> blend_revolt (acc >> I.blend (revolt pt)) (next max dv pt) 
    in
    let margin = 
      let area = `O { P.o with P.width = 0.1 } in
      I.const Color.white >> I.cut ~area (P.empty >> P.rect view)
    in
    blend_revolt (I.const Color.white) (Some P2.o) >> I.rot angle >> 
    I.move (P2.v 0.2 (-. sin (angle))) >>
    I.blend margin
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
