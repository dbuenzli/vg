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

(** Pie chart illusion. 
    Data taken from here http://en.wikipedia.org/wiki/File:Piecharts.svg *)

Db.image "pie-ambiguity" ~author:Db.dbuenzli
  ~title:"Pie chart ambiguity"
  ~tags:["image"; "illusion"]
  ~note:"Proportions showing that angles are hard to compare \
         visually."
  ~size:(Size2.v 90. 138.)
  ~view:(Box2.v P2.o (Size2.v 1.5 2.3))
  begin fun _ ->
    let pie_chart r colors pcts =
      let rv = V2.v r r in
      let sector (acc, start) color pct =
        let stop = start +. (pct /. 100.) *. Float.two_pi in
        let sector = 
          P.empty >> 
          P.line (V2.polar r start) >> P.earc rv (V2.polar r stop) >> 
          P.line P2.o 
        in
        acc >> I.blend (color >> I.cut sector), stop
      in
      fst (List.fold_left2 sector (I.void, Float.pi_div_2) colors pcts)
    in
    let bar_chart bar_size pad colors pcts = 
      let w, h = V2.to_tuple bar_size in
      let font = Font.create ~weight:`W400 "Open Sans" (h *. 0.015) in
      let mgray = I.const (Color.gray 0.45) in 
      let lgray = I.const (Color.gray 0.75) in
      let bar (acc, x) color pct = 
        let bar = 
          let box = Box2.v P2.o (Size2.v w ((pct /. 100.) *. h)) in
          color >> I.cut (P.empty >> P.rect box) 
        in
        let label =
          let text = Printf.sprintf "%g" pct in
          let pos = P2.v (0.275 *. w) (-1.4 *. (Font.size font)) in 
          mgray >> I.cut_glyphs ~text font [] >> I.move pos
        in
        let x = x +. pad in 
        acc >> I.blend (bar >> I.blend label >> I.move (V2.v x 0.)), x +. w
      in
      let bars, xmax = List.fold_left2 bar (I.void, 0.) colors pcts in
      let floor = 
        let ln = P.empty >> P.sub (P2.v pad 0.) >> P.line (P2.v xmax 0.) in
        lgray >> I.cut ~area:(`O { P.o with P.width = h *. 0.001 }) ln
      in
      bars >> I.blend floor
    in 
    let distribs = [[ 23.; 22.; 20.; 18.; 17.];
                    [ 20.; 20.; 19.; 21.; 20.];
                    [ 17.; 18.; 20.; 22.; 23.]]
    in
    let colors = 
      let min = Float.rad_of_deg 0. in
      let max = Float.rad_of_deg 360. in
      let rec colors count acc i =
        if i < 0 then acc else 
        let h = min +. (float i) *. ((max -. min) /. count) in
        let c = Color.of_laba ~lch:true (V4.v 75. 40. h 1.0) in
        colors count (I.const c :: acc) (i - 1)
      in
      colors 5. [] 4
    in
    let bar_and_pie (acc, y) pcts = 
      let pie = pie_chart 0.25 colors pcts in
      let bars = bar_chart (Size2.v 0.08 2.) 0.04 colors pcts in
      let bp = bars >> I.blend (pie >> I.move (V2.v 1.0 0.25)) in
      acc >> I.blend (bp >> I.move (V2.v 0. y)), y +. 0.75
    in
    let white = I.const Color.white in
    let charts = fst (List.fold_left bar_and_pie (white, 0.) distribs) in
    charts >> I.move (V2.v 0.125 0.15)
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
