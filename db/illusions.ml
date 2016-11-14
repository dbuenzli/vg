(*---------------------------------------------------------------------------
   Copyright (c) 2013 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
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
      let font =
        { Font.name = "Open Sans"; slant = `Normal; weight = `W400;
          size = (h *. 0.015) }
      in
      let mgray = I.const (Color.gray 0.3) in
      let lgray = I.const (Color.gray 0.75) in
      let bar (acc, x) color pct =
        let bar =
          let box = Box2.v P2.o (Size2.v w ((pct /. 100.) *. h)) in
          color >> I.cut (P.empty >> P.rect box)
        in
        let label =
          let text = Printf.sprintf "%g" pct in
          let pos = P2.v (0.275 *. w) (-1.4 *. font.Font.size) in
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
    let colors =                   (* Brewer's Set2, http://colorbrewer.org/ *)
      let c r g b = I.const (Color.v_srgbi r g b) in
      [c 102 194 165; c 252 141 98; c 141 160 203; c 231 138 195; c 166 216 84]
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
   Copyright (c) 2013 Daniel C. Bünzli

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
