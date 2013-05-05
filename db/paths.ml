(*---------------------------------------------------------------------------
   Copyright 2013 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

open Gg
open Vg

(** Test images for path areas. *)

let author = "Daniel C. Bünzli <daniel.buenzl i@erratique.ch>";;

Db.image "path-square" ~author
  ~title:"Square area in gray"
  ~tags:["path"; "area-nz"]
  ~size:(Size2.v 50. 50.)
  ~view:(Box2.v P2.o (Size2.v 1. 1.))
  ~note:"Gray indeed."
  begin fun () ->
    let square = P.empty >> P.rect (Box2.v P2.o (P2.v 1. 1.)) in
    I.const (Color.gray 0.3) >> I.cut square
  end;

Db.image "path-square-outline" ~author
  ~title:"Square outline in gray"
  ~tags:["path"; "area-outline"]
  ~note:"Line width is 0.1 as we are on the surface edge."
  ~size:(Size2.v 50. 50.)
  ~view:(Box2.v P2.o (P2.v 1. 1.))
  begin fun () -> 
    let square = P.empty >> P.rect (Box2.v P2.o (Size2.v 1. 1.)) in
    let area = `O { P.o with P.width = 0.2 } in
    I.const (Color.gray 0.3) >> I.cut ~area square 
  end;

Db.image "path-cap-styles" ~author
  ~title:"Lines with different cap styles"
  ~tags:["path"; "area-outline"; "cap-style";]
  ~size:(Size2.v 50. 50.)
  ~view:(Box2.v P2.o (Size2.v 14. 14.))
  ~note:"From top to bottom, `Butt, `Round, `Square."
  begin fun () ->
    let gray = I.const (Color.gray 0.3) in 
    let white = I.const Color.white in 
    let line = P.(empty >> sub (P2.v 3. 0.) >> line (P2.v 11. 0.)) in 
    let line y cap = 
      let outline = I.cut ~area:(`O { P.o with P.width = 2.; cap }) line gray in
      let data = I.cut ~area:(`O { P.o with P.width = 0.1 }) line white in 
      outline >> I.blend data >> I.move (P2.v 0. y)
    in
    (line 3. `Square) >> I.blend (line 7. `Round) >> I.blend (line 11. `Butt)
  end;

Db.image "path-join-styles" ~author
  ~title:"Lines with different join styles" 
  ~tags:["path"; "area-outline"; "join-style";]
  ~size:(Size2.v 50. 100.)
  ~view:(Box2.v P2.o (Size2.v 18. 36.))
  ~note:"From top to bottom `Miter, `Round, `Bevel."
  begin fun () ->
    let gray = I.const (Color.gray 0.3) in 
    let white = I.const Color.white in
    let wedge = 
      P.(empty >> sub (P2.v 3. 0.) >> line (P2.v 9. 8.) >> line (P2.v 15. 0.))
    in
    let path y join = 
      let area = `O { P.o with P.width = 2.; join } in
      let outline = I.cut ~area wedge gray in
      let data = I.cut ~area:(`O { P.o with P.width = 0.1 }) wedge white in 
      outline >> I.blend data >> I.move (P2.v 0. y)
    in
    (path 2. `Bevel) >> I.blend (path 13. `Round) >> I.blend (path 25. `Miter)
  end;

Db.image "path-square-outline-dashed" ~author 
  ~title:"Dashed square outline in gray"
  ~tags:["path"; "area-outline"; "dashes";]
  ~note:"Line width is 0.1 as we are on the surface edge."  
  ~size:(Size2.v 50. 50.)
  ~view:(Box2.v P2.o (Size2.v 1. 1.))
  begin fun () ->
    let square = P.empty >> P.rect (Box2.v P2.o (P2.v 1. 1.)) in
    let area = `O { P.o with P.width = 0.2; dashes = Some (0., [0.05]); } in
    I.const (Color.gray 0.3) >> I.cut ~area square
  end;

Db.image "path-earcs" ~author
  ~title:"Elliptical arcs"
  ~tags:["path"; "ellipse";]
  ~note:"In red, elliptical arc from left point to right point. Top row \
         is ~large:false. Left column is ~cw:false."
  ~size:(Size2.v 75. 45.)
  ~view:(Box2.v P2.o (Size2.v 7.5 4.5))
  begin fun () ->
    let angle = Float.rad_of_deg 0. in
    let r = Size2.v 1.0 0.5 in
    let p0 = P2.v 0. (Size2.h r) in
    let p1 = P2.v (Size2.w r) 0.0 in 
    let square = 
      let o = V2.(neg (v 0.05 0.05)) in
      P.empty >> P.rect (Box2.v o V2.(2. * neg o))
    in
    let mark pt = I.const (Color.gray 0.1) >> I.cut square >> I.move pt in
    let ellipses =
      let area = `O { P.o with P.width = 0.02 } in
      let els = 
        P.empty >> P.ellipse ~angle P2.o r >> P.ellipse ~angle V2.(p0 + p1) r 
      in
      I.const (Color.gray 0.5) >> I.cut ~area els
    in
    let solution x y sol =
      ellipses >> I.blend sol >> I.blend (mark p0) >> I.blend (mark p1) >>
      I.move (P2.v x y)
    in
    let arc ~large ~cw = 
      let a = P.(empty >> sub p0 >> earc ~large ~cw ~angle r  p1) in
      I.const Color.red >> I.cut ~area:(`O { P.o with P.width = 0.02 }) a
    in
    let l, r, t, b = 1.5, 5.0, 3.0, 1.0 in
    (arc ~large:false ~cw:false >> solution l t) >> I.blend
    (arc ~large:false ~cw:true  >> solution r t) >> I.blend
    (arc ~large:true  ~cw:false >> solution l b) >> I.blend 
    (arc ~large:true  ~cw:true  >> solution r b)
  end;

Db.image "path-dashes" ~author
  ~title: "Dash patterns"
  ~tags:["path"; "dashes"]
  ~size:(Size2.v 100. 100.)
  ~view:(Box2.v P2.o (Size2.v 26. 26.))
  begin fun () -> 
    let path = P.empty >> P.sub (P2.v 1. 0.) >> P.line (P2.v 25. 0.) in
    let line y d = 
      let area = `O { P.o with P.dashes = Some d } in
      I.const (Color.gray 0.3) >> I.cut ~area path >> I.move (V2.v 0. y)
    in
    line 25. (0., []) >> 
    I.blend (line 23. (0., [1.])) >> 
    I.blend (line 21. (1., [1.])) >>
    I.blend (line 19. (0., [2.])) >> 
    I.blend (line 17. (1., [2.])) >> 
    I.blend (line 15. (1., [2.; 1.])) >> 
    I.blend (line 13. (6., [3.; 5.])) >> 
    I.blend (line 11. (0., [2.; 3.])) >> 
    I.blend (line 9. (11., [2.; 3.])) >> 
    I.blend (line 7. (0., [2.; 1.; 3.])) >> 
    I.blend (line 5. (0., [2.; 1.; 3.; 2.])) >> 
    I.blend (line 3. (1., [1.; 5.])) >> 
    I.blend (line 1. (0., [5.; 1.]))
  end;

Db.image "path-cantor-dashes" ~author
  ~title:"Cantor set with dashes" 
  ~tags:["path"; "dashes"; "fractal"]
  ~note:"The Cantor set is drawn with dashes to represent its elements. \
         Maximal dash pattern size is a largely undocumented parameter of \
         the renderer backends, the line renderings may quickly become \
         incorrect."
  ~size:(Size2.v 120. 90.)
  ~view:(Box2.v P2.o (Size2.v 1.2 0.9))
  begin fun () ->

    (* Cantor set http://mathworld.wolfram.com/CantorSet.html *)

    let unit = P.empty >> P.line V2.ox in 
    let o = { P.o with P.width = 0.05 } in
    let cantor max =
      let rec split odd acc = function
      | [] -> acc
      | d :: l -> 
          if odd then split false (d :: acc) l else 
          let d' = d /. 3. in 
          split true (d' :: d' :: d' :: acc) l
      in
      let rec loop level i dashes = 
	if level < 0 then i else
        let area = `O { o with P.dashes = Some (0., dashes); } in
        let i' = 
          I.const (Color.gray 0.3) >> I.cut ~area unit >> 
          I.move (P2.v 0. (0.1 *. float level)) >> I.blend i
        in
        loop (level - 1) i' (split false [] dashes)
      in
      loop max I.void [ 1. ]
    in
    cantor 6 >> I.move (P2.v 0.1 0.15)
  end;
  
Db.image "path-derived" ~author
  ~title:"Derived subpath of Vg.P"
  ~size:(Size2.v 100. 100.)
  ~tags:["path";]
  ~view:(Box2.v P2.o (Size2.v 1. 1.)) 
  begin fun () -> 
    let c = P2.v 0.5 0.5 in
    let p = 
      P.empty >> 
      P.ellipse c (V2.v 0.1 0.2) >>
      P.circle c 0.25 >>
      P.rect (Box2.v (P2.v 0.2 0.15) (Size2.v 0.6 0.7)) >>
      P.rrect (Box2.v (P2.v 0.1 0.05) (Size2.v 0.8 0.9)) (Size2.v 0.2 0.1)
    in
    let area = `O { P.o with P.width = 0.01 } in 
    I.const (Color.gray 0.3) >> I.cut ~area p
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
