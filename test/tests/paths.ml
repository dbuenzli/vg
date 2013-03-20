(*---------------------------------------------------------------------------
   Copyright %%COPYRIGHT%%. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

open Gg;;
open Vg;;
open Testing;;

test "path-square-area"
~info: "Square area in black."
~size: (Size2.v 20. 20.)
~view: (Box2.v P2.o (P2.v 1. 1.))
& lazy begin 
  let square = P.empty >> P.rect (Box2.v P2.o (P2.v 1. 1.)) in
  let black = I.mono Color.black in
  I.cut `Aeo black square
end;;

test "path-square-outline"
~info: "Square outline in black."
~note: "Line width is 0.1 as we are on the surface edge."
~size: (Size2.v 20. 20.)
~view: (Box2.v P2.o (P2.v 1. 1.))
& lazy begin 
  let r = P.empty >> P.rect (Box2.v P2.o (P2.v 1. 1.)) in
  let black = I.mono Color.black in
  let o = `Ol { I.ol with I.width = 0.2 } in
  I.cut o black r
end;;

test "path-square-dashed-outline"
~info: "Dashed square outline in black."
~note: "Line width is 0.1 as we are on the surface edge."
~size: (Size2.v 20. 20.)
~view: (Box2.v P2.o (P2.v 1. 1.))
& lazy begin
  let r = P.empty >> P.rect (Box2.v P2.o (P2.v 1. 1.)) in
  let black = I.mono Color.black in
  let o = `Ol { I.ol with I.width = 0.2; dashes = Some (0., [0.1]); } in
  I.cut o black r 
end;;

(*

test "path-cap-styles"
~info: "Lines with different cap styles."
~view: (box2_ P2.o (size2_ 18. 18.))
& lazy begin
  let white = I.mono Color.white in
  let line = P.empty >> P.start (p2_ 3. 0.) >> P.line (p2_ 15. 0.) in 
  let line y cap = 
    let outline = I.cut (`Ol {I.ol with I.width = 2.; cap = cap }) black line 
    in
    let data = I.cut (`Ol {I.ol with I.width = 0.1}) white line in 
    I.move (p2_ 0. y) (I.blend data outline)
  in
  (line 14. `Butt) >> I.blend (line 9. `Round) >> I.blend (line 12. `Square)
end;;

test "path-cap-styles"
~info: "Lines with different cap styles."
~view: (_box2 P2.o (_size2 18. 18.))
& lazy begin
  let line = P.empty >> P.start (_p2 3. 0.) >> P.line (_p2 15. 0.) in 
  let line y cap = 
    let outline = I.cut (`Ol {I.ol with I.width = 2.; cap = cap }) black line in
    let data = I.cut (`Ol {I.ol with I.width = 0.1}) white line in 
    I.move (_p2 0. y) (I.blend data outline)
  in
  (line 14. `Butt) >> I.blend (line 9. `Round) >> I.blend (line 12. `Square)
end;;

test "path-cap-styles"
~info: "Lines with different cap styles."
~view: (gbox2 P2.o (gsize2 18. 18.))
& lazy begin
  let line = P.empty >> P.start (gp2 3. 0.) >> P.line (gp2 15. 0.) in 
  let line y cap = 
    let outline = I.cut (`Ol {I.ol with I.width = 2.; cap = cap }) black line in
    let data = I.cut (`Ol {I.ol with I.width = 0.1}) white line in 
    I.move (gp2 0. y) (I.blend data outline)
  in
  (line 14. `Butt) >> I.blend (line 9. `Round) >> I.blend (line 12. `Square)
end;;

test "path-cap-styles"
~info: "Lines with different cap styles."
~view: (Box2.v P2.o (Size2.v 18. 18.))
& lazy begin
  let line = P.empty >> P.start (P2.v 3. 0.) >> P.line (P2.v 15. 0.) in 
  let line y cap = 
    let outline = I.cut (`Ol {I.ol with I.width = 2.; cap = cap }) black line in
    let data = I.cut (`Ol {I.ol with I.width = 0.1}) white line in 
    I.move (V2.v 0. y) (I.blend data outline)
  in
  (line 14. `Butt) >> I.blend (line 9. `Round) >> I.blend (line 12. `Square)
end;;




(*

let () = test
    ~name: "path-join-styles"
    ~description: "Lines with different line join styles."
    ~size: (size 0.06 0.12)
    ~view: (rect Pt.o (size 18. 36.))
    & lazy begin 
      let black = I.cloth (I.mono C.black) in
      let white = I.cloth (I.mono C.white) in
      let path = 
	P.empty >> 
	P.start (pt 3. 0.) >> 
	P.line (pt 9. 8.) >>
	P.line (pt 15. 0.)
      in
      let path join y = 
	let stroke = black & I.cutter_l [`Width 2.; `Join join] & I.ocut path in
	let data = white & I.cutter (`Width 0.1) & I.ocut path in
	I.transl (pt 0. y) & stroke ++ data
      in
      path `Bevel 2. ++ path `Round 13. ++ path `Miter 25.
    end

let () = test
    ~name: "path-dashes"
    ~description: "Lines with different dash patterns."
    ~size: (size 0.12 0.12)
    ~view: (rect Pt.o (size 26. 26.))
    & lazy begin 
      let path = P.empty >> P.start (pt 1. 0.) >> P.line (pt 25. 0.) in
      let line d y = I.transl (pt 0. y) & I.cutter (`Dashes d) & I.ocut path in
      line (0., []) 25. ++
      line (0., [1.]) 23. ++
      line (1., [1.]) 21. ++
      line (0., [2.]) 19. ++
      line (1., [2.]) 17. ++
      line (1., [2.; 1.]) 15. ++
      line (6., [3.; 5.]) 13. ++
      line (0., [2.; 3.]) 11. ++
      line (11., [2.; 3.]) 9. ++
      line (0., [2.; 1.; 3.]) 7. ++
      line (0., [2.; 1.; 3.; 2.]) 5. ++
      line (1., [1.; 5.]) 3. ++
      line (0., [5.; 1.]) 1.
    end

let () = test 
    ~name: "path-arrowhead"
    ~description: "Sierpinsky arrowhead curve, level 7"
    ~size: cm15
    ~view: sq
    & lazy begin 
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
      in
      I.cutter_l [ `Width 0.005; `Cap `Butt; `Join `Round ] & 
      I.ocut (arrowhead_path 7)
    end


let () = test
    ~name: "path-cantor"
    ~description: "Cantor Set, level 6, drawn with dashes"
    ~size: cm15
    ~view: sq
    & lazy begin 
      let cantor i = 
	let h = 1. /. (1. +. 2. *. float_of_int i) in
	let cut_unit = I.ocut (P.empty >> P.line V2.ex) in
	let rec aux i img (_, dashes) = 
	  if i = -1 then img else
	  let dashes' = 
	    let rec even acc = function
	      | d :: l -> let d' = d /. 3. in odd (d' :: d' :: d' :: acc) l
	      | [] -> (0., acc)
	    and odd acc = function
	      | d :: l -> even (d :: acc) l
	      | [] -> (0., acc)
	    in
	    even [] dashes 
	  in
	  let new_line = 
	    I.transl (v2 0. (0.5 *. h +. 2. *. h *. float_of_int i)) &
	    I.cutter (`Dashes dashes') & cut_unit
	  in
	  aux (i - 1) (img ++ new_line) dashes'
	in
      I.cutter (`Width h) & aux i (I.mono C.white) (0., [ 1. ])
      in
      cantor 2
    end


let () = test
  ~name: "path-arcs"
  ~description: "Elliptical arcs of Vg.P"
  ~size: cm15
  ~view: (rect (Pt.neg (pt 1.25 0.5)) (size 4.5 4.5))
  & lazy begin 
    let to_rad a = (a *. pi) /. 180. in
    let r = size 1.0 0.75 in
    let p0 = pt 1. 0.5 in
    let p1 = pt 0. 0. in 
    let angle = to_rad 45. in
    let a ~large ~cw = 
      P.empty >> P.start p0 >> P.earc ~large ~cw r angle p1 >> P.close 
    in
    let arc ~large ~cw = I.cutter (`Width 0.005) 
	(I.cloth (I.mono C.black) (I.ocut (a ~large ~cw)))
    in
    let i x y im = I.transl (v2 x y) im in
    let i1 = i 0. 0. (arc ~large:false ~cw:false) in
    let i2 = i 1. 0. (arc ~large:false ~cw:true) in 
    let i3 = i 0. 2. (arc ~large:true ~cw:false) in 
    let i4 = i 1. 2. (arc ~large:true ~cw:true) in 
    i1 ++ i2 ++ i3 ++ i4
  end

(* TODO degenerate arc test should collapse to a line *)

let () = test 
  ~name: "path-derived-subpaths"
  ~description: "Derived subpath of Vg.P"
  ~size: cm15
  ~view: sq
  & lazy begin 
    let c = pt 0.5 0.5 in
    let p = 
      P.empty >> 
      P.ellipse c (v2 0.1 0.2) >>
      P.circle c 0.25 >>
      P.rect (rect (pt 0.2 0.15) (size 0.6 0.7)) >>
      P.rrect (rect (pt 0.1 0.05) (size 0.8 0.9)) (size 0.1 0.2)
    in
    I.cutter (`Width 0.005) & I.ocut p 
  end
*)


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
