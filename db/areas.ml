(*---------------------------------------------------------------------------
   Copyright 2013 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

open Gg
open Vg

(** Test images for path areas. *)

let author = "Daniel C. Bünzli <daniel.buenzl i@erratique.ch>"
;;

Db.image "area-square" ~author
  ~title:"Square area in red"
  ~tags:["area"; "area-nz"]
  ~size:(Size2.v 50. 50.)
  ~view:(Box2.v P2.o (Size2.v 1. 1.))
  begin fun () ->
    let sq = P.empty >> P.rect (Box2.v P2.o (P2.v 1. 1.)) in
    I.const Color.red >> I.cut sq
  end;

Db.image "area-square-outline" ~author
  ~title:"Square outline in red"
  ~tags:["area"; "area-outline"]
  ~note:"Line width is 0.1 as we are on the surface edge"
  ~size:(Size2.v 50. 50.)
  ~view:(Box2.v P2.o (P2.v 1. 1.))
  begin fun () -> 
    let sq = P.empty >> P.rect (Box2.v P2.o (Size2.v 1. 1.)) in
    let area = `O { P.o with P.width = 0.2 } in
    I.const Color.red >> I.cut ~area sq 
  end;

Db.image "area-square-outline-dashed" ~author 
  ~title:"Dashed square outline in red"
  ~tags:["area"; "area-outline"; "dashes";]
  ~note:"Line width is 0.1 as we are on the surface edge."  
  ~size:(Size2.v 50. 50.)
  ~view:(Box2.v P2.o (Size2.v 1. 1.))
  begin fun () -> 
    let sq = P.empty >> P.rect (Box2.v P2.o (P2.v 1. 1.)) in
    let area = `O { P.o with P.width = 0.2; dashes = Some (0., [0.1]); } in
    I.const Color.red >> I.cut ~area sq 
  end;

Db.image "path-cap-styles" ~author
  ~title:"Lines with different cap styles"
  ~tags:["area"; "area-outline"; "cap-style";]
  ~size:(Size2.v 60. 60.)
  ~view:(Box2.v P2.o (Size2.v 14. 14.))
  ~note:"From top to bottom, `Butt, `Round, `Square."
  begin fun () ->
    let gray = I.const (Color.gray 0.2) in 
    let white = I.const Color.white in 
    let line = P.empty >> P.sub (P2.v 3. 0.) >> P.line (P2.v 11. 0.) in 
    let line y cap = 
      let outline = I.cut ~area:(`O { P.o with P.width = 2.; cap }) line gray in
      let data = I.cut ~area:(`O { P.o with P.width = 0.1 }) line white in 
      outline >> I.blend data >> I.move (P2.v 0. y)
    in
    (line 11. `Butt) >> I.blend (line 7. `Round) >> I.blend (line 3. `Square)
  end;

Db.image "page-join-styles" ~author
  ~title:"Lines with different join styles" 
  ~tags:["area"; "area-outline"; "join-style";]
  ~size:(Size2.v 60. 120.)
  ~view:(Box2.v P2.o (Size2.v 18. 36.))
  ~note:"From top to bottom `Miter, `Round, `Bevel."
  begin fun () ->
    let gray = I.const (Color.gray 0.2) in 
    let white = I.const Color.white in
    let path = P.empty >> P.sub (P2.v 3. 0.) >> P.line (P2.v 9. 8.) >> 
               P.line (P2.v 15. 0.)
    in
    let path y join = 
      let outline = I.cut ~area:(`O{ P.o with P.width = 2.; join }) path gray in
      let data = I.cut ~area:(`O { P.o with P.width = 0.1 }) path white in 
      outline >> I.blend data >> I.move (P2.v 0. y)
    in
    (path 2. `Bevel) >> I.blend (path 13. `Round) >> I.blend (path 25. `Miter)
  end;

(*
(*

let () = test
    ~name: "path-dashes"
    ~description: "Lines with different dash patterns"
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
    ~name: "path-cantor"
    ~description: "Cantor Set, level 6, drawn with dashes"
    ~size: cm15
    ~note:"This test strokes horizontal lines from 0 to 1, using dashes to
represent Cantor's set gaps. There seem to be quite strong
(undocumented) limitations in pdf readers about the size of a dash
array. The only library that seems to be able to handle this is MacOS
X native pdf support."
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
