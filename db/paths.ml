(*---------------------------------------------------------------------------
   Copyright 2013 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

open Gg
open Vg
;;

(** Test images for path areas. *)

Db.image "path-sq-outline" ~author:Db.dbuenzli
  ~title:"Square outline in gray"
  ~tags:["path"; "area" ]
  ~note:"Line width is 0.1 as we are on the surface edge."
  ~size:(Size2.v 50. 50.)
  ~view:Box2.unit
  begin fun _ -> 
    let square = P.empty >> P.rect (Box2.v P2.o (Size2.v 1. 1.)) in
    let area = `O { P.o with P.width = 0.2 } in
    I.const (Color.gray 0.3) >> I.cut ~area square 
  end;

Db.image "path-sq-outline-dashed" ~author:Db.dbuenzli 
  ~title:"Dashed square outline in gray"
  ~tags:["path"; "area"; "dashes";]
  ~note:"Line width is 0.1 as we are on the surface edge."  
  ~size:(Size2.v 50. 50.)
  ~view:Box2.unit
  begin fun _ ->
    let square = P.empty >> P.rect (Box2.v P2.o (P2.v 1. 1.)) in
    let area = `O { P.o with P.width = 0.2; dashes = Some (0., [0.05]); } in
    I.const (Color.gray 0.3) >> I.cut ~area square
  end;

Db.image "path-earcs" ~author:Db.dbuenzli
  ~title:"Elliptical arcs"
  ~tags:["path"]
  ~note:"In red, elliptical arc from left point to right point. Top row \
         is ~large:false. Left column is ~cw:false."
  ~size:(Size2.v 75. 45.)
  ~view:(Box2.v P2.o (Size2.v 7.5 4.5))
  begin fun _ ->
    let angle = Float.rad_of_deg 0. in
    let r = Size2.v 1.0 0.5 in
    let p0 = P2.v 0. (Size2.h r) in
    let p1 = P2.v (Size2.w r) 0.0 in 
    let square = P.empty >> P.rect (Box2.v_mid P2.o (Size2.v 0.1 0.1)) in
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

Db.image "path-earcs-tr" ~author:Db.dbuenzli
  ~title:"Elliptical arcs"
  ~tags:["path"]
  ~note:"In red, elliptical arc from left point to right point. Top row \
         is ~large:false. Left column is ~cw:false."
  ~size:(Size2.v 75. 75.)
  ~view:(Box2.v (P2.v (-2.) (-2.)) (Size2.v 4. 4.))
  begin fun _ ->
    let m = M3.scale2 (V2.v (-1.) 1.) in
    let e = P.empty >> 
      P.sub (P2.v 1. 0.) >> P.earc (P2.v 1.0 0.5) (P2.v (-1.) 0.)
    in
    let et = P.tr m e in 
    let et' = P.empty >> 
      P.sub (P2.v (-1.) 0.) >> P.earc ~cw:true 
               ~angle:Float.pi (P2.v 1.0 0.5) (P2.v (1.) 0.)
    in
    Mui.Log.msg "%a" P.pp et;
    I.const Color.black >> I.cut et';
  end;

Db.image "path-cubics" ~author:Db.dbuenzli
  ~title:"Cubic paths cases" 
  ~tags:["path"]
  ~note:"Geometric cases for cubic curves. Except in the bottom row, only \
         the end point moves."
  ~size:(Size2.v 115. 105.) 
  ~view:(Box2.v (P2.v (-0.75) (0.625)) (Size2.v 5.75 5.25))
  begin fun _ ->
    let square = P.empty >> P.rect (Box2.v_mid P2.o (Size2.v 0.06 0.06)) in
    let lgray = Color.gray 0.5 >> I.const in
    let mgray = Color.gray 0.3 >> I.const in
    let dgray = Color.gray 0.1 >> I.const in 
    let blue  = Color.blue     >> I.const in
    let ctrl_pt pt = blue >> I.cut square >> I.move pt in 
    let end_pt pt = dgray >> I.cut square >> I.move pt in 
    let tangent p0 p1 = 
      let t = P.empty >> P.sub p0 >> P.line p1 in
      lgray >> I.cut ~area:(`O { P.o with P.width = 0.01 }) t
    in 
    let cubic ~at p0 c0 c1 p1 =
      let curve = P.empty >> P.sub p0 >> P.ccurve c0 c1 p1 in 
      mgray >> I.cut ~area:(`O { P.o with P.width = 0.02 }) curve >> 
      I.blend (tangent p0 c0) >> I.blend (tangent p1 c1) >>
      I.blend (ctrl_pt c0)    >> I.blend (ctrl_pt c1)    >> 
      I.blend (end_pt p0)     >> I.blend (end_pt p1)     >> 
      I.move at
    in
    let p0 = P2.v 0.0 0.5 in 
    let c1 = P2.v 0.3 1.5 in 
    let c2 = P2.v 1.1 0.9 in 
    let pa = P2.v 1.5 0.0 in
    let pb = P2.v (0.8) 1.8 in
    let pc = P2.v (-0.7) 0.7 in
    let pd = P2.v (-0.4) 1.2 in
    let b00 = cubic ~at:(P2.v 0.00 4.00) p0 c1 c2 pa in 
    let b01 = cubic ~at:(P2.v 2.85 3.60) p0 c1 c2 pb in
    let b10 = cubic ~at:(P2.v 0.50 1.75) p0 c1 c2 pc in
    let b11 = cubic ~at:(P2.v 3.15 1.75) p0 c1 c2 pd in
    let p0 = P2.o in
    let c1 = P2.v 0.3 0.0 in 
    let c2 = P2.v 1.2 0.0 in 
    let p1 = P2.v 1.5 0.0 in
    let b20 = cubic ~at:(P2.v (-0.1) 1.25) p0 c1 c2 p1 in
    let c1 = P2.v (-. 0.3) 0.0 in 
    let c2 = P2.v 1.8 0.0 in
    let b21 = cubic ~at:(P2.v 2.7 1.25) p0 c1 c2 p1 in
    b00 >> I.blend b01 >> I.blend 
    b10 >> I.blend b11 >> I.blend 
    b20 >> I.blend b21 
  end;

Db.image "path-dashes" ~author:Db.dbuenzli
  ~title:"Dash patterns"
  ~tags:["path";]
  ~note:"Miscellaneous dash patterns and offsets. "
  ~size:(Size2.v 100. 100.)
  ~view:(Box2.v P2.o (Size2.v 26. 26.))
  begin fun _ -> 
    let path = P.empty >> P.sub (P2.v 1. 0.) >> P.line (P2.v 25. 0.) in
    let line y d = 
      let area = `O { P.o with P.dashes = Some d } in
      Color.gray 0.3 >> I.const >> I.cut ~area path >> I.move (V2.v 0. y)
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

Db.image "path-cantor-dashes" ~author:Db.dbuenzli
  ~title:"Cantor set with dashes" 
  ~tags:["path"; "fractal"]
  ~note:"The Cantor set is drawn with dashes to represent its elements. \
         Maximal dash pattern size is a largely undocumented parameter of \
         the renderer backends, the line renderings may quickly become \
         incorrect."
  ~size:(Size2.v 120. 90.)
  ~view:(Box2.v P2.o (Size2.v 1.2 0.9))
  begin fun _ ->
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
  
Db.image "path-derived" ~author:Db.dbuenzli
  ~title:"Derived subpath of Vg.P"
  ~tags:["path";]
  ~note:"From inward to outward, ellipse, circle, rectangle, rectangle \ 
          with elliptic corners."
  ~size:(Size2.v 50. 50.)
  ~view:(Box2.v P2.o (Size2.v 1. 1.)) 
  begin fun _ -> 
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
