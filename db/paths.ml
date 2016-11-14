(*---------------------------------------------------------------------------
   Copyright (c) 2013 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Gg
open Vg
;;

(** Test images for path areas. *)

Db.image "path-sq-outline" ~author:Db.dbuenzli
  ~title:"Square outline in gray"
  ~tags:["path"]
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
  ~tags:["path"; "dashes";]
  ~note:"Line width is 0.1 as we are on the surface edge."
  ~size:(Size2.v 50. 50.)
  ~view:Box2.unit
  begin fun _ ->
    let square = P.empty >> P.rect (Box2.v P2.o (P2.v 1. 1.)) in
    let area = `O { P.o with P.width = 0.2; dashes = Some (0., [0.05]); } in
    I.const (Color.gray 0.3) >> I.cut ~area square
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
  ~tags:["path"; "dashes";]
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
  ~tags:["path"; "fractal"; "dashes";]
  ~note:"The Cantor set is drawn with dashes to represent its elements. \
         Maximal dash pattern size is a largely undocumented parameter of \
         the renderer backends, the line renderings may quickly become \
         incorrect (e.g. ghostscript says the file is broken)."
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
        let pat =                (* only half + 1 of the dashes are needed. *)
          let rec keep c l acc =
            if c = 0 then List.rev acc else
            keep (c - 1) (List.tl l) ((List.hd l) :: acc)
          in
          keep (List.length dashes / 2 + 1) dashes []
        in
        let area = `O { o with P.dashes = Some (0., pat); } in
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
  end;

Db.image "path-miter-angle" ~author:Db.dbuenzli
  ~title:"Miter angle limit"
  ~tags:["path";]
  ~note:"In the left column the miter angle is set below the angle made by \
         the path, all joins should be pointy. In the right column the miter \
         angle is set above the angle made by the path, all joins should be \
         bevelled."
  ~size:(Size2.v 120. 570.)
  ~view:(Box2.v P2.o (Size2.v 4.0 19.0))
  begin fun _ ->
    let gray = I.const (Color.gray 0.3) in
    let white = I.const Color.white in
    let wedge a =
      P.empty >> P.sub (V2.polar 0.6 a)  >> P.line P2.o >> P.line (V2.v 0.6 0.)
    in
    let path x y miter_angle a =
      let area = (`O { P.o with P.width = 0.1; miter_angle }) in
      let wedge = wedge a in
      let outline = I.cut ~area wedge gray in
      let data = I.cut ~area:(`O { P.o with P.width = 0.01 }) wedge white in
      outline >> I.blend data >> I.move (P2.v x (y +. 0.2))
    in
    let acc = ref I.void in
    for i = 0 to 18 do
      let y = float i in
      let base = y *. 10. in
      let a = Float.rad_of_deg base in
      let less = Float.rad_of_deg (base -. 1.) in
      let more = Float.rad_of_deg (base +. 1.) in
      acc := !acc >> I.blend (path 1. y less a) >> I.blend (path 3. y more a)
    done;
    !acc
  end;

Db.image "path-circle-ellipse" ~author:Db.dbuenzli
  ~note:"Shows dilation on line width due to scaling \
         (from Christophe Troestler ocaml-cairo example tips_ellipse.ml).
         The form on the left is a scaled circle. The form on the right is
         an ellipse."
  ~title:"Line width dilation"
  ~tags:["path"]
  ~size:(Size2.v 120. 120.)
  ~view:(Box2.v P2.o (Size2.v 1. 1.))
  begin fun _ ->
    let circle =
      let circle = P.(empty >> circle (P2.v 0.5 0.5) 0.4) in
      let area = `O { P.o with P.width = 0.1 } in
      I.cut ~area circle (I.const Color.black) >>
      I.scale (Size2.v 0.5 1.)
    in
    let ellipse =
      let ellipse = P.(empty >> ellipse (P2.v 0.5 0.5) (Size2.v 0.2 0.4)) in
      let area = `O { P.o with P.width = 0.1 } in
      I.cut ~area ellipse (I.const Color.black) >>
      I.move (V2.v 0.25 0.)
    in
    I.blend circle ellipse
  end


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
