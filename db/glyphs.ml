(*---------------------------------------------------------------------------
   Copyright (c) 2013 The vg programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Gg
open Vg
;;

(** Test images for glyphs. *)

let open_sans_xbold =
  { Font.name = "Open Sans"; size = 1.0; weight = `W800; slant = `Normal}

(* Font info for the string "Revolt!" as found in Open_sans.extra_bold. *)

let glyphs = [ 53; 72; 89; 82; 79; 87; 4 ]
let advances = [1386.; 1266.; 1251.; 1305.; 662.; 942.; 594.;]
let u_to_em = 2048.
;;

Db.image "glyph-revolt" __POS__ ~author:Db.dbuenzli
  ~title:"Revolt in black"
  ~tags:["glyph"]
  ~note:"Black characters “Revolt!”, approximatively centered \
         in the image."
  ~size:(Size2.v 135. 45.)
  ~view:(Box2.v P2.o (Size2.v 3. 1.))
  begin fun _ ->
    let font = { open_sans_xbold with Font.size = 0.7 } in
    let text = "Revolt!" in
    I.const Color.black |> I.cut_glyphs ~text font glyphs |>
    I.move (V2.v 0.23 0.25)
  end;

Db.image "glyph-revolt-outline" __POS__ ~author:Db.dbuenzli
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
    I.const Color.black |> I.cut_glyphs ~area ~text font glyphs |>
    I.move (V2.v 0.23 0.25)
  end;

Db.image "glyph-revolt-fade" __POS__ ~author:Db.dbuenzli
  ~title:"Revolt from black to white"
  ~tags:["glyph"; "gradient" ]
  ~note:"Characters “Revolt!”, approximatively centered \
         in the image and fading from black to white"
  ~size:(Size2.v 135. 45.)
  ~view:(Box2.v P2.o (Size2.v 3. 1.))
  begin fun _ ->
    let font = { open_sans_xbold with Font.size = 0.7 } in
    let text = "Revolt!" in
    let stops = [0.0, Color.black; 1.0, Color.white] in
    I.axial stops P2.o (P2.v 3. 0.) |> I.cut_glyphs ~text font glyphs |>
    I.move (V2.v 0.23 0.25)
  end;


Db.image "glyph-aspect" __POS__ ~author:Db.dbuenzli
  ~title:"Glyph aspect"
  ~tags:["glyph"]
  ~note:"The character should read “R”, without distortion."
  ~size:(Size2.v 25. 50.)
  ~view:(Box2.v P2.o (Size2.v 2. 1.))
  begin fun _ ->
    let font = { open_sans_xbold with Font.size = 0.5 } in
    let text = "R" in
    let sq = P.empty |> P.rect (Box2.v (P2.v 0. 0.75) (P2.v 0.25 0.25)) in
    I.const Color.black |> I.cut sq |>
    I.blend (I.const Color.black |> I.cut_glyphs ~text font [53;]) |>
    I.scale (V2.v 4.0 1.0)
  end;

Db.image "glyph-multi" __POS__ ~author:Db.dbuenzli
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
      I.const Color.black |> I.cut_glyphs ~text font glyphs |>
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
    | Some pt -> blend_revolt (acc |> I.blend (revolt pt)) (next max dv pt)
    in
    let margin =
      let area = `O { P.o with P.width = 0.1 } in
      I.const Color.white |> I.cut ~area (P.empty |> P.rect view)
    in
    blend_revolt (I.const Color.white) (Some P2.o) |> I.rot angle |>
    I.move (P2.v 0.2 (-. sin (angle))) |>
    I.blend margin
  end;

Db.image "glyph-advances" __POS__ ~author:Db.dbuenzli
  ~title:"Advancing revolt"
  ~tags:["glyph"]
  ~note:"First line, no advances specified. Second line advances with glyph
         advances, should render same as first line. Third line, funky glyph
         advances with up and down."
  ~size:(Size2.v 135. (45. *. 3.))
  ~view:(Box2.v P2.o (Size2.v 3. 3.))
  begin fun _ ->
    let fsize = 0.7 in
    let font = { open_sans_xbold with Font.size = fsize } in
    let text = "Revolt!" in
    let black = I.const Color.black in
    let ypos n = V2.v 0.23 (0.25 +. n *. 0.98) in
    let no_advances = I.cut_glyphs ~text font glyphs in
    let adv_advances =
      let adv a = V2.v ((a *. fsize) /. u_to_em) 0. in
      I.cut_glyphs ~text ~advances:(List.map adv advances) font glyphs
    in
    let funky_advances =
      let adv i a =
        V2.v ((a *. fsize) /. u_to_em) (if i mod 2 = 0 then 0.2 else -0.2)
      in
      I.cut_glyphs ~text ~advances:(List.mapi adv advances) font glyphs
    in
    black |> funky_advances |> I.move (ypos 0.) |>
    I.blend (black |> adv_advances |> I.move (ypos 1.)) |>
    I.blend (black |> no_advances |> I.move (ypos 2.))
  end
;;

Db.image "glyph-affiche-blocks" __POS__ ~author:Db.dbuenzli
  ~title:"Affiché with ligature and text to glyph correspondence"
  ~tags:["glyph"]
  ~note:"The ffi is a single glyph and the é glyph is encoded as the sequence
         <U+0065, U+0301> in the text string."
  ~size:(Size2.v 135. 45.)
  ~view:(Box2.v P2.o (Size2.v 3. 1.))
  begin fun _ ->
    let font = { open_sans_xbold with Font.size = 0.7 } in
    let glyphs = [ 36; 605; 70; 75; 171 ] in
    let text = "Affiche\xCC\x81" in
    let blocks = false, [(1,1); (3,1); (1,1); (1,1); (2,1)] in
    I.const Color.black |> I.cut_glyphs ~text ~blocks font glyphs |>
    I.move (V2.v 0.23 0.25)
  end
;;

(*---------------------------------------------------------------------------
   Copyright (c) 2013 The vg programmers

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
