(*---------------------------------------------------------------------------
   Copyright (c) 2013 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Gg
open Vg

(** Test images for non-primitive cuts *)

let annulus =
  let circle r = P.empty >> P.circle (P2.v 0.5 0.5) r in
  let outer = 0.3 in
  let inner = outer *. 0.6 in
  P.append (circle outer) (circle inner)

let unit_dots count dot_width =
  let s = Random.State.make [|1922278|] in
  let rand = Float.srandom s in
  let dot =
    let circle = P.empty >> P.circle P2.o (0.5 *. dot_width) in
    I.const Color.black >> I.cut circle
  in
  let acc = ref I.void in
  for i = 1 to count do
    let x = rand ~min:0.2 ~len:0.6 () in
    let y = rand ~min:0.2 ~len:0.6 () in
    acc := I.blend !acc (dot >> I.move (P2.v x y))
  done;
  !acc

let dotted_region area =
  let outline = `O { P.o with P.width = 0.001 } in
  I.cut ~area annulus (unit_dots 800 0.01) >>
  I.blend (I.cut ~area:outline annulus (I.const (Color.gray 0.75)))
;;

Db.image "npcut-aeo" __POS__ ~author:Db.dbuenzli
  ~title:"Even-odd area, non primitive cut"
  ~tags:["cut"]
  ~note:"Ring with dots."
  ~size:(Size2.v 60. 60.)
  ~view:Box2.unit
  begin fun _ -> dotted_region `Aeo end;

Db.image "npcut-anz" __POS__ ~author:Db.dbuenzli
  ~title:"Non-zero winding area, non primitive cut"
  ~tags:["cut"]
  ~note:"Circle with dots."
  ~size:(Size2.v 60. 60.)
  ~view:Box2.unit
  begin fun _ -> dotted_region `Anz end;

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
