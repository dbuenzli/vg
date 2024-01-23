(*---------------------------------------------------------------------------
   Copyright (c) 2013 The vg programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Gg
open Vg
;;

(** Test images for data escapes *)

Db.image "escape-xmp" __POS__ ~author:Db.dbuenzli
  ~title:"XMP metadata escape </g> & \"bla\""
  ~tags:["escape"; ]
  ~note:"These </g> markup \"delimiters\" should be & escaped in \
         the meta data. The image is just a gray square."
  ~size:(Size2.v 50. 50.)
  ~view:Box2.unit
  begin fun _ ->
    I.const (Color.gray 0.3) |> I.cut (P.empty |> P.rect Box2.unit)
  end;
