(*---------------------------------------------------------------------------
   Copyright 2013 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Image database. *)

(** {1 Images} *)

type image = 
  { id : string;                              (** unique image identifier. *)
    title : string;                                       (** image title. *)
    author : string;                                     (** image author. *)
    tags : string list;                          (** descriptive tag list. *)
    subject : string option;                            (** image subject. *)
    note : string option;                        (** note about the image. *)
    meta : Vg.meta;                                   (** render metadata. *)
    size : Gg.size2;                              (** render surface size. *)
    view : Gg.box2;                              (** image view rectangle. *)
    image : unit -> Vg.image; }                      (** image definition. *) 
(** The type for database images. *)

val image : string -> title:string -> author:string -> ?tags:string list -> 
  ?subject:string -> ?note:string -> ?meta:Vg.meta -> size:Gg.size2 -> 
  view:Gg.box2 -> (unit -> Vg.image) -> unit
(** [image id authors title subject note tags meta size view fimg]
    adds an image to the database. *)

val find : ?ids:string list -> ?prefixes:string list -> ?tags:string list -> 
  unit -> image list
(** [find ids prefixes tags ()] is a list of images lexicographically
    sorted by id that satisfy at least one of these conditions:
    {ul 
    {- The image id is in [ids].}
    {- The image id is prefixed by an element of [prefixes].}
    {- The image has a tag in [tags].}} *)

val indexes : unit -> string list * string list
(** [indexes ()] is the lexicographically sorted lists of ids and
    tags present in the database. *)

val render_meta : image -> Vg.meta
(** [render_meta i] is metadata with {{!Vg.Vgm.stdkeys}standard} keys 
    added followed by those of [i.meta]. *)

val renderable : image -> Vg.Vgr.renderable

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
