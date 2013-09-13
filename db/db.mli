(*---------------------------------------------------------------------------
   Copyright 2013 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Image database. *)

(** {1 Images} *)

type author = string * string 
(** The type for authors. Author name and link. *) 

type image = 
  { id : string;                              (** unique image identifier. *)
    title : string;                                       (** image title. *)
    author : author;                                     (** image author. *)
    tags : string list;                          (** descriptive tag list. *)
    note : string option;                        (** note about the image. *)
    size : Gg.size2;                              (** render surface size. *)
    view : Gg.box2;                              (** image view rectangle. *)
    image : Gg.box2 -> Vg.image; }    (** image definition, arg is [view]. *) 
(** The type for database images. *)


val image : string -> title:string -> author:author -> ?tags:string list -> 
  ?note:string -> size:Gg.size2 -> view:Gg.box2 -> (Gg.box2 -> Vg.image) -> unit
(** [image id authors title subject note tags meta size view fimg]
    adds an image to the database. *)

val all : unit -> image list 
(** [all ()] is the list of all images in the db lexicographically
    sorted by id. *)

val mem : string -> bool
(** [mem id] is [true] if there an image with id [id]. *)

val find : string -> image option
(** [find id] is the image with id [id], if any. *)

val search : ?ids:string list -> ?prefixes:string list -> ?tags:string list -> 
  unit -> image list
(** [search ids prefixes tags ()] is a list of images lexicographically
    sorted by id that satisfy at least one of these conditions:
    {ul 
    {- The image id is in [ids].}
    {- The image id is prefixed by an element of [prefixes].}
    {- The image has a tag in [tags].}} *)

val indexes : unit -> string list * string list
(** [indexes ()] is the lexicographically sorted lists of ids and
    tags present in the database. *)

val xmp : create_date:float -> creator_tool:string -> image -> string
(** [xmp create_date creator_tool i] is an XMP metadata packet for [i] *)

val renderable : image -> Vg.Vgr.renderable
(** [renderable i] is a renderable for [i]. *)

val find_loc : string -> (string * (string * int)) list -> (string * int) option
(** [find_loc id locs] looks up in locs the filename and line of of [id]. *)

(** {1 Authors} 
    
    Authors used in many files. *)

val dbuenzli : author


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
