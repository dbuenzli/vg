(*---------------------------------------------------------------------------
   Copyright (c) 2013 The vg programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Image database. *)

(** {1 Images} *)

type author = string * string
(** The type for authors. Author name and link. *)

type image =
  { id : string;                              (** unique image identifier. *)
    loc : string * int;                                 (** File and line. *)
    title : string;                                       (** image title. *)
    author : author;                                     (** image author. *)
    tags : string list;                          (** descriptive tag list. *)
    note : string option;                        (** note about the image. *)
    size : Gg.size2;                              (** render surface size. *)
    view : Gg.box2;                              (** image view rectangle. *)
    image : Gg.box2 -> Vg.image; }    (** image definition, arg is [view]. *)
(** The type for database images. *)


val image :
  string -> string * int * int * int -> title:string ->
  author:author -> ?tags:string list -> ?note:string ->
  size:Gg.size2 -> view:Gg.box2 -> (Gg.box2 -> Vg.image) -> unit
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

(** {1 Authors}

    Authors used in many files. *)

val dbuenzli : author
