(*---------------------------------------------------------------------------
   Copyright (c) 2013 The vg programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Micro browser UI library

    Nothing serious and still ugly.

    Open the module to use it. It defines only modules in your scope
    and a single composition value. *)


(** {1 UI elements} *)

(** UI elements. *)
module Ui : sig
  type 'a printer = Format.formatter -> 'a -> unit
  (** The type for value printers. *)

  type 'a t
  (** The type for UI elements defining values of type ['a]. *)

  type ('a, 'b) conf = 'a t * ('b -> unit)
  (** The type for UI elements and their configuration function. *)

  val on_change : 'a t -> ('a -> unit) -> unit
  (** [on_change ui f] calls [f] whenever the value of [ui] changes.
      Previous [on_change] function is overwritten. *)

  val show : 'a t -> unit
  (** [show ui] shows [ui] on screen. *)

  val main : (unit -> unit) -> unit
  (** [main setup] executes [setup ()] and runs the UI. *)

  (** {1 UI elements} *)

  val group : ?id:string -> unit -> unit t
  val label : ?id:string -> ?title:string -> ?ctrl:bool -> string -> string t
  val label_mut : ?id:string -> ?title:string -> ?ctrl:bool -> string ->
    (string, string) conf
  val text : ?id:string -> string -> (string, string) conf
  val bool : ?id:string -> bool -> (bool, bool) conf

  type link_conf = [ `Text of string | `Href of string | `Download of string ]
  val link : ?id:string -> ?title:string ->
    href:string -> string -> (unit, link_conf) conf

  type 'a select_conf = [ `Select of 'a option | `List of 'a list ]
  val select : ?id:string -> ?title:string -> 'a printer -> 'a option ->
    'a list -> ('a option, 'a select_conf) conf

  val mselect : ?id:string -> ?title:string -> 'a printer -> 'a list ->
    'a list ->
    ('a list, 'a list) conf

  val canvas : ?id:string -> unit
    -> unit t * Js_of_ocaml.Dom_html.canvasElement Js_of_ocaml.Js.t
  val canvas_data : Js_of_ocaml.Dom_html.canvasElement Js_of_ocaml.Js.t
    -> string
(*  val canvas_blob : Dom_html.canvasElement Js.t -> File.blob Js.t *)


  type object_conf =
    [ `Data of string | `Size of float * float | `Name of string]

  val object_ : ?id:string -> unit -> (unit, object_conf) conf

  type 'a menu_conf = [ `Select of 'a | `List of 'a list ]
  val menu : ?id:string -> 'a printer -> 'a -> 'a list ->
    ('a, 'a menu_conf) conf

  val classify : 'a t -> string -> bool -> unit
  val visible : ?relayout:bool -> 'a t -> bool -> unit
  val set_raw_child : 'a t -> string -> unit
  val set_svg_child : 'a t -> string -> unit
  val set_txt_child : 'a t -> string -> unit
  val client_size : 'a t -> int * int
  val set_height : 'a t -> string -> unit
  val set_width : 'a t -> string -> unit

  val hash : unit -> string
  val set_hash : string -> unit
  val on_hash_change : (string -> unit) -> unit

  val escape_binary : string -> string
  (** [escape data] escapes the binary data [data]. *)
end

val ( *> ) : 'a Ui.t -> 'b Ui.t -> 'a Ui.t
(** [p *> c] add [c] as a child to [p] and returns [p]. Left associative. *)

(** {1 Persistent storage} *)

(** Persistent storage.

    Safe if nobody messes with the storage outside of the program.

    {b WARNING} During developement code reorderings of {!key} will
    corrupt existing storage. {!Store.force_version} can mitigate that
    problem. *)
module Store : sig

  (** {1 Storage scope and support} *)

  type scope = [ `Session | `Persist ]
  (** The storage scope. *)

  val support : scope -> bool
  (** [support scope] is [true] iff values can be stored in [scope]. *)

  (** {1 Keys} *)

  type 'a key
  (** The type for keys whose lookup value is 'a *)

  val key : unit -> 'a key
  (** [key ()] is a new storage key. *)

  (** {1 Versioning} *)

  val force_version : ?scope:scope -> string -> unit
  (** [force_version v] checks that the version of the store is [v].  If
      it's not it {!clear}s the store and sets the version to [v]. *)

  (** {1 Storage}

      In the functions below [scope] defaults to [`Persist]. *)

  val mem : ?scope:scope -> 'a key -> bool
  (** [mem k] is [true] iff [k] has a mapping. *)

  val add : ?scope:scope -> 'a key -> 'a -> unit
  (** [add k v] maps [k] to [v]. *)

  val rem : ?scope:scope -> 'a key -> unit
  (** [rem k] unbinds [k]. *)

  val find : ?scope:scope -> 'a key -> 'a option
  (** [find k] is [k]'s mapping in [m], if any. *)

  val get : ?scope:scope -> ?absent:'a -> 'a key -> 'a
  (** [get k] is [k]'s mapping. If [absent] is provided and [m] has
      not binding for [k], [absent] is returned.

      @raise Invalid_argument if [k] is not bound and [absent]
      is unspecified. *)

  val clear : ?scope:scope -> unit -> unit
  (** [clear ()], clears all mapping. *)
end

(** {1 Timing and logging} *)

(** Timing functions. *)
module Time : sig
  val now : unit -> float
  (** [now ()] is the current UTC time as a Unix timestamp (in secs). *)

  val now_date : unit -> (int * int * int) * (int * int * int)
  (** [now_date ()] is the current UTC time as
      [(YYYY, MM, DD), (HH, mm, ss)]. *)

  val duration : ('a -> 'b) -> 'a -> float * 'b
  (** [duration f v] is [(t, f v)] with [t] the time taken by the call in
      seconds. *)

  val delay : float -> (unit -> unit) -> unit
  (** [delay s f] executes [f] in [s] seconds. *)
end

(** Logging functions. *)
module Log : sig
  val msg_js : 'a -> unit
  (** [msg_js v] logs [v] as JavaScript a object. *)

  val msg : ('a, Format.formatter, unit) format -> 'a
  (** [msg] is like {!Format.printf} *)

  val err : ('a, Format.formatter, unit) format -> 'a
  (** [err] is like {!Format.eprintf} *)

end


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
