(*---------------------------------------------------------------------------
   Copyright 2013 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Micro browser UI library 

    Nothing serious and still ugly.

    Open the module to use it. It defines only modules in your scope
    and a single composition value. *)

(**/**)
module D : sig  
  (** {1 Constants} *)

  val window : Dom_html.window Js.t
  val document : Dom_html.document Js.t 

  (** {1 DOM nodes} *)

  val el : ?id:string -> string -> Dom_html.element Js.t
  (** [el n] is an element [m]. *)

  val txt : string -> Dom.text Js.t
  (** [txt s] is [s] as a text node. *)

  val canvas : ?id:string -> unit -> Dom_html.canvasElement Js.t

  (** {1 Events } *)
end

module Ev : sig
  val make : string -> 'a Dom.Event.typ 
  val load : Dom_html.event Js.t Dom.Event.typ
  include module type of Dom_html.Event
    
  val cb : (< .. > as 'a) Js.t -> ('c #Dom.event as 'b) Js.t Dom.Event.typ -> 
    ('a Js.t -> 'b Js.t -> bool) -> unit
    (** [cb n e f] invokes [f n event] whenever event [e] occurs. If [f]
          returns [false] prevents the default action. *)
end

(**/**)

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
  val label : ?id:string -> ?ctrl:bool -> string -> string t
  val label_mut : ?id:string -> ?ctrl:bool -> string -> (string, string) conf
  val text : ?id:string -> string -> (string, string) conf
  val bool : ?id:string -> bool -> (bool, bool) conf

  type link_conf = [ `Text of string | `Href of string ]
  val link : ?id:string -> href:string -> string -> (unit, link_conf) conf

  type 'a select_conf = [ `Select of 'a option | `List of 'a list ]
  val select : ?id:string -> 'a printer -> 'a option -> 'a list -> 
    ('a option, 'a select_conf) conf

  val mselect : ?id:string -> 'a printer -> 'a list -> 'a list -> 
    ('a list, 'a list) conf

  val canvas : ?id:string -> unit -> unit t * Dom_html.canvasElement Js.t
  val canvas_data : Dom_html.canvasElement Js.t -> string

  type 'a menu_conf = [ `Select of 'a | `List of 'a list ]
  val menu : ?id:string -> 'a printer -> 'a -> 'a list -> 
    ('a, 'a menu_conf) conf 

  val classify : 'a t -> string -> bool -> unit
  val visible : ?relayout:bool -> 'a t -> bool -> unit

  val hash : unit -> string
  val set_hash : string -> unit
  val on_hash_change : (string -> unit) -> unit
end

val ( *> ) : 'a Ui.t -> 'b Ui.t -> 'a Ui.t
(** [p *> c] add [c] as a child to [p] and returns [p]. Left associative. *)

(** {1 Persistent storage} *) 

(** Persistent storage.

    Safe if nobody messes with the storage outside of the program. 

    {b WARNING/TODO} During developement code reorderings of {!key}
    will corrupt existing storage.  
*)
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
      
  (** {1 Storage} 

      In the functions below [scope] defaults to [`Persist]. *)
      
  val mem : ?scope:scope-> 'a key -> bool
  (** [mem k] is [true] iff [k] has a mapping. *)
    
  val add : ?scope:scope -> 'a key -> 'a -> unit 
  (** [add k v] maps [k] to [v]. *)
    
  val rem : ?scope:scope -> 'a key -> unit
  (** [rem k] unbinds [k]. *)
    
  val find : ?scope:scope -> 'a key -> 'a option
  (** [find k] is [k]'s mapping in [m], if any. *)
      
  val get : ?scope:scope -> 'a key -> 'a 
  (** [get k] is [k]'s mapping. 
      @raise Invalid_argument if [k] is not bound. *)
    
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

  val delay : (unit -> unit) -> float -> unit
  (** [delay f s] executes [f] in [s] seconds. *)
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
