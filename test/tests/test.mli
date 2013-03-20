(*---------------------------------------------------------------------------
   Copyright (c) %%COPYRIGHTYEAR%%, Daniel C. Bünzli. All rights reserved.
   Distributed under a BSD license, see license at the end of the file.
   %%PROJECTNAME%% version %%VERSION%%
  ---------------------------------------------------------------------------*)

type t = 
    { name : string;          (** Test identifier, case insensitive. *) 
      info : string;          (** Short description of the image *)
      note : string;          (** Details to look for. *)
      size : Gg.size2;        (** Output surface size. *)
      view : Gg.box2;         (** View rectangle. *)
      image : Vg.image Lazy.t (** Image. *) }
(** The type for tests. *)

val find : ?names:string list -> ?prefixes:string list -> ?keywords:string list
    -> unit -> t list
(** [find names prefixes keywords] is a list of tests sorted by increasing
    test name that satisfy at least one of these conditions : 
    {ul 
    {- The name of the test is in [names].}
    {- An element of [prefixes] is a prefix of the test name.}
    {- The name or info or note of the test matches a keyword in [keywords]}}
    Matches are case insentitive and all arguments default to [[]]. 
    [find ~prefix:[""]] lists the contents of the database. *)

val paragraphs : string -> string list list
(** [paragraphs s] is the list of paragraphs in [s]. A paragraph is a
    list of words. A word is a sequence of non-blank characters.
    Paragraphs are separated by two or more sucessive newlines. Words
    are separated by one or more spaces or a single newline. *)


(*---------------------------------------------------------------------------
   Copyright (c) %%COPYRIGHTYEAR%%, Daniel C. Bünzli
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

   3. Neither the name of the Daniel C. Bünzli nor the names of
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
