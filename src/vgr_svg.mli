(*---------------------------------------------------------------------------
   Copyright 2013 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)
(** Vg SVG renderer.

    The SVG renderer outputs a {!renderable} value as an
    {{:http://www.w3.org/Graphics/SVG/} SVG} 1.1 document.

    {b Render metadata.} The following metadata keys are supported.
    {ul
    {- {!Vg.Vgr.Meta.title}, title of the document.}}

    {b Unsupported capabilities.} Only the default [`Over] blend mode 
    is supported. Any use of other blend mode falls back on [`Over].

    {e Release %%VERSION%% - %%AUTHORS%% } *)

(** {1 Renderer} *)

val renderer : ?meta:Vg.meta -> [< Vg.Vgr.dst_stored] -> Vg.renderer
(** [renderer ?meta dst] is an SVG renderer rendering to [dst]. *)

(** {1 Render metadata} *)

type warning = [`Blend]
(** The type for rendering warnings. 
    {ul
    {- [`Blend] a blend mode different from [`Over] was used.}} *)

val warn : (warning -> unit) Vg.key
(** [warn] is the function used to report unsupported capabilities. The warning
    are the following. *)

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
