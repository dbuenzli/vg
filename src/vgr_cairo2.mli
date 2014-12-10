(*---------------------------------------------------------------------------
   Copyright 2013 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Vg Cairo2 renderer.

    {b Dependency:}
    {ul {- {e {{:http://forge.ocamlcore.org/projects/cairo/}Cairo2 library
     for OCaml}}}}

    {e Release %%VERSION%% — %%MAINTAINER%% } *)

(** {1:target Cairo2 render targets} *)

type cairo_backend = [ `Surface | `PNG | `PDF ]

val target : ?resolution:float -> cairo_backend -> Vg.Vgr.dst_stored Vg.Vgr.target

val target_surface : Cairo.Surface.t -> [`Other] Vg.Vgr.target
(** [target s] is a render target for rendering to the Cairo2 surface [s].

    {b Multiple images.} Multiple images render on the target is supported.
    Each new render clears the surface. *)

(** {1:text Text rendering}

    {b Warning.} The following is subject to change in the future.

    Currently text rendering uses Cairo2's font selection mechanism
    and doesn't support the glyph API.

    Given a glyph cut:

{!Vg.I.cut_glyphs}[ ~text ~blocks ~advances font glyphs]

    The [blocks], [advances] and [glyphs] parameters are ignored.
    [text] must be provided and is used to define the text to render.
    [font] is used to select the font family.
    
    The weight is limited to Normal ([< `W600]) and Bold ([>= `W600]). *)

(** {1:limits Render warnings and limitations}

    The following render warnings are reported.
    {ul
    {- [`Unsupported_cut (`O o, i)], outline area cuts can be performed
       only on (possibly transformed) {!Vg.I.const}, {!Vg.I.axial} and
       {!Vg.I.radial} images.}
    {- [`Unsupported_glyph_cut (`O o, i)], outline glyph cuts can be
       performed only on (untransformed) {!Vg.I.const}, {!Vg.I.axial}
       and {!Vg.I.radial} images.}
    {- [`Textless_glyph_cut i] if no [text] argument is specified in a
       glyph cut.}}

    *)

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
