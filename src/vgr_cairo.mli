(*---------------------------------------------------------------------------
   Copyright 2014 Arthur Wendling, Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(* Based on the Vgr_htmlc documentation by Daniel C. Bünzli. *)

(** Vg Cairo renderer.

    {b Dependencies:}
    {ul {- {e {{:http://forge.ocamlcore.org/projects/cairo/}Cairo2 bindings
     for OCaml}}}
     {- {e {{:http://cairographics.org/}Cairo Graphics library}}}}

    {e Release %%VERSION%% — %%MAINTAINER%% } *)

(** {1:target Cairo render targets} *)

val target : ?resolution:Gg.V2.t -> [< `PDF | `PNG | `PS | `SVG ] ->
  Vg.Vgr.dst_stored Vg.Vgr.target
(** [target resolution fmt] is a render target for rendering to the stored
    destination given to {!Vg.Vgr.create} in the chosen format [fmt].
    {ul
    {- [resolution], specifies the rendering resolution in samples per
       meters. The PDF, PS and SVG formats are measured in points by Cairo,
       while the PNG format is in pixels. If unspecified, the default
       conversion to points is used.}}

    {b Multiple images.} Multiple images render on the target are not
    supported. [Invalid_argument] is raised by {!Vg.Vgr.render} if multiple
    images are rendered. *)

val target_surface : ?size:Gg.size2 -> Cairo.Surface.t ->
  [`Other] Vg.Vgr.target
(** [target_surface size s] is a render target for rendering to the Cairo
    surface [s].
    {ul
    {- The physical size of {{!Vg.Vgr.renderable}renderables} is ignored and
       the view rectangle is mapped on the surface size.}
    {- [size], Surfaces created with [Cairo.Surface] have a valid size, while
       file based surfaces have a size of zero by default: If the size of the
       surface can not be determined, the optional argument [size] is used
       instead. [Invalid_argument] is raised if the size is invalid.}}

    {b Multiple images.} Multiple images render on the target is supported.
    Each new render clears the surface. However, the results are dependent on
    Cairo internals: file based surfaces for PDF, PS and SVG do not clear the
    view and blend the different images instead. *)

(** {1:text Text rendering}

    {b Warning.} The following is subject to change in the future.

    Currently text rendering uses Cairo's font selection mechanism
    and doesn't support the glyph API.

    Given a glyph cut:

{!Vg.I.cut_glyphs}[ ~text ~blocks ~advances font glyphs]

    The [blocks], [advances] and [glyphs] parameters are ignored.
    [text] must be provided and is used to define the text to render.
    [font] is used to select the font family.
    
    The weight is limited to Normal ([< `W700]) and Bold ([>= `W700]). *)

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

    The following limitations should be taken into account.
    {ul
    {- In Cairo, the gradient color interpolation is performed
       in (non-linear) sRGB space. This doesn't respect Vg's semantics.}} *)

(*---------------------------------------------------------------------------
   Copyright 2014 Arthur Wendling, Daniel C. Bünzli.
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
