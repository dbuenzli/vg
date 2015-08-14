(*---------------------------------------------------------------------------
   Copyright 2014 Arthur Wendling, Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Vg {{:http://cairographics.org/}Cairo} renderer.

    {e Release %%VERSION%% — %%MAINTAINER%% } *)

(** {1:target Cairo render targets} *)

val target : Cairo.context -> [`Other] Vg.Vgr.target
(** [target ctx] is a render target for rendering to the Cairo
    context [ctx]. Rendering a {{!Vg.Vgr.renderable}renderable}
    [(size, view, i)] is done as follows.
    {ol
    {- The context's current state is saved using {!Cairo.save}.}
    {- The context's is clipped to [Box2.v P2.o size]. This
       box is cleared with {!Color.void} and the portion
       [view] of [i] is rendered in this box.}
    {- The context's initial state is restored using {!Cairo.restore}}}
    Nothing else is done to [ctx].

    {b Multiple images.} Multiple images rendering is supported.  For
    each renderable the above procedure is performed on [ctx]. If you
    want to have each renderable on a single page on backends that support
    it you should handle this between two renderable using Cairo's API. *)

val stored_target : [< `Pdf | `Png of Gg.V2.t | `Ps | `Svg ] ->
  Vg.Vgr.dst_stored Vg.Vgr.target
(** [stored_target fmt] is a [fmt] render target for rendering to
    the stored destination given to {!Vg.Vgr.create}. For [`Png]
    the argument specifies the rendering resolution in samples
    per meters.

    {b Multiple images.} Multiple image rendering is supported on
    [`Pdf] and [`Ps] target, each renderable creates a new page of the
    renderable size. Multiple image rendering is not supported on
    [`Png] and [`Svg] and [Invalid_argument] is raised by
    {!Vg.Vgr.render} if multiple images are rendered. *)

(** {1:text Text rendering}

    {b Warning.} The following is subject to change in the future.

    Currently text rendering uses Cairo's font selection mechanism
    and doesn't support the glyph API.

    Given a glyph cut:

{!Vg.I.cut_glyphs}[ ~text ~blocks ~advances font glyphs]

    The [blocks], [advances] and [glyphs] parameters are ignored.
    [text] must be provided and is used to define the text to render.
    [font] is used to select the font family.

    The weight is limited to Normal ([< `W700]) and Bold ([>= `W700]).

    {1:limits Render warnings and limitations}

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
