(*---------------------------------------------------------------------------
   Copyright (c) 2013 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Vg SVG renderer.

    {b References.}
    {ul {- Erik Dahlström et al. {{:http://www.w3.org/TR/SVG11/}{e
    Scalable Vector Graphics (SVG) 1.1}}, 2011.}}

    {e Release %%VERSION%% — %%MAINTAINER%% } *)

(** {1:target SVG render targets} *)

val target : ?xml_decl:bool -> ?xmp:string ->unit ->
  Vg.Vgr.dst_stored Vg.Vgr.target
(** [target xml_decl xmp ()] is an SVG render target for rendering to the
    stored destination given to {!Vg.Vgr.create}.
    {ul
    {- [xml_decl], if [true] (default) the
       {{:http://www.w3.org/TR/REC-xml/#NT-XMLDecl}XML declaration} is
       output.}
    {- [xmp] is an optional UTF-8 encoded XML XMP metadata packet describing
       the SVG document (see ISO 16684-1 or the equivalent
        {{:http://www.adobe.com/devnet/xmp.html}Adobe spec.}).
       The convenience function {!Vg.Vgr.xmp} can be used to
       generate a packet.}}

    {b Multiple images.} Multiple image renders on the target are not
    supported. [Invalid_argument] is raised by {!Vg.Vgr.render} if multiple
    images are rendered. *)

(** {1:text Text rendering}

    {b Warning.} The following is subject to change in the future.

    Currently text rendering uses SVG's CSS font selection mechanism
    and doesn't support the glyph API.

    Given a glyph cut:

{!Vg.I.cut_glyphs}[ ~text ~blocks ~advances font glyphs]

    The [blocks], [advances] and [glyphs] parameters are ignored.
    [text] must be provided and is used to define the text to render.
    [font] is used to select the font in a CSS stylesheet. *)

(** {1:limits Render warnings and limitations}

    The following render warnings are reported.
    {ul
    {- [`Unsupported_cut (`O o, i)], outline area cuts can be performed
       only on (possibly transformed) {!Vg.I.const}, {!Vg.I.axial} and
       {!Vg.I.radial} images.}
    {- [`Unsupported_glyph_cut (a, i)], glyph cuts can be performed
       only on (untransformed) {!Vg.I.const}, {!Vg.I.axial} and
       {!Vg.I.radial} images.}
    {- [`Textless_glyph_cut i] if no [text] argument is specified in a
       glyph cut.}}

    The following limitations should be taken into account.
    {ul
    {- Generated SVG files do specify that gradient interpolation
       must be done in linear sRGB space, however many SVG viewers
       do not respect that directive (e.g. most browsers).}} *)

(*---------------------------------------------------------------------------
   Copyright (c) 2013 Daniel C. Bünzli

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
