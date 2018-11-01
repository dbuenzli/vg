(*---------------------------------------------------------------------------
   Copyright (c) 2013 The vg programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Vg PDF renderer.

    Renders a sequence of renderables as a multi-page PDF 1.7
    document. Each renderable defines a page of the document.

    {b Bug reports.}  PDF being an insane standard, rendering
    abilities of PDF readers vary wildly. No rendering bug report for
    this renderer will be considered if it cannot be reproduced in the
    latest Adobe Acrobat Reader.

    {e %%VERSION%% - {{:%%PKG_HOMEPAGE%% }homepage}} *)

(** {1:fonts Font resolution}

    Font resolution happens during the rendering of {!Vg.I.cut_glyphs}
    images through the [font] callback given to the PDF rendering {!target}.
    See {!text} for more details. *)

type otf_font
(** The type for OpenType fonts. *)

val otf_font : string -> ([> `Otf of otf_font], Otfm.error) Result.result
(** [otf_font bytes] is an OpenType font from the OpenType byte
    serialization [bytes]. *)

type font =
  [ `Otf of otf_font
  | `Serif | `Sans | `Fixed
  | `Helvetica | `Times | `Courier ]
(** The type for font resolution results. Any case different from [`Otf]
    ends up using the PDF standard fonts. See {!text} for details. *)

val font : Vg.font -> font
(** [font] is the default font resolver. Given a {!Vg.font} [f] it performs the
    following resolutions according to value of [f.Font.name]:
    {ul
    {- ["Helvetica"], returns [`Sans]}
    {- ["Times"], returns [`Serif]}
    {- ["Courier"], returns [`Fixed]}
    {- Any other, returns [`Sans]}}
    See {!text} for understanding what this entails. *)

(** {1:target PDF render targets} *)

val target :
  ?font:(Vg.font -> font) -> ?xmp:string -> unit ->
  Vg.Vgr.dst_stored Vg.Vgr.target
(** [target font xmp ()] is a PDF render target for rendering to the stored
    destination given to {!Vg.Vgr.create}.
    {ul
    {- [font] is the font resolver, defaults to {!val:font}, see {!text} for
       details. Note that [font] results are cached by the renderer.}
    {- [xmp] is an optional UTF-8 encoded XML XMP metadata packet describing
       the PDF document (see ISO 16684-1 or the equivalent
        {{:http://www.adobe.com/devnet/xmp.html}Adobe spec.}).
       The convenience function {!Vg.Vgr.xmp} can be used to
       generate a packet.}}

    {b Multiple image.} Multiple image render is supported. Each image
    defines a page of the resulting PDF file. *)

(** {1:text Text rendering}

    Text rendering depends on the way fonts are resolved by the
    function specified in the rendering {!target}. Given a glyph
    cut:

{!Vg.I.cut_glyphs}[ ~text ~blocks ~advances font glyphs]

    First, if the optional [text] and [blocks] arguments are
    specified, they are always used to map the rendered glyphs to
    text for PDF text extraction. Then the following happens
    according to the resolution of the [font] argument by the render
    target:
    {ul
    {- [`Otf otf], the values in [glyphs] are glyph indexes of
       the OpenType font [otf]. If [advances] is specified these vectors
       are used to position the glyphs (e.g. you need to use this to perform
       kerning), otherwise the font's glyph advances, as found in [otf], are
       used.}
    {- [`Helvetica], uses one of the standard PDF font Helvetica,
       Helvetica-Bold, Helvetica, Helvetica-Bold, Helvetica-Oblique,
       Helvetica-BoldOblique according to [font]'s {!Vg.Font.slant} and
       {!Vg.Font.weight}. The values in [glyphs] are glyph indexes
       representing the corresponding Unicode character (e.g. glyph
       index [0x20] is the glyph for character [U+0020]). The font
       supports glyph indexes for all the characters listed in the in
       the second column of
   {{:http://www.unicode.org/Public/MAPPINGS/VENDORS/MICSFT/WINDOWS/CP1252.TXT}
       this document} which is all the Basic Latin block, the Latin-1
       Supplement block without its control characters and some
       additional characters (those at rows 0x80-0x9F in that document).
       If a glyph index is not supported it is replaced by [0]. If
       [advances] is specified these vectors are used to position the glyphs,
       otherwise the internal font's glyphs advances are used.}
    {- [`Times], same as [`Helvetica] but uses one of the standard PDF font
       Times-Roman, Times-Bold, Times-Italic or Times-BoldItalic.}
    {- [`Courier], same as [`Helvetica] but uses one of the standard PDF font
       Courier, Courier-Bold, Courier-Oblique, Courier-BoldOblique.}
    {- [`Sans] is the same as [`Helvetica] except [advances] and [glyphs]
       are {b ignored}. Instead the UTF-8 string [text] is used to generate
       a corresponding [glyphs] list, mapping supported characters as mentioned
       above to their corresponding glyph. Unsupported characters are
       mapped to glyph 0.}
    {- [`Serif] same as [`Sans] but uses the same fonts as [`Times].}
    {- [`Fixed] same as [`Sans] but uses the same fonts as [`Courier].}}

    So what should be used ? In general clients should use a font
    resolution mechanism independent from [Vg] in order to get an
    OpenType font [otf] for [font]. Using this [otf] font is should
    compute, using again a mechanism independent from [Vg], a glyph
    layout resulting in [advances] and [glyphs] to use with
    {!Vg.I.cut_glyphs} and finally resolve [font] in the target with
    [`Otf otf]. This means that font resolution should:
    {ul
    {- Use [`Otf otf] whenever it is guaranteed that the glyph indexes
       in [glyphs] actually correspond to the glyph indexes of [otf].}
    {- Use [`Sans], [`Serif] or [`Fixed] whenever its is unable to resolve
       [font] to an appropriate [otf], this may not result in the expected
       rendering but still at least show (the latin) part of the text.}
    {- Use [`Helvetica], [`Times] or [`Courier] to perform glyph
       layout using PDF's standard fonts without having to embed the fonts in
       the PDF (the font metrics can be downloaded
    {{:https://www.adobe.com/devnet/font.html}here}).
       PDFs without embedded fonts are however not recommended.}}  *)

(** {1:limits Render warnings and limitations}

    The following render warnings are reported.
    {ul
    {- [`Unsupported_cut (`O o, i)], outline area cuts can be performed only
       on (possibly transformed) {!Vg.I.const}, {!Vg.I.axial}, {!Vg.I.radial}
       images.}
    {- [`Unsupported_glyph_cut (`O o, i)], outline area glyph cuts can
       be performed only on (possibly transformed) {!Vg.I.const},
       {!Vg.I.axial}, {!Vg.I.radial} images.}}

    The following limitations should be taken into account:
    {ul
    {- Streams in the PDF files are currently uncompressed and fonts
       are embedded without subsetting which may result in large file
       sizes. This will be lifted in future versions of the
       library. Meanwhile if you need to reduce the size of generated
       PDFs you can pass them through
       {{:http://community.coherentpdf.com}cpdf} or
       {{:http://www.ghostscript.com}ghostscript}.
{v
> cpdf -compress -o output.pdf input.pdf
> gs -dNOPAUSE -dBATCH -sDEVICE=pdfwrite -sOutputFile=output.pdf input.pdf
v}}}
*)

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
