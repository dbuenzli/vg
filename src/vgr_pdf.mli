(*---------------------------------------------------------------------------
   Copyright 2013 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Vg PDF renderer. 

    Renders a sequence of renderables as a multi-page PDF 1.7
    document. Each renderable defines a page of the document.
    
    {b Bug reports.}  PDF being an insane standard, rendering
    abilities of PDF readers vary wildly. No rendering bug report for
    this renderer will be considered if it cannot be reproduced in the
    latest Adobe Acrobat Reader.    

    {e Release %%VERSION%% — %%MAINTAINER%% } *)

(** {1:fonts Font resolution} 

    Font resolution happens during the rendering of {!Vg.I.cut_glyphs} 
    images through the [font] callback given to the PDF rendering {!target}. 
    See {!text} for more details. *) 

type otf_font 
(** The type for OpenType fonts. *)

val otf_font : string -> [ `Otf of otf_font | `Error of Otfm.error ]
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
    following resolutions according to the result of {!Vg.Font.name}[ f]: 
    {ul 
    {- ["Helvetica"], returns [`Sans]}
    {- ["Times"], returns [`Serif]}
    {- ["Courier"], returns [`Fixed]}
    {- Any other, returns [`Sans]}} 
    See {!text} for understanding what this entails. *)

(** {1:target PDF render targets} *)

val target : ?font:(Vg.font -> font) -> ?xmp:string -> 
  unit -> Vg.Vgr.dst_stored Vg.Vgr.target
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

    {b Multiple image.} Multiple images render is supported. Each image
    defines a page of the resulting PDF file. *)

(** {1:text Text rendering} 

    Text rendering depends on the way fonts are resolved by the
    function specified in the rendering {!target}. Given a glyph
    cut:

{!Vg.I.cut_glyphs}[ ~text ~advances font glyphs]

    First, if the optional [text] argument is specified, it is always used to 
    map the rendered glyphs to this UTF-8 text for PDF text extraction. Then 
    the following happens according to the resolution of the [font] argument 
    by the render target:
    {ul 
    {- [`Otf otf], the values in [glyphs] are glyph indexes of 
       the OpenType font [otf]. If [advances] is specified these vectors
       are used to position the glyphs (e.g. you need to use this to perform 
       kerning), otherwise the font's glyph widths, as found in [otf], are 
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
       otherwise the internal font's glyphs width are used.}
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
    {{:http://partners.adobe.com/public/developer/font/index.html#pcfi}here}).
       PDFs without embedded fonts are however not recommended.}}  *)

(** {1:limits Render warnings and limitations}

    {ul
    {-  The page content streams in the PDF files are currently uncompressed. 
        This will be lifted in future versions of the library. If you need to 
        reduce the size of generated PDFs you can, for example, filter them 
        through {{:http://www.ghostscript.com}ghostscript} with:
{[
gs -dNOPAUSE -dBATCH -sDEVICE=pdfwrite -sOutputFile=output.pdf input.pdf
]}}}
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
