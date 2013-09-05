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

    {e Release %%VERSION%% - %%MAINTAINER%% } *)

(** {1:target PDF render targets} *)

type font = [ `Otf of string | `Serif | `Sans | `Fixed ]
(** The type for PDF fonts. Either an OpenType file ([`Otf]) or
    a fallback font that uses the PDF standard fonts, see {{!text}details}. *)

val target : ?font:(Vg.font -> font) -> ?share:int -> ?xmp:string -> 
  unit -> Vg.Vgr.dst_stored Vg.Vgr.target
(** [target share xmp ()] is a PDF render target for rendering to the stored
    destination given to {!Vg.Vgr.create}. 
    {ul 
    {- [font] is the font resolver, given a {!Vg.font} [f] it must 
       return the bytes of a corresponding OpenType font file (which will
       most likely be independent of the size [Vg.Font.size f]).}
    {- [share] indicates the number of consecutive pages that share
       resources. If unspecified all pages share resources. TODO 
       now that the compilation strategy changed it may not make 
       much sence to expose that.}
    {- [xmp] is an optional UTF-8 encoded XML XMP metadata packet describing
       the SVG document (see ISO 16684-1 or the equivalent
        {{:http://www.adobe.com/devnet/xmp.html}Adobe spec.}). 
       The convenience function {!Vg.Vgr.xmp_metadata} can be used to 
       generate a packet.}}

    {b Multiple image.} Multiple images render is supported. Each image
    defines a page of the resulting PDF file.

    @raise Invalid_argument if [share] is not strictly positive. *)

(** {1:text Text rendering support} 

    If the [font] resolver of the target returns something different 
    than [`Otf], the following fonts are used:
    {ul 
    {- For [`Serif], one of Times-Roman, Times-Bold, Times-Italic or 
       Times-BoldItalic, according to the font's slant and weight.}
    {- For [`Sans], one of Helvetica, Helvetica-Bold, Helvetica-Oblique, 
       Helvetica-BoldOblique, according the font's slant and weight.}
    {- For [`Fixed], one of Courier, Courier-Bold, Courier-Oblique, 
       Courier-BoldOblique, according to the font's slant and weight.}}    

    In that case the glyph values given to {!I.cut_glyphs} are
    interpreted as an
    {{:http://unicode.org/glossary/#Unicode_scalar_value}Unicode
    scalar value} which map to glyphs for the corresponding Unicode
    scalar value. The supported set of glyph values are those listed
    in the second column of
    {{:http://www.unicode.org/Public/MAPPINGS/VENDORS/MICSFT/WINDOWS/CP1252.TXT}
    this document}, basically this is ISO-8859-1 (latin1) with some
    additional characters (see those corresponding in the rows 0x80-0x9F). 
    Any other glyph value is substituted by glyph 0. *)

(** {1:limits Render warnings and limitations}

    The page content streams in the PDF files are currently uncompressed. 
    This will be lifted in future versions of the library. If you need to 
    reduce the size generated PDF you can for example filter it through 
    {{:http://www.ghostscript.com}ghostscript} with:
{[
gs -dNOPAUSE -dBATCH -sDEVICE=pdfwrite -sOutputFile=output.pdf input.pdf
]}
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
