(*---------------------------------------------------------------------------
   Copyright 2013 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
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
