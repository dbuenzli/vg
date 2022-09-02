(*---------------------------------------------------------------------------
   Copyright (c) 2013 The vg programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** Vg HTML canvas renderer.

    {b References.}
    {ul {- Rik Cabanier et al. {e {{:http://www.w3.org/TR/2dcontext/}HTML
     Canvas 2D Context}}, 2012-12-17.}} *)

(** {1:target HTML canvas render targets} *)

val screen_resolution : Gg.v2
(** [screen_resolution] is the screen resolution in pixels per meters. *)

val target : ?resize:bool -> ?resolution:Gg.v2
  -> Js_of_ocaml.Dom_html.canvasElement Js_of_ocaml.Js.t
  -> [`Other] Vg.Vgr.target
(** [target resize resolution c] is a render target for rendering to the
    canvas element [c].
    {ul
    {- [resize] if [true] (default) sets the canvas CSS size to the
       physical size of {{!Vg.Vgr.renderable}renderables}. If [false]
       the physical size of renderables is ignored and the view
       rectangle is simply mapped on the canvas CSS size at the given
       resolution but {b WARNING} for this to work do not use any
       CSS [border] on the canvas element ([outline] can be used though)
       as it interacts badly with the underlying canvas size computation
       made by [Vgr_htmlc] (a typical symptom will be a vertically growing
       canvas on redraws, this seems to happen regardless of [box-sizing]).}
    {- [resolution], specifies the rendering resolution in samples per
       meters. If unspecified the {!screen_resolution} is used in both
       dimensions.}}

    {b Multiple images.} Multiple images render on the target is supported.
    Each new render clears the HTML canvas. *)

(** {1:text Text rendering}

    Text rendering uses the HTML canvas CSS font selection mechanism.
    As there is no control over glyph rendering in the HTML canvas,
    the glyph API is unsupported.

    Given a glyph cut:

{!Vg.I.cut_glyphs}[ ~text ~blocks ~advances font glyphs]

    The [blocks], [advances] and [glyphs] parameters are ignored.
    [text] must be provided and is used to define the text to render.
    [font] is used to select the font in the CSS stylesheet. Make sure
    that the fonts you use are embedded and {b loaded} in your DOM via
    [\@font-face].

    At the moment the renderer also needs to work around a particular
    browser bug which means that glyph cuts are currently limited to
    non-outline area cuts in {!I.const} images.  *)

(** {1:limits Render warnings and limitations}

    The following render warnings are reported.
    {ul
    {- [`Unsupported_cut ((`O o), i)], outline area cuts can be performed
       only on (possibly transformed) {!I.const}, {!I.axial} and {!I.radial}
       primitive images.}
    {- [`Unsupported_glyph_cut (a, i)], glyph cuts can be performed only
       on bare {!I.const} primitive images and outline area glyph cuts are
       currently unsupported.}
    {- [`Textless_glyph_cut i] if no [text] argument is specified in a glyph
       cut.}
    {- [`Other _] if dashes are rendered but unsupported by the browser.}}

    The following limitations should be taken into account.
    {ul
    {- The even-odd area rule is supported according to the
       latest whatwg spec. This may not work in all browsers.}
    {- In the HTML canvas gradient color interpolation is performed
       in (non-linear) sRGB space. This doesn't respect Vg's semantics.}} *)

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
