Vg — Declarative 2D vector graphics for OCaml
=============================================

Vg is a declarative 2D vector graphics library. Images are values that
denote functions mapping points of the cartesian plane to colors and
combinators are provided to define and compose them.

Renderers for PDF, SVG, Cairo and the HTML canvas are distributed with the
module. An API allows to implement new renderers.
     
Vg is distributed under the ISC license. Vg and the SVG renderer
depend on [Gg]. The PDF renderer depends on [Uutf] and [Otfm], the
HTML canvas renderer depends on [Brr], the Cairo renderer depends on
[cairo2].
     
[Gg]: http://erratique.ch/software/gg
[Uutf]: http://erratique.ch/software/uutf
[Otfm]: http://erratique.ch/software/otfm
[Brr]: http://erratique.ch/software/brr
[cairo2]: https://github.com/Chris00/ocaml-cairo

Home page: http://erratique.ch/software/vg  

## Installation

Vg can be installed with `opam`:

    opam install vg                       # SVG renderer only
    opam install uutf otfm brr cairo2 vg  # all renderers
    
If you don't use `opam` consult the [`opam`](opam) file for
build instructions and a complete specification of the dependencies.

## Documentation

The documentation can be consulted [online] or via `odig doc vg`.

Questions are welcome but better asked on the [OCaml forum] than on
the issue tracker.

[online]: http://erratique.ch/software/vg/doc/
[OCaml forum]: https://discuss.ocaml.org/

## Sample programs and images

If you install Vg with `opam` sample programs are located in the
directory `opam var vg:doc`.

A database of sample images can be found in the `db` directory of the
distribution. An online rendering of the database is available
[here][online-db].

[online-db]: http://erratique.ch/software/vg/demos/rhtmlc.html

Sample programs are located in the `test` directory of the
distribution. They can be built and listed with:

    topkg build --tests true && topkg test --list

- `min_pdf.native`, minimal example to render an image to a PDF file. 
- `min_svg.native`, minimal example to render an image to an SVG file. 
- `min_htmlc.byte`, minimal example to render with the HTML canvas.
- `rsvg.native`, renders images of the Vg image database to SVG files.
- `rpdf.native`, renders images of the Vg image database to PDF files.
- `rcairo.native`, renders images of the Vg image database with Cairo
   to PDF, PNG, PS or SVG files.
- `rhtmlc.html` and `rhtmlc.byte` can be processed with `js_of_ocaml`,
   the resulting webapp renders images of the Vg image database with
   the HTML canvas, PDF and SVG renderers.   
- `vecho.native`, like echo(1) but produces a PDF file on stdout, the 
  font file can be specified, invoke with `-help` for options. 
- `fglyphs.native`, renders a font's glyphs to a PDF file (without using
  Vg's glyph API).
