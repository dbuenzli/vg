Vg — Declarative 2D vector graphics for OCaml
=============================================

Vg is a declarative 2D vector graphics library. Images are values that
denote functions mapping points of the cartesian plane to colors and
combinators are provided to define and compose them.

Renderers for PDF, SVG, Cairo and the HTML canvas are distributed with the
module. An API allows to implement new renderers.

Vg is distributed under the ISC license. Vg and the SVG renderer
depend on [Gg]. The PDF renderer depends on [Otfm], the HTML canvas
renderer depends on [Brr], the Cairo renderer depends on [cairo2].
     
[Gg]: http://erratique.ch/software/gg
[Otfm]: http://erratique.ch/software/otfm
[Brr]: http://erratique.ch/software/brr
[cairo2]: https://github.com/Chris00/ocaml-cairo

Home page: http://erratique.ch/software/vg  

## Installation

Vg can be installed with `opam`:

    opam install vg                   # SVG renderer only
    opam install brr cairo2 otfm vg   # All renderers
    
If you don't use `opam` consult the [`opam`](opam) file for
build instructions and a complete specification of the dependencies.

## Documentation

The documentation can be consulted [online] or via `odig doc vg`.

Questions are welcome but better asked on the [OCaml forum] than on
the issue tracker.

[online]: http://erratique.ch/software/vg/doc/
[OCaml forum]: https://discuss.ocaml.org/

## Sample programs

A database of test images can be found in the `test/db` directory.  An
online rendering of the database with the different backends and links
to the source of images is available [here][online-db]

A few test programs and minimal rendering examples can be found in
the [`test`][test] directory, see `b0 list`.

[online-db]: http://erratique.ch/software/vg/demos/db_viewer.html
