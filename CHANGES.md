
- Require OCaml 4.03.
- Deprecate `Vg.(>>)`. Use OCaml's stdlib's `|>` operator instead.

v0.9.1 2017-12-20 La Forclaz (VS)
---------------------------------

- Fix a stackoverlow in the SVG renderer. Thanks to Guyslain Naves for
  the report.

v0.9.0 2015-11-25 Zagreb
------------------------

- Automated migration from camlp4 to ppx. Many thanks to the authors
  of camlp4-to-ppx.
- Use standard library `result` type. This changes the dubious interface
  of `Vgr_pdf.otf_font`.
- Support uutf v1.0.0 and otfm v0.3.0.
- Build depend on topkg.
- Relicense from BSD3 to ISC.

v0.8.2 2015-08-14 Cambridge (UK)
--------------------------------

- Add `Vgr_cairo` module. A Cairo backend contributed by Arthur Wendling.
- `-safe-string` support. In the public API this only affects users of
  stored `Manual` rendering destination: `Vg.Vgr.Manual.dst` now takes
  a `bytes` value instead of a `string`.


v0.8.1 2014-08-23 Cambridge (UK)
--------------------------------

- Use package builder topkg for distribution.
- Fix build and installation glitches. Thanks to Philippe Veber and
  Grégoire Lionnet for the reports.
- Gg 0.9.0 compatibility.
- Add `Vgr_htmlc.screen_resolution` value.
- `Vgr_htmlc.target` default value for `resolution` argument is now the
  screen resolution rather than 300ppi.
- `Vgr_htmlc.target` add a `resize` optional argument. When set to
  `false` the canvas size is kept intact and doesn't resize according
  to renderable sizes.

v0.8.0 2013-09-24 Lausanne
--------------------------

First release.
Sponsored by Citrix Systems R&D and OCaml Labs.
