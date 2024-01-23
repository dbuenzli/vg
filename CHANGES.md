
- The `Vgr_svg` module is now part of the `vg` library. 
  The `vg.svg` library is deprecated, it warns on usage
  and simply requires `vg`.
- Reworked documentation into `.mld` pages.
- Add `Vg.P.smooth_{ccurve,qcurve}` to smoothly stitch cubic and
  quadratic Bézier curves. Thanks to François Thiré for the patch
  (#33).
- `Vgr_htmlc` is now implemented via `brr` which becomes an optional
  dependency of the package. The mandatory `js_of_ocaml` and
  `js_of_ocaml-ppx` dependencies are dropped.
- `Vgr_htmlc.screen_resolution` is now a function taking unit. This 
  allows the safe (but useless) linking of `Vgr_htmlc` in a web workers.
- Fix `Vgr_pdf` glyph cut rendering. All glyphs id of the form `0xHH0D`
  were rendered as id `0xHH0A`. The text of the 2008 standard of the
  `Tj` operator (§9.4.3) misleads, PDF strings do perform newline
  normalisation (§7.3.4.2) so `0D` bytes also need to be escaped.
  
v0.9.4 2020-05-28 La Forclaz (VS)
---------------------------------

- jsoo 3.6.0 support.

v0.9.3 2019-06-14 Zagreb
------------------------

- Fix 4.08 `Pervasives`' deprecation.
- jsoo 3.3.0 support. Thanks to @monstasat for the patch.

v0.9.2 2018-11-02 Zagreb
------------------------

- Fix bug in `cairo2` backend. The initial clip region and clear
  was not done correctly.
- Require `cairo2` 0.6.
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
