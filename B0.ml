open B0_kit.V000

(* OCaml library names *)

let brr = B0_ocaml.libname "brr"
let cairo = B0_ocaml.libname "cairo2"
let gg = B0_ocaml.libname "gg"
let otfm = B0_ocaml.libname "otfm"
let unix = B0_ocaml.libname "unix"

let vg = B0_ocaml.libname "vg"
let vg_cairo = B0_ocaml.libname "vg.cairo"
let vg_htmlc = B0_ocaml.libname "vg.htmlc"
let vg_pdf = B0_ocaml.libname "vg.pdf"

(* Libraries *)

let vg_lib =
  let srcs = [`Dir ~/"src"] and requires = [gg] in
  B0_ocaml.lib vg ~srcs ~requires

let vg_pdf_lib =
  let srcs = [`Dir ~/"src/pdf"] and requires = [gg; vg; otfm] in
  B0_ocaml.lib vg_pdf ~srcs ~requires

let vg_htmlc_lib =
  let srcs = [`Dir ~/"src/htmlc"] and requires = [gg; vg; brr] in
  B0_ocaml.lib vg_htmlc ~srcs ~requires

let vg_cairo_lib =
  let srcs = [`Dir ~/"src/cairo"] and requires = [gg; vg; cairo] in
  B0_ocaml.lib vg_cairo ~srcs ~requires

(* Tests *)

let test ?more_srcs:(srcs = []) ?(requires = []) ?doc base =
  let srcs = `File (Fpath.fmt "test/%s.ml" base) :: srcs in
  let requires = gg :: vg :: requires in
  B0_ocaml.exe base ?doc ~srcs ~requires

let test_jsoo ?more_srcs:(srcs = []) ?(requires = []) ?doc base =
  let p = Fpath.fmt "test/%s" base in
  let srcs = `File Fpath.(p + ".ml") :: `File Fpath.(p + ".html") :: srcs in
  let requires = gg :: vg :: requires in
  B0_jsoo.html_page base ?doc ~srcs ~requires

let test_min_svg =
  let doc = "Minimal SVG rendering example" in
  test "min_svg" ~doc

let test_min_pdf =
  let doc = "Minimal PDF rendering example" in
  test "min_pdf" ~doc ~requires:[vg_pdf]

let test_min_htmlc =
  let doc = "Minimal HTML canvas rendering example" in
  test_jsoo "min_htmlc" ~doc ~requires:[brr; vg_htmlc]

let test_min_cairo =
  let doc = "Minimal cairo rendering to PNG example" in
  test "min_cairo_png" ~doc ~requires:[cairo; vg_cairo;]

let test_min_cairo_mem =
  let doc = "Minimal cairo to memory rendering example" in
  test "min_cairo_mem" ~doc ~requires:[cairo; vg_cairo]

let test_examples =
  test "examples" ~doc:"Examples for the docs"

let test_font_glyphs =
  let doc = "Render a font's repertoire to PDF (without the glyph API)" in
  test "font_glyphs" ~requires:[otfm; vg_pdf] ~doc

let test_sqc =
  let doc = "Square circle spiral illusion" in
  test_jsoo "sqc" ~requires:[brr; vg_htmlc] ~doc

let test_vecho =
  let doc = "An echo(3) producing PDF banners" in
  test "vecho" ~doc ~requires:[otfm; vg_pdf]

(* Vg test image database. *)

let db_srcs = `Dir ~/"test/db"
let sdb = [db_srcs; `File ~/"test/test_vgr_stored.ml"]

let test_vgr_svg =
  let doc = "Renders test images with Vgr_svg" in
  test "test_vgr_svg" ~doc ~requires:[unix] ~more_srcs:sdb

let test_vgr_cairo =
  let doc = "Renders test images with Vgr_cairo" in
  test "test_vgr_cairo" ~doc ~requires:[vg_cairo] ~more_srcs:sdb

let test_vgr_pdf =
  let doc = "Renders test images with Vgr_pdf" in
  test "test_vgr_pdf" ~doc ~requires:[unix; otfm; vg_pdf] ~more_srcs:sdb

let db_viewer =
  let doc = "Test image browser viewer" in
  let more_srcs = [`File ~/"test/mui.ml"; `File ~/"test/mui.mli"; db_srcs ] in
  let requires = [otfm; brr; vg_pdf; vg_htmlc] in
  test_jsoo "db_viewer" ~doc ~requires ~more_srcs

(* Packs *)

let default =
  let meta =
    B0_meta.empty
    |> ~~ B0_meta.authors ["The vg programmers"]
    |> ~~ B0_meta.maintainers ["Daniel Bünzli <daniel.buenzl i@erratique.ch>"]
    |> ~~ B0_meta.homepage "https://erratique.ch/software/vg"
    |> ~~ B0_meta.online_doc "https://erratique.ch/software/vg/doc"
    |> ~~ B0_meta.licenses ["ISC"]
    |> ~~ B0_meta.repo "git+https://erratique.ch/repos/vg.git"
    |> ~~ B0_meta.issues "https://github.com/dbuenzli/vg/issues"
    |> ~~ B0_meta.description_tags
      ["pdf"; "svg"; "canvas"; "cairo"; "browser"; "declarative"; "graphics";
       "org:erratique"; ]
    |> ~~ B0_opam.build
      {|[["ocaml" "pkg/pkg.ml" "build" "--dev-pkg" "%{dev}%"
          "--with-brr" "%{brr:installed}%"
          "--with-cairo2" "%{cairo2:installed}%"
          "--with-otfm" "%{otfm:installed}%"]]|}
    |> ~~ B0_opam.depopts
      [ "brr", "";
        "cairo2", "";
        "otfm", "";]
    |> ~~ B0_opam.conflicts
      [ "brr", {|< "0.0.6"|};
        "cairo2", {|< "0.6"|};
        "otfm", {|< "0.3.0"|} ]
    |> ~~ B0_opam.depends
      [ "ocaml", {|>= "4.14.0"|};
        "ocamlfind", {|build|};
        "ocamlbuild", {|build|};
        "topkg", {|build & >= "1.0.3"|};
        "gg", {|>= "1.0.0"|}; ]
    |> B0_meta.tag B0_opam.tag

  in
  B0_pack.make "default" ~doc:"vg package" ~meta ~locked:true @@
  B0_unit.list ()
