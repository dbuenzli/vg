open B0_kit.V000

(* OCaml library names *)

let brr = B0_ocaml.libname "brr"
let cairo = B0_ocaml.libname "cairo2"

let gg = B0_ocaml.libname "gg"
let otfm = B0_ocaml.libname "otfm"
let unix = B0_ocaml.libname "unix"
let uutf = B0_ocaml.libname "uutf"

let vg = B0_ocaml.libname "vg"
let vg_cairo = B0_ocaml.libname "vg.cairo"
let vg_htmlc = B0_ocaml.libname "vg.htmlc"
let vg_pdf = B0_ocaml.libname "vg.pdf"
let vg_svg = B0_ocaml.libname "vg.svg"

(* Libraries *)

let mod_srcs m =
  let mli = Fmt.str "src/%s.mli" m and ml = Fmt.str "src/%s.ml" m in
  Fpath.[ `File (v mli); `File (v ml) ]

let vg_lib =
  let srcs = mod_srcs "vg" in
  B0_ocaml.lib vg ~doc:"The vg library" ~srcs ~requires:[gg]

let vg_pdf_lib =
  let srcs = mod_srcs "vgr_pdf" in
  let requires = [vg; gg; otfm; uutf] in
  B0_ocaml.lib vg_pdf ~doc:"The vg.pdf library" ~srcs ~requires

let vg_svg_lib =
  let srcs = mod_srcs "vgr_svg" in
  B0_ocaml.lib vg_svg ~doc:"The vg.svg library" ~srcs ~requires:[vg; gg]

let vg_htmlc_lib =
  let srcs = mod_srcs "vgr_htmlc" in
  let requires = [vg; gg; brr] in
  B0_ocaml.lib vg_htmlc ~doc:"The vg.htmlc library" ~srcs ~requires

let vg_cairo_lib =
  let srcs = mod_srcs "vgr_cairo" in
  let requires = [vg; gg; cairo] in
  B0_ocaml.lib vg_cairo ~doc:"The vg.cairo library" ~srcs ~requires

(* Tests *)

let test ?more_srcs:(srcs = []) ?(requires = []) ?doc base =
  let requires = gg :: vg :: requires in
  let srcs = `File (Fpath.fmt "test/%s.ml" base) :: srcs in
  B0_ocaml.exe base ?doc ~srcs ~requires

let test_jsoo ?more_srcs:(srcs = []) ?(requires = []) ?doc base =
  let requires = gg :: vg :: requires in
  let p = Fpath.fmt "test/%s" base in
  let srcs = `File Fpath.(p + ".ml") :: `File Fpath.(p + ".html") :: srcs in
  B0_jsoo.html_page base ?doc ~requires ~srcs

let test_min_svg =
  let doc = "Minimal SVG rendering example" in
  test "min_svg" ~doc ~requires:[vg_svg]

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
  let doc = "Examples for the docs" in
  test "examples" ~requires:[vg_svg] ~doc

let test_fglyphs = test "fglyphs" ~requires:[vg_pdf; otfm]
let test_sqc = test_jsoo "sqc" ~requires:[vg_htmlc; brr]
let test_vecho = test "vecho" ~requires:[uutf; otfm; vg_pdf]

(* Vg image test database. *)

let db_srcs = `Dir ~/"db"
let rdb = [db_srcs; `File ~/"test/rstored.ml"]

let test_rsvg =
  let doc = "Renders sample image database with Vgr_svg" in
  test "rsvg" ~doc ~requires:[unix; uutf; vg_svg] ~more_srcs:rdb

let test_rcairo =
  let doc = "Renders sample image database with Vgr_cairo" in
  test "rcairo" ~doc ~requires:[uutf; vg_cairo] ~more_srcs:rdb

let test_rpdf =
  let doc = "Renders sample image database with Vgr_rpdf" in
  test "rpdf" ~doc ~requires:[unix; uutf; otfm; vg_pdf] ~more_srcs:rdb

let test_rhtmlc =
  let doc = "Renders sample image database in the browser" in
  let more_srcs = [`File ~/"test/mui.ml"; `File ~/"test/mui.mli"; db_srcs ] in
  let requires = [uutf; otfm; brr; vg_svg; vg_pdf; vg_htmlc] in
  test_jsoo "rhtmlc" ~doc ~requires ~more_srcs

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
          "--with-uutf" "%{uutf:installed}%"
          "--with-otfm" "%{otfm:installed}%"
          "--with-js_of_ocaml" "%{js_of_ocaml:installed}%"
          "--with-cairo2" "%{cairo2:installed}%"]]|}
    |> ~~ B0_opam.depopts
      [ "uutf", "";
        "otfm", "";
        "cairo2", "" ]
    |> ~~ B0_opam.conflicts
      [ "otfm", {|< "0.3.0"|};
        "cairo2", {|< "0.6"|};
        "uutf", {|< "1.0.0"|}; ]
    |> ~~ B0_opam.depends
      [ "ocaml", {|>= "4.08.0"|};
        "ocamlfind", {|build|};
        "ocamlbuild", {|build|};
        "topkg", {|build & >= "1.0.3"|};
        "gg", {|>= "0.9.0"|};
        "js_of_ocaml", {|>= "3.6.0"|};
        "js_of_ocaml-compiler", {|>= "3.6.0"|};
        "js_of_ocaml-ppx", {|>= "3.6.0"|};]
    |> B0_meta.tag B0_opam.tag

  in
  B0_pack.make "default" ~doc:"vg package" ~meta ~locked:true @@
  B0_unit.list ()
