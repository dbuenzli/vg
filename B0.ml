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

let test ?(more_srcs = []) ?doc base ~requires =
  let srcs = Fpath.(`File (v (Fmt.str "test/%s.ml" base))) :: more_srcs in
  B0_ocaml.exe base ?doc ~srcs ~requires

let db = `Dir (Fpath.v "db")

let test_min_svg = test "min_svg" ~requires:[vg; gg; vg_svg]
let test_min_pdf = test "min_pdf" ~requires:[vg; gg; vg_pdf]
let test_min_cairo = test "min_cairo_png" ~requires:[vg; gg; cairo; vg_cairo;]
let test_min_cairo_mem =
  test "min_cairo_mem" ~requires:[vg; gg; cairo; vg_cairo]

let r_srcs = [db; `File (Fpath.v "test/rstored.ml")]

let test_rsvg =
  test "rsvg" ~requires:[unix; vg; gg; uutf; vg_svg] ~more_srcs:r_srcs

let test_rpdf =
  test "rpdf" ~requires:[unix; vg; gg; uutf; otfm; vg_pdf] ~more_srcs:r_srcs

let test_rcairo =
  test "rcairo" ~requires:[vg; gg; uutf; vg_cairo] ~more_srcs:r_srcs

let test_examples = test "examples" ~requires:[vg; gg; vg_svg]
let test_fglyphs = test "fglyphs" ~requires:[vg; gg; vg_pdf; otfm]

let test_jsoo ?(more_srcs = []) ?requires ?doc base =
  let srcs =
    `File ~/(Fmt.str "test/%s.ml" base) ::
    `File ~/(Fmt.str "test/%s.html" base) :: more_srcs
  in
  B0_jsoo.web base ?requires ~srcs ?doc

let test_min_htmlc = test_jsoo "min_htmlc" ~requires:[vg; gg; vg_htmlc; brr]
let test_sqc = test_jsoo "sqc" ~requires:[vg; gg; vg_htmlc; brr]
let test_rhtmlc =
  let more_srcs = [`File ~/"test/mui.ml"; `File ~/"test/mui.mli"; db ] in
  let requires = [gg; vg; vg_svg; vg_pdf; vg_htmlc; uutf; otfm; brr] in
  test_jsoo "rhtmlc" ~requires ~more_srcs

(* Packs *)

let default =
  let meta =
    B0_meta.empty
    |> B0_meta.(add authors) ["The vg programmers"]
    |> B0_meta.(add maintainers)
       ["Daniel Bünzli <daniel.buenzl i@erratique.ch>"]
    |> B0_meta.(add homepage) "https://erratique.ch/software/vg"
    |> B0_meta.(add online_doc) "https://erratique.ch/software/vg/doc"
    |> B0_meta.(add licenses) ["ISC"]
    |> B0_meta.(add repo) "git+https://erratique.ch/repos/vg.git"
    |> B0_meta.(add issues) "https://github.com/dbuenzli/vg/issues"
    |> B0_meta.(add description_tags)
      ["pdf"; "svg"; "canvas"; "cairo"; "browser"; "declarative"; "graphics";
       "org:erratique"; ]
    |> B0_meta.tag B0_opam.tag
    |> B0_meta.add B0_opam.build
      {|[["ocaml" "pkg/pkg.ml" "build" "--dev-pkg" "%{dev}%"
          "--with-uutf" "%{uutf:installed}%"
          "--with-otfm" "%{otfm:installed}%"
          "--with-js_of_ocaml" "%{js_of_ocaml:installed}%"
          "--with-cairo2" "%{cairo2:installed}%"]]|}
    |> B0_meta.add B0_opam.depopts
      [ "uutf", "";
        "otfm", "";
        "cairo2", "" ]
    |> B0_meta.add B0_opam.conflicts
      [ "otfm", {|< "0.3.0"|};
        "cairo2", {|< "0.6"|};
        "uutf", {|< "1.0.0"|}; ]
    |> B0_meta.add B0_opam.depends
      [ "ocaml", {|>= "4.08.0"|};
        "ocamlfind", {|build|};
        "ocamlbuild", {|build|};
        "topkg", {|build & >= "1.0.3"|};
        "gg", {|>= "0.9.0"|};
        "js_of_ocaml", {|>= "3.6.0"|};
        "js_of_ocaml-compiler", {|>= "3.6.0"|};
        "js_of_ocaml-ppx", {|>= "3.6.0"|};]

  in
  B0_pack.make "default" ~doc:"logs package" ~meta ~locked:true @@
  B0_unit.list ()
