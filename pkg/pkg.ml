#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let uutf = Conf.with_pkg "uutf"
let otfm = Conf.with_pkg "otfm"
let jsoo = Conf.with_pkg "js_of_ocaml"
let cairo2 = Conf.with_pkg "cairo2"

let jsoo_test ~cond test =
  Pkg.flatten
    [ Pkg.test ~run:false ~cond ~auto:false (test ^ ".js");
      Pkg.test ~run:false ~cond ~auto:false (test ^ ".html"); ]

let doc_images () =
  let is_dir p = OS.Dir.exists p |> Log.on_error_msg ~use:(fun _ -> false) in
  let skip p = not (Fpath.has_ext ".png" p) && not (is_dir p) in
  let mv acc p = (Pkg.doc ~built:false p ~dst:"odoc-assets/") :: acc in
  OS.File.fold ~skip (fun p acc -> p :: acc) [] ["doc"]
  >>= fun files -> Ok (Pkg.flatten (List.fold_left mv [] files))

let build =
  let cmd c os files =
    let jsoo = Cmd.(v "-plugin-tag" % "package(js_of_ocaml.ocamlbuild)") in
    OS.Cmd.run @@ Cmd.(Pkg.build_cmd c os %% jsoo %% of_list files)
  in
  Pkg.build ~cmd ()

let publish =
  Pkg.publish ~artefacts:[`Doc; `Distrib; `Alt "www-demos"] ()


let () =
  Pkg.describe "vg" ~build ~publish @@ fun c ->
  let uutf = Conf.value c uutf in
  let otfm = Conf.value c otfm in
  let jsoo = Conf.value c jsoo in
  let cairo2 = Conf.value c cairo2 in
  let vgr_pdf = uutf && otfm in
  doc_images () >>= fun doc_images ->
  Ok [
    Pkg.mllib "src/vg.mllib";
    Pkg.mllib "src/vgr_svg.mllib";
    Pkg.mllib ~cond:vgr_pdf "src/vgr_pdf.mllib";
    Pkg.mllib ~cond:jsoo "src/vgr_htmlc.mllib";
    Pkg.mllib ~cond:cairo2 "src/vgr_cairo.mllib";
    Pkg.bin ~cond:vgr_pdf "test/vecho";

    Pkg.doc "test/examples.ml";
    Pkg.doc "test/min_htmlc.html";
    Pkg.doc "test/min_htmlc.ml";
    Pkg.doc "test/min_pdf.ml";
    Pkg.doc "test/min_svg.ml";
    Pkg.doc "test/min_cairo_png.ml";
    Pkg.doc "test/min_cairo_mem.ml";
    Pkg.doc "test/fglyphs.ml";
    Pkg.doc "test/vecho.ml";
    doc_images;

    Pkg.test ~run:false "test/min_svg";
    Pkg.test ~run:false "test/min_pdf";
    Pkg.test ~run:false ~cond:cairo2 "test/min_cairo_png";
    Pkg.test ~run:false ~cond:cairo2 "test/min_cairo_mem";
    Pkg.test ~run:false "test/rsvg";
    Pkg.test ~run:false ~cond:vgr_pdf "test/rpdf";
    Pkg.test ~run:false ~cond:cairo2 "test/rcairo";
    Pkg.test ~run:false "test/examples";
    Pkg.test ~run:false ~cond:vgr_pdf "fglyphs";

    jsoo_test ~cond:jsoo "test/min_htmlc";
    jsoo_test ~cond:jsoo "test/sqc";
    jsoo_test ~cond:jsoo "test/rhtmlc";
]
