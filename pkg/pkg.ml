#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let uutf = Conf.with_pkg "uutf"
let otfm = Conf.with_pkg "otfm"
let brr = Conf.with_pkg "brr"
let cairo2 = Conf.with_pkg "cairo2"

let doc_images () =
  let is_dir p = OS.Dir.exists p |> Log.on_error_msg ~use:(fun _ -> false) in
  let skip p = not (Fpath.has_ext ".png" p) && not (is_dir p) in
  let mv acc p = (Pkg.doc ~built:false p ~dst:"odoc-assets/") :: acc in
  OS.File.fold ~skip (fun p acc -> p :: acc) [] ["doc"]
  >>= fun files -> Ok (Pkg.flatten (List.fold_left mv [] files))

let () =
  Pkg.describe "vg" @@ fun c ->
  let uutf = Conf.value c uutf in
  let otfm = Conf.value c otfm in
  let brr = Conf.value c brr in
  let cairo2 = Conf.value c cairo2 in
  let vgr_pdf = uutf && otfm in
  doc_images () >>= fun doc_images ->
  Ok [
    Pkg.mllib "src/vg.mllib";
    Pkg.mllib ~cond:vgr_pdf "src/pdf/vgr_pdf.mllib" ~dst_dir:"pdf";
    Pkg.mllib ~cond:brr "src/htmlc/vgr_htmlc.mllib" ~dst_dir:"htmlc";
    Pkg.mllib ~cond:cairo2 "src/cairo/vgr_cairo.mllib" ~dst_dir:"cairo";

    Pkg.bin ~cond:vgr_pdf "test/vecho";
    Pkg.doc "doc/index.mld" ~dst:"odoc-pages/index.mld";
    Pkg.doc "doc/tutorial.mld" ~dst:"odoc-pages/tutorial.mld";
    Pkg.doc "doc/semantics.mld" ~dst:"odoc-pages/semantics.mld";
    Pkg.doc "doc/image_howto.mld" ~dst:"odoc-pages/image_howto.mld";
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
]
