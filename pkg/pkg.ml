#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let brr = Conf.with_pkg "brr"
let cairo2 = Conf.with_pkg "cairo2"
let otfm = Conf.with_pkg "otfm"

let doc_images () =
  let is_dir p = OS.Dir.exists p |> Log.on_error_msg ~use:(fun _ -> false) in
  let skip p = not (Fpath.has_ext ".png" p) && not (is_dir p) in
  let mv acc p = (Pkg.doc ~built:false p ~dst:"odoc-assets/") :: acc in
  OS.File.fold ~skip (fun p acc -> p :: acc) [] ["doc"]
  >>= fun files -> Ok (Pkg.flatten (List.fold_left mv [] files))

let () =
  Pkg.describe "vg" @@ fun c ->
  let brr = Conf.value c brr in
  let cairo2 = Conf.value c cairo2 in
  let otfm = Conf.value c otfm in
  doc_images () >>= fun doc_images ->
  Ok [
    Pkg.mllib "src/vg.mllib";
    Pkg.mllib ~cond:otfm "src/pdf/vgr_pdf.mllib" ~dst_dir:"pdf";
    Pkg.mllib ~cond:brr "src/htmlc/vgr_htmlc.mllib" ~dst_dir:"htmlc";
    Pkg.mllib ~cond:cairo2 "src/cairo/vgr_cairo.mllib" ~dst_dir:"cairo";

    Pkg.bin ~cond:otfm "test/vecho";
    Pkg.doc "doc/index.mld" ~dst:"odoc-pages/index.mld";
    Pkg.doc "doc/tutorial.mld" ~dst:"odoc-pages/tutorial.mld";
    Pkg.doc "doc/semantics.mld" ~dst:"odoc-pages/semantics.mld";
    Pkg.doc "doc/image_howto.mld" ~dst:"odoc-pages/image_howto.mld";
    doc_images; ]
