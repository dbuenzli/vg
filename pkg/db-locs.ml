#!/usr/bin/env ocaml
#directory "pkg"
#use "topkg-ext.ml"

let () = Cmd.exec "pkg/db-locs" >>& fun () -> ()
