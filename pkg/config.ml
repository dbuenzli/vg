#!/usr/bin/env ocaml
#directory "pkg"
#use "topkg-ext.ml"

module Config = struct
  include Config_default

  let distrib_hook = Some "pkg/db-locs"
  
  let vars =
    [ "NAME", "";
      "VERSION", Git.describe ~chop_v:true "master";
      "MAINTAINER", "Daniel Bünzli <daniel.buenzl i\\@erratique.ch>" ]

  let www_demos = ["rhtmlc"; "sqc" ]
end
