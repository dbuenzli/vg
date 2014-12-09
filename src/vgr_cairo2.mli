(*---------------------------------------------------------------------------
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Vg Cairo2 renderer.

    {b References.}
    {ul {- {e {{:http://forge.ocamlcore.org/projects/cairo/}Cairo2 library
     for OCaml}}}}

    {e Release %%VERSION%% — %%MAINTAINER%% } *)

val target : Cairo.Surface.t -> [`Other] Vg.Vgr.target
