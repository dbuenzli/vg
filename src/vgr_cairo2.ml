(*---------------------------------------------------------------------------
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

open Gg
open Vg
open Vgr.Private.Data

type state = unit

let render s v k r = match v with
| `End -> k r
| `Image (size, view, i) ->
    failwith "todo"

let target surface =
  let target r _ =
    let ctx = Cairo.create surface in
    let state = () in
    true, render state
  in
  Vgr.Private.create_target target
