(*---------------------------------------------------------------------------
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

open Gg
open Vg
open Vgr.Private.Data

type cmd = Draw of Vgr.Private.Data.image
type state =
  { r : Vgr.Private.renderer;                  (* corresponding renderer. *)
    surface : Cairo.Surface.t;                    (* surface rendered to. *)
    ctx : Cairo.context;                         (* context of [surface]. *)
    mutable cost : int;                        (* cost counter for limit. *)
    mutable todo : cmd list;                      (* commands to perform. *)
  }

let partial = Vgr.Private.partial
let limit s = Vgr.Private.limit s.r
let warn s w = Vgr.Private.warn s.r w
let image i = Vgr.Private.I.of_data i

let rec r_image s k r =
  if s.cost > limit s then (s.cost <- 0; partial (r_image s k) r) else
  match s.todo with
  | [] -> k r
  | Draw i :: todo ->
      s.cost <- s.cost + 1;
      match i with
      | _ ->
          s.todo <- todo;
          r_image s k r

let render s v k r = match v with
| `End -> k r
| `Image (size, view, i) ->
    s.cost <- 0;
    s.todo <- [ Draw i ];
    r_image s k r

let target surface =
  let target r _ =
    let ctx = Cairo.create surface in
    let state =
      { r; surface; ctx;
        cost = 0;
        todo = [];
      } in
    true, render state
  in
  Vgr.Private.create_target target
