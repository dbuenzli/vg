(*---------------------------------------------------------------------------
   Copyright 2013 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(* Renders the Vg image database to the HTML canvas element *)

open Gg
open Vg
open Mui

include Db_htmlc

let str = Format.sprintf
let pp_str = Format.pp_print_string 

(* Persistent ui state. *)

module S = struct
  type t = 
    { id : string;                  (* selected image id *)
      white_bg : bool;              (* white background *)
      budget : bool;
      tags : string list;           (* selected tags *)
      prefix : string; }            (* filter prefix *)
  
  let state : t Store.key = Store.key () 
  
  let default = 
    { id = "arrowhead-8"; (* TODO change that *)
      white_bg = true; 
      budget = false;
      tags = [];
      prefix = "default" }      

  let set s = Store.add state s; s
  let get () = match Store.find state with 
  | None -> set default
  | Some s -> s

  let ids st = 
    let id i = i.Db.id in
    List.map id (Db.find ~prefixes:[st.prefix] ~tags:st.tags ())
end

(* Render *)

let render_image c i stats = 
  let r = Vgr_htmlc.renderer ~meta:(Db.render_meta i) c in 
  let start = Time.now () in
  let rec finish steps v = match Vgr.render r v with 
  | `Partial -> finish steps v (* should not happen *)
  | `Ok -> stats (Time.now () -. start) steps 
  in
  let rec loop steps v = match Vgr.render r v with 
  | `Ok -> finish steps `End 
  | `Partial -> Time.delay (fun () -> ignore (loop (steps + 1) `Await)) 0.
  in
  loop 0 (`Image (Db.renderable i))

(* User interface *)

(* to do canvas.toDataURL() 
         window*)

let ui () = 
  let s = S.get () in
  let db_ids, db_tags = Db.indexes () in
  let budget, set_budget = Ui.bool s.S.budget in 
  let white, set_white = Ui.bool s.S.white_bg in
  let ids, conf_ids = Ui.select pp_str (Some s.S.id) db_ids in
  let id_count, set_id_count = Ui.text ~id:"id-count" "" in
  let tags, set_tags = Ui.mselect pp_str s.S.tags db_tags in 
  let tag_count, set_tag_count = Ui.text ~id:"tag-count" "" in
  let title, set_title = Ui.text ~id:"i-title" "" in
  let author, set_author = Ui.text ~id:"i-author" "" in
  let note, set_note = Ui.text ~id:"i-note" "" in
  let image, canvas = Ui.canvas ~id:"i-canvas" () in
  let rinfo, set_rinfo = Ui.text ~id:"i-rinfo" "bla" in
  let cmd = function
  | `Use_budget b -> 
      let _ = S.set { (S.get ()) with S.budget = b } in
      Log.msg "Use budget: %b" b;
  | `Use_white_bg b -> 
      let s = S.set { (S.get ()) with S.white_bg = b } in
      Ui.classify image "white" s.S.white_bg
  | `Select_id id -> 
      let s = S.set { (S.get ()) with S.id = id } in
      let i = match Db.find ~ids:[s.S.id] () with [i] -> i | l ->assert false in
      let set_stats dur steps =
        let steps = if steps = 0 then "" else str "and %d steps" steps in 
        set_rinfo (str "Rendered in %.4fs%s" dur steps) 
      in
      set_title i.Db.title; 
      set_author i.Db.author;
      begin match i.Db.note with 
      | None ->  Ui.visible ~relayout:true note false
      | Some n -> set_note n; Ui.visible note true
      end;
      render_image canvas i set_stats 
  | `Use_tags ts ->
      let s = S.set { (S.get ()) with S.tags = ts } in
      let ids = List.map (fun i -> i.Db.id) (Db.find ~tags:ts ()) in
      let sel = if List.mem s.S.id ids then Some s.S.id else None in
      let tag_count = if ts = [] then "" else str "(%d)" (List.length ts) in
      let id_count = str "(%d)" (List.length ids) in
      set_tag_count tag_count;
      set_id_count id_count;
      conf_ids (`List ids); conf_ids (`Select sel)
  in
  let link () = (* Untying the recursive knot... *)
    Ui.on_change budget (fun b -> cmd (`Use_budget b)); 
    Ui.on_change white (fun b -> cmd (`Use_white_bg b));
    Ui.on_change ids (fun id -> match id with 
    | Some i -> cmd (`Select_id i)
    | None -> ()); 
    Ui.on_change tags (fun ts -> cmd (`Use_tags ts)); 
  in
  let init () = 
    ignore (cmd (`Select_id s.S.id));
    ignore (cmd (`Use_tags s.S.tags));
    ignore (cmd (`Use_white_bg s.S.white_bg));
  in
  let layout () = 
    Ui.group () *> 
      (Ui.group () ~id:"header" *>
         Ui.label "Vg Image database" *>
         (fst (Ui.text ~id:"vg-version" "v0.0.0"))) *> (* TODO %%VERSION%% *)
      (Ui.group ~id:"ui" () *> 
         (Ui.group ~id:"ids" () *> 
            (Ui.group () *> Ui.label "Images" *> id_count) *> ids) *>
         (Ui.group ~id:"tags" () *> 
            (Ui.group () *> Ui.label "Tags" *> tag_count) *> tags) *>
         (Ui.group ~id:"rsetts" () *> 
            Ui.label "Render settings" *> 
            (Ui.label ~ctrl:true "White background" *> white) *>
            (Ui.label ~ctrl:true "Budget" *> budget))) *>
      (Ui.group ~id:"image" () *>
         (Ui.group ~id:"info" () *> title *> author *> note *> image *> rinfo))
  in
  link (); init (); layout ()

let main () = Ui.show (ui ())
let () = Ui.main main

(*---------------------------------------------------------------------------
   Copyright 2013 Daniel C. Bünzli.
   All rights reserved.

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions
   are met:
     
   1. Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

   2. Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

   3. Neither the name of Daniel C. Bünzli nor the names of
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
  ---------------------------------------------------------------------------*)
