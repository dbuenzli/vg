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
let pp = Format.fprintf
let pp_str = Format.pp_print_string 
let to_str_of_pp pp v =
  Format.fprintf Format.str_formatter "%a" pp v; 
  Format.flush_str_formatter ()

let src_link = 
  format_of_string "https://github.com/dbuenzli/vg/blob/master/db/%s#L%d"

(* Resolution *)

let ppi_300 = 11811.
let res_ppcm = [ 2834.; 3779.; 5905.; 11811.; 23622. ]
let pp_res ppf d = 
  pp ppf "%3d ppi" (Float.int_of_round ((d *. 2.54) /. 100.))

(* Persistent ui state. *)

module S = struct
  type t = 
    { id : string;                  (* selected image id *)
      white_bg : bool;              (* white background *)
      budget : bool;
      tags : string list;           (* selected tags *)
      resolution : float;
      prefix : string; }            (* filter prefix *)
  
  let state : t Store.key = Store.key () 
  
  let default = 
    { id = "arrowhead-5";
      white_bg = true; 
      budget = false;
      resolution = ppi_300;
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

let render_image warn c res i stats = 
  let meta = Db.render_meta i in 
  let meta = Vgm.add meta Vgm.resolution (V2.v res res) in
  let r = Vgr_htmlc.renderer ~warn ~meta c in 
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

let ui () = 
  let s = S.get () in
  let db_ids, db_tags = Db.indexes () in
  let budget, set_budget = Ui.bool s.S.budget in 
  let white, set_white = Ui.bool s.S.white_bg in
  let res, conf_res = Ui.menu ~id:"r-res" pp_res s.S.resolution res_ppcm in
  let ids, conf_ids = Ui.select pp_str (Some s.S.id) db_ids in
  let id_count, set_id_count = Ui.text ~id:"id-count" "" in
  let tags, set_tags = Ui.mselect pp_str s.S.tags db_tags in 
  let tag_count, set_tag_count = Ui.text ~id:"tag-count" "" in
  let title, set_title = Ui.text ~id:"i-title" "" in
  let author, set_author = Ui.text ~id:"i-author" "" in
  let note, set_note = Ui.text ~id:"i-note" "" in
  let image_frame = Ui.group ~id:"i-frame" () in
  let image, canvas = Ui.canvas ~id:"i-canvas" () in
  let src, conf_src = Ui.link ~id:"png-btn" ~href:"#" "SRC" in
  let png, conf_png = Ui.link ~id:"png-btn" ~href:"#" "PNG" in
  let rinfo, set_rinfo = Ui.text ~id:"i-rinfo" "" in
  let log, conf_log = Ui.select Vgr.pp_warning None ~id:"i-rlog" [] in 
  let warn, clear_log = 
    let warns = ref [] in
    (fun w _ -> warns := w :: !warns; conf_log (`List !warns)), 
    (fun () -> warns := []; conf_log (`List []))
  in
  let rec cmd = function
  | `Use_budget b -> 
      let _ = S.set { (S.get ()) with S.budget = b } in
      Log.msg "Use budget: %b" b;
  | `Use_white_bg b -> 
      let s = S.set { (S.get ()) with S.white_bg = b } in
      Ui.classify image_frame "white" s.S.white_bg
  | `Set_resolution r -> 
      let s = S.set { (S.get()) with S.resolution = r } in 
      Log.msg "Resolution change: %f" r;
      cmd (`Select_id s.S.id)
  | `Select_id id -> 
      let s = S.set { (S.get ()) with S.id = id } in
      let i = match Db.find ~ids:[s.S.id] () with [i] -> i | l ->assert false in
      let set_stats dur steps =
        let steps = if steps = 0 then "" else str "and %d steps" steps in 
        let dur = Float.int_of_round (dur *. 1000.) in
        set_rinfo (str "Rendered in %dms%s" dur steps)
      in
      clear_log ();
      set_title i.Db.title; 
      set_author i.Db.author;
      begin match Db.find_loc id Db_locs.values with
      | None -> Ui.visible ~relayout:true src false
      | Some (fn, loc) -> 
          conf_src (`Href (str src_link fn loc));
          Ui.visible src true
      end; 
      begin match i.Db.note with 
      | None ->  Ui.visible ~relayout:true note false
      | Some n -> set_note n; Ui.visible note true
      end;
      Ui.set_hash id;
      render_image warn canvas s.S.resolution i set_stats
  | `Use_tags ts ->
      let s = S.set { (S.get ()) with S.tags = ts } in
      let ids = List.map (fun i -> i.Db.id) (Db.find ~tags:ts ()) in
      let sel = if List.mem s.S.id ids then Some s.S.id else None in
      let tag_count = if ts = [] then "" else str "(%d)" (List.length ts) in
      let id_count = str "(%d)" (List.length ids) in
      set_tag_count tag_count;
      set_id_count id_count;
      conf_ids (`List ids); conf_ids (`Select sel)
  | `Make_png -> 
      let durl = Ui.canvas_data canvas in 
      conf_png (`Href durl)
  in
  let link () = (* Untying the recursive knot... *)
    Ui.on_change budget (fun b -> cmd (`Use_budget b)); 
    Ui.on_change white (fun b -> cmd (`Use_white_bg b));
    Ui.on_change res (fun r -> cmd (`Set_resolution r));
    Ui.on_change ids (fun id -> match id with 
    | Some i -> cmd (`Select_id i)
    | None -> ()); 
    Ui.on_change tags (fun ts -> cmd (`Use_tags ts)); 
    Ui.on_change png (fun () -> cmd (`Make_png))
  in
  let init () =
    let hash_change id = if id <> "" && Db.mem id then cmd (`Select_id id) in
    Ui.on_hash_change hash_change;
    let id = Ui.hash () in
    if id <> "" && Db.mem id then ignore (S.set { (S.get ()) with S.id = id });
    ignore (cmd (`Use_white_bg s.S.white_bg));
    ignore (cmd (`Use_tags s.S.tags));
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
            (Ui.group () *> Ui.label "Tag filter" *> tag_count) *> tags) *>
         (Ui.group ~id:"rsetts" () *> 
            Ui.label "Settings" *> 
            (Ui.label ~ctrl:true "White background" *> white) *>
            (Ui.label ~ctrl:true "Resolution" *> res) *>
            (Ui.label ~ctrl:true "Timeout test" *> budget))) *>
      (Ui.group ~id:"image" () *>
         (Ui.group ~id:"info" () *> 
            title *> author *> note *> (image_frame *> image) *> 
            (Ui.group ~id:"i-btns" () *> src *> png) *>
            rinfo *> log))
  in
  link (); init (); layout ()

let main () = Log.msg "rhtmlc loaded"; Ui.show (ui ())
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
