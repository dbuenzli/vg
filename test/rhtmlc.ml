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
let pp_dur ppf dur = pp ppf "%dms" (Float.int_of_round (dur *. 1000.))
let pp_renderer ppf = function 
| `CNV -> pp ppf "CNV" 
| `SVG -> pp ppf "SVG" 
| `PDF -> pp ppf "PDF" 
| `TXT -> pp ppf "TXT" 

let to_str_of_pp pp v =
  Format.fprintf Format.str_formatter "%a" pp v; 
  Format.flush_str_formatter ()

let src_link = (* TODO use %%VERSION%% *)
  format_of_string "https://github.com/dbuenzli/vg/blob/master/db/%s#L%d"

(* Resolution *)

let ppi_300 = 11811.
let res_ppcm = [ 2834.; 3779.; 5905.; 11811.; 23622. ]
let pp_res ppf d = pp ppf "%3d ppi" (Float.int_of_round ((d *. 2.54) /. 100.))

(* Persistent ui state. *)

module S = struct
  
  let store_version = "%%VERSION%%-003"
  let () = Store.force_version store_version 

  type t = 
    { id : string;                                     (* selected image id. *)
      tags : string list;                                  (* selected tags. *)
      renderer : [ `CNV | `SVG | `PDF | `TXT ];
      white_bg : bool;                                  (* white background. *)
      resolution : float; }                            (* render resolution. *)

  let state : t Store.key = Store.key () 
  let default = 
    { id = "arrowhead-5";
      tags = [];
      renderer = `CNV;
      white_bg = true; 
      resolution = ppi_300; }      

  let set s = Store.add state s; s
  let get () = match Store.find state with 
  | None -> set default
  | Some s -> 
      match Db.find s.id with (* check id still exists. *)
      | Some i -> s
      | None -> set { s with id = (List.hd (Db.all ())).Db.id }

  let to_hash s = s.id
  let set_hash s hash =
    if hash = "" then `Fail else 
    if Db.mem hash then `Ok (set { s with id = hash }) else 
    `Fail

  let ids s = 
    let imgs = match s.tags with [] -> Db.all () | tags -> Db.search ~tags () in
    List.map (fun i -> i.Db.id) imgs

  let image s = match Db.find s.id with
  | Some i -> i | None -> assert false 
end

(* Render *)

let renderers = [ `CNV; `SVG; `TXT ]

let render ?limit ?warn ?(meta = Vgm.empty) target dst i finish = 
  Log.msg "Render: %s" i.Db.id;
  let meta = Vgm.add_meta (Db.render_meta i) meta in
  let r = Vgr.create ?limit ?warn ~meta target dst in 
  let start = Time.now () in
  let rec loop steps v = match Vgr.render r v with 
  | `Ok ->
      let rec flush steps v = match Vgr.render r v with 
      | `Partial -> flush (steps + 1) v 
      | `Ok -> finish (Time.now () -. start) steps 
      in
      flush steps `End
  | `Partial -> Time.delay (fun () -> ignore (loop (steps + 1) `Await)) 0. 
  in
  loop 1 (`Image (Db.renderable i))

(* User interface *)

let ui () = 
  let s = S.get () in
  let db_ids, db_tags = Db.indexes () in
  let white, set_white = Ui.bool s.S.white_bg in
  let res, conf_res = Ui.menu ~id:"r-res" pp_res s.S.resolution res_ppcm in
  let ids, conf_ids = Ui.select 
      ~title:"Select an image to render" 
      pp_str (Some s.S.id) db_ids 
  in
  let id_count, set_id_count = Ui.text ~id:"id-count" "" in
  let tags, set_tags = Ui.mselect 
      ~title:"Filter images matching selected tags"
      pp_str s.S.tags db_tags in 
  let tag_count, set_tag_count = Ui.text ~id:"tag-count" "" in
  let title, title_conf = Ui.link ~id:"r-title"
      ~title:"See the image's source code" ~href:"#" ""
  in
  let author, author_conf = Ui.link ~id:"r-author" 
      ~title:"See the author's website" ~href:"#" "" 
  in
  let note, set_note = Ui.text ~id:"r-note" "" in
  let targets = Ui.group ~id:"r-targets" () in
  let t_cnv, canvas = Ui.canvas ~id:"r-canvas" () in
  let t_txt = Ui.group ~id:"r-txt" () in 
  let t_cnv_link, conf_t_cnv_link = Ui.link 
      ~id:"cnv-link" ~title:"Download PNG file"~href:"#" "" 
  in
  let t_svg_link, conf_t_svg_link = Ui.link 
      ~id:"svg-link" ~title:"Download SVG file"~href:"#" "" 
  in
  let rends, conf_rends = Ui.select 
      ~id:"r-rends" ~title:"Select the image renderer"
      pp_renderer (Some s.S.renderer) renderers 
  in 
  let t_uis = [`CNV, t_cnv_link; `SVG, t_svg_link; `TXT, t_txt ] in
  let time, set_time = Ui.text ~id:"r-time" "" in
  let log, conf_log = Ui.select Vgr.pp_warning None ~id:"r-log" [] in 
  let warn, clear_log = 
    let warns = ref [] in
    (fun w -> warns := w :: !warns; conf_log (`List !warns)), 
    (fun () -> warns := []; conf_log (`List []))
  in
  let set_white_bg s = Ui.classify targets "white" s.S.white_bg in
  let set_tags s = 
    let ids = S.ids s in
    let ts = s.S.tags in
    let sel = if List.mem s.S.id ids then Some s.S.id else None in
    let tag_count = if ts = [] then "" else str "(%d)" (List.length ts) in
    let id_count = str "(%d)" (List.length ids) in
    set_tag_count tag_count;
    set_id_count id_count;
    conf_ids (`List ids); conf_ids (`Select sel)
  in
  let set_author (a, url) = 
    author_conf (`Text a);
    author_conf (`Href url); 
  in
  let set_title i = 
    let url = match Db.find_loc i.Db.id Db_locs.values with
    | None -> "" | Some (fn, loc) -> str src_link fn loc
    in
    title_conf (`Text i.Db.title);
    title_conf (`Href url)
  in
  let set_image_meta s = 
    let i = S.image s in
    set_title i; 
    set_author i.Db.author;
    begin match i.Db.note with 
    | None ->  Ui.visible ~relayout:true note false
    | Some n -> set_note n; Ui.visible note true
    end;
  in
  let show_target t = 
    let set (t', ui) = Ui.visible ~relayout:true ui (t = t') in
    List.iter set t_uis
  in
  let render s =
    let i = S.image s in
    clear_log ();
    match s.S.renderer with 
    | `CNV -> 
        let set_stats dur steps =
          let steps = if steps = 1 then "." else str " and %d steps." steps in 
          let dur = Float.int_of_round (dur *. 1000.) in
          set_time (str "Rendered in %dms%s" dur steps); 
          let durl = Ui.canvas_data canvas in 
          conf_t_cnv_link (`Href durl);
          conf_t_cnv_link (`Download (str "%s.png" i.Db.id));
          show_target `CNV;
        in
        let res = s.S.resolution in 
        let meta = Vgm.add Vgm.empty Vgm.resolution (V2.v res res) in
        render ~warn ~meta (Vgr_htmlc.target canvas) `Other i set_stats
    | `SVG -> 
        let b = Buffer.create 2048 in
        let finish dur steps = 
          let svg = Buffer.contents b in
          let steps = if steps = 1 then "." else str " and %d steps." steps in 
          let dur = Float.int_of_round (dur *. 1000.) in
          set_time (str "Rendered in %dms%s" dur steps); 
          let u = "data:image/svg+xml," ^ 
                    (Ui.escape_binary (Buffer.contents b))          
          in
          conf_t_svg_link (`Href u);
          conf_t_svg_link (`Download (str "%s.svg" i.Db.id));
          Ui.set_svg_child t_svg_link svg;
          show_target `SVG;
        in
        let t = Vgr_svg.target ~xml_decl:true () in
        render ~limit:20 ~warn t (`Buffer b) i finish;
    | `TXT -> 
        let b = Buffer.create 2048 in 
        let ppf = Format.formatter_of_buffer b in
        let _, _, i = Db.renderable i in
        let start = Time.now () in
        pp ppf "%a" I.pp i;
        let dur = Time.now () -. start in 
        let dur = Float.int_of_round (dur *. 1000.) in
        set_time (str "Rendered in %dms" dur); 
        Ui.set_txt_child t_txt (Buffer.contents b);
        show_target `TXT;
          
    | `PDF -> assert false
  in
  let update ~force o n = 
    let f = force in
    let redraw = ref false in
    if f || o.S.id <> n.S.id then (set_image_meta n; redraw := true); 
    if f || o.S.tags <> n.S.tags then set_tags n;
    if f || o.S.renderer <> n.S.renderer then redraw := true;
    if f || o.S.white_bg <> n.S.white_bg then set_white_bg n;
    if f || o.S.resolution <> n.S.resolution then redraw := true; 
    if !redraw then render n;
    Ui.set_hash (S.to_hash n);
  in
  let on_change ui f = 
    let on_ev v = 
      let old_s = S.get () in 
      let new_s = S.set (f old_s v) in
      update ~force:false old_s new_s;
    in
    Ui.on_change ui on_ev 
  in
  let link () =
    on_change white (fun s b -> { s with S.white_bg = b });
    on_change res (fun s r -> { s with S.resolution = r });
    on_change ids begin fun s id -> match id with
    | Some id -> { s with S.id = id }
    | None -> s
    end;
    on_change tags (fun s ts -> { s with S.tags = ts });
    on_change rends begin fun s r -> match r with 
    | Some r -> { s with S.renderer = r}
    | None -> s
    end;
  in
  let init () =
    let hash_change ~force hash = 
      let old_s = S.get () in 
      let new_s = match S.set_hash old_s hash with 
      | `Ok new_s -> new_s 
      | `Fail -> Ui.set_hash (S.to_hash old_s); old_s 
      in
      update ~force old_s new_s
    in
    Ui.on_hash_change (hash_change ~force:false) ;
    hash_change ~force:true (Ui.hash ())
  in
  let layout () = 
    let header = 
      Ui.group () ~id:"r-header" *>
        Ui.label "Vg Image database" *>
        (fst (Ui.text ~id:"r-version" "v0.0.0")) (* TODO %%VERSION%% *)
    in
    let ui = 
      Ui.group ~id:"r-ui" () *> 
        (Ui.group ~id:"r-ids" () *>
           (Ui.group () *> Ui.label "Images" *> id_count) *> ids) *>
        (Ui.group ~id:"r-tags" () *> 
           (Ui.group () *> Ui.label "Tag filter" *> tag_count) *> tags) *>
        (Ui.group ~id:"r-rs" () *> 
           Ui.label "Renderer" *> rends) *>                      
        (Ui.group ~id:"r-set" () *>
           Ui.label "Settings" *>
           (Ui.label 
              ~title:"Render image against a white background" 
              ~ctrl:true "White background" *> white) *>
           (Ui.label 
              ~title:"Canvas resolution in pixel per inches"
              ~ctrl:true "Resolution" *> res))
    in
    let image = 
      Ui.group ~id:"r-image" () *>
        (targets *>
           (t_cnv_link *> t_cnv) *>
           (t_svg_link) *>
           (t_txt))
      *>
        (Ui.group ~id:"r-info" () *> 
           (Ui.group () *> title *> author) *> note) *>           
        time *> log
    in
    Ui.group ~id:"r-app" () *> header *> ui *> image 
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
