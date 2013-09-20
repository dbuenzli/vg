(*---------------------------------------------------------------------------
   Copyright 2013 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(* Renders the Vg image database to the HTML canvas element *)

open Gg
open Vg
open Mui

include Db_contents

let app_name = "rhtmlc" 

let str = Format.sprintf
let pp = Format.fprintf
let pp_str = Format.pp_print_string 
let pp_renderer ppf = function 
| `CNV -> pp ppf "CNV" 
| `SVG -> pp ppf "SVG" 
| `PDF -> pp ppf "PDF" 
| `TXT -> pp ppf "TXT" 

let to_str_of_pp pp v =
  Format.fprintf Format.str_formatter "%a" pp v; 
  Format.flush_str_formatter ()

let src_link =
  format_of_string "https://github.com/dbuenzli/vg/blob/master/db/%s#L%d"

let open_sans_xbold = match Vgr_pdf.otf_font Open_sans.extra_bold with 
| `Error e -> Log.msg "%a" Otfm.pp_error e; `Sans 
| `Otf _ as otf -> otf 

let font f = match f.Font.name, f.Font.weight with     (* PDF font resolver. *) 
| "Open Sans", `W800 -> open_sans_xbold
| _ -> Vgr_pdf.font f

(* Resolution *)

let ppi_300 = 11811.
let res_ppcm = [ 2834.; 3779.; 5905.; 11811.; 23622. ]
let pp_res ppf d = pp ppf "%3d ppi" (Float.int_of_round ((d *. 2.54) /. 100.))

(* Persistent ui state. *)

module S = struct
  
  let store_version = "%%VERSION%%-004"
  let () = Store.force_version store_version 

  type t = 
    { id : string;                                     (* selected image id. *)
      tags : string list;                                  (* selected tags. *)
      renderer : [ `CNV | `SVG | `PDF | `TXT ];        (* selected renderer. *)
      white_bg : bool;                                  (* white background. *)
      resolution : float; }                            (* render resolution. *)

  let state : t Store.key = Store.key () 
  let default = 
    { id = "rmark-ticks";
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

let renderers = [ `CNV; `SVG; `PDF; `TXT ]

let render ?limit ?warn target dst i finish = 
  Log.msg "Render: %s" i.Db.id;
  let r = Vgr.create ?limit ?warn target dst in 
  let warn w = match warn with None -> () | Some warn -> warn w in
  let start = Time.now () in
  let rec loop steps v = match Vgr.render r v with 
  | `Ok ->
      let rec flush steps v = match Vgr.render r v with 
      | `Partial -> flush (steps + 1) v 
      | `Ok -> finish ~exn:false (Time.now () -. start) steps 
      in
      flush steps `End
  | `Partial -> 
      Time.delay 0. begin fun () -> 
        try (loop (steps + 1) `Await) with 
        | e -> 
            warn (`Other "Sorry, a stack overflow occured."); 
            finish ~exn:true (Time.now () -. start) steps 
      end
  in
  try loop 1 (`Image (Db.renderable i)) with 
  | e -> 
      warn (`Other "Sorry, a stack overflow occured."); 
      finish ~exn:true (Time.now () -. start) 0

(* User interface *)

let pdf_pad = 2.

(* Does this have to be so ugly ? *)

let ui_activity () : 'a Ui.t * (bool -> unit) = 
  let active, set_active = Ui.text ~id:"r-rendering" "" in 
  let started = ref false in
  let state = ref false in
  let rec animate () = 
    let toggle () = Ui.classify active "r-activity" !state; in
    if not !started 
    then (state := false; toggle ())
    else (state := not !state; toggle (); Time.delay 0.4 animate)
  in
  let activate start = 
    if (!started == start) then () else 
    (started := start; if (!started) then animate ())
  in
  active, activate 
  
let ui_render_stats () : 'a Ui.t * (float -> int -> unit) = 
  let g = Ui.group ~id:"r-stats" () in 
  let time, set_time = Ui.text ~id:"r-time" "" in
  let steps, set_steps = Ui.text ~id:"r-steps" "" in
  let set_stats dur steps =
    let dur = str "%dms" (Float.int_of_round (dur *. 1000.)) in
    let steps = if steps = 1 then "" else str " and %d steps" steps in 
    set_time dur; set_steps steps
  in
  g *> (fst (Ui.text "Rendered in ")) *> time *> steps *> (fst (Ui.text ".")), 
  set_stats

let ui_image_info () : 'a Ui.t * (S.t -> unit) = 
  let g = Ui.group ~id:"r-image-info" () in
  let title, title_conf =
    Ui.link ~id:"r-title" ~title:"See the image's source code" ~href:"#" ""
  in
  let author, author_conf = 
    Ui.link ~id:"r-author" ~title:"See the author's website" ~href:"#" "" 
  in
  let note, set_note = Ui.text ~id:"r-note" "" in
  let set_image_info s =
    let i = S.image s in
    let src_url = match Db.find_loc i.Db.id Db_locs.values with
    | None -> "" | Some (fn, loc) -> str src_link fn loc
    in
    title_conf (`Text i.Db.title);
    title_conf (`Href src_url);
    author_conf (`Text (fst i.Db.author));
    author_conf (`Href (snd i.Db.author));
    begin match i.Db.note with
    | None ->  Ui.visible ~relayout:true note false
    | Some n -> set_note n; Ui.visible note true
    end;
  in
  g *> (Ui.group () *> title *> author) *> note, 
  set_image_info

let ui_log ppf : 'a Ui.t * ('b -> unit) * (unit -> unit) * (unit -> unit) =
  let log, conf_log = Ui.select ppf None ~id:"r-log" [] in 
  let warns = ref [] in
  let add_log w = warns := w :: !warns in
  let update_log () = conf_log (`List !warns) in
  let clear_log () = warns := []; conf_log (`List []) in
  log, add_log, clear_log, update_log

let ui_render_targets () = 
  let targets = Ui.group ~id:"r-targets" () in
  let activity, activate = ui_activity () in 
  let txt = Ui.group ~id:"r-txt" () in 
  let cnv, canvas = Ui.canvas ~id:"r-canvas" () in
  let cnv_link, conf_cnv_link = 
    Ui.link ~id:"cnv-link" ~title:"Download PNG file" ~href:"#" "" 
  in
  let svg_link, conf_svg_link = 
    Ui.link ~id:"svg-link" ~title:"Download SVG file" ~href:"#" "" 
  in
  let pdf, conf_pdf = Ui.object_ ~id:"r-pdf" () in
  let pdf_link, conf_pdf_link = 
    Ui.link ~id:"pdf-link" ~title:"Download PDF file" ~href:"#" 
      "No viewer: Download PDF"
  in
  let uis = [`CNV, cnv_link; `SVG, svg_link; `PDF, pdf; `TXT, txt ] in
  let show_target i t = 
    let set (t', ui) = Ui.visible ~relayout:true ui (t = t') in
    List.iter set uis;
    let height = match t with  (* adjust height to baseline *)
    | `CNV | `SVG | `PDF -> 
        let pad = if t = `PDF then pdf_pad else 0. in
        let baseline = 18 (* from the style sheet..., getting dynamically
                             problem at init time. If you delay, flickers. *) 
        in
        let height = (pad +. Size2.h i.Db.size) /. 0.2646 in 
        let adjust = Float.int_of_round (height /. (float baseline)) in
        str "%dpx" (adjust * baseline)
    | _ -> "auto" 
    in
    Ui.set_height targets height;        
  in
  let render s ~warn ~start ~finish =
    let valid s = 
      let current = S.get () in 
      S.image s == S.image current && s.S.renderer = current.S.renderer 
    in
    let i = S.image s in
    start ();
    activate true;
    match s.S.renderer with 
    | `CNV -> 
        let finish ~exn dur steps =
          if not (valid s) then () (* user moved on *) else
          if exn then (activate false; finish dur steps; show_target i `N) else
          let url = Ui.canvas_data canvas in 
          conf_cnv_link (`Href url);
          conf_cnv_link (`Download (str "%s.png" i.Db.id));
          activate false;
          finish dur steps; 
          show_target i `CNV;
        in
        let resolution = (V2.v s.S.resolution s.S.resolution) in
        render ~warn (Vgr_htmlc.target ~resolution canvas) `Other i finish
    | `SVG -> 
        let b = Buffer.create 2048 in
        let finish ~exn dur steps = 
          if not (valid s) then () (* user moved on *) else
          if exn then (activate false; finish dur steps; show_target i `N) else
          let svg = Buffer.contents b in
          let u = "data:image/svg+xml," ^ 
                    (Ui.escape_binary (Buffer.contents b))          
          in
          conf_svg_link (`Href u);
          conf_svg_link (`Download (str "%s.svg" i.Db.id));
          Ui.set_svg_child svg_link svg;
          activate false;
          finish dur steps; 
          show_target i `SVG
        in
        let create_date, creator_tool = Time.now (), app_name in
        let xmp = Db.xmp ~create_date ~creator_tool i in
        let t = Vgr_svg.target ~xml_decl:true ~xmp () in
        render ~limit:20 ~warn t (`Buffer b) i finish;
    | `TXT -> 
        let b = Buffer.create 2048 in
        let ppf = Format.formatter_of_buffer b in
        let _, _, img = Db.renderable i in
        let start = Time.now () in
        activate true;
        Time.delay 0. begin fun () -> 
          if not (valid s) then () else
          begin 
            pp ppf "%a@?" I.pp img;
            let dur = Time.now () -. start in 
            Ui.set_txt_child txt (Buffer.contents b);
            activate false;
            finish dur 0;
            show_target i `TXT
          end
        end
    | `PDF ->
        let b = Buffer.create 2048 in
        let finish ~exn dur steps = 
          if not (valid s) then () (* user moved on *) else
          if exn then (activate false; finish dur steps; show_target i `N) else
          let u = "data:application/pdf," ^ 
                    (Ui.escape_binary (Buffer.contents b))          
          in
          let size = V2.(i.Db.size + (v pdf_pad pdf_pad)) in
          let file = str "%s.pdf" i.Db.id in
          conf_pdf_link (`Href u);
          conf_pdf_link (`Download file);
          conf_pdf (`Name file);
          conf_pdf (`Data u);
          conf_pdf (`Size (V2.to_tuple size));
          activate false;
          finish dur steps; 
          show_target i `PDF
        in
        let create_date, creator_tool = Time.now (), app_name in
        let xmp = Db.xmp ~create_date ~creator_tool i in
        let t = Vgr_pdf.target ~font ~xmp () in
        render ~limit:20 ~warn t (`Buffer b) i finish;
  in
  List.iter (fun (_, ui) -> Ui.visible ~relayout:true ui false) uis;
  (targets *> (cnv_link *> cnv) *> (pdf *> pdf_link) *> svg_link *> txt), 
  render, activity

let ui_ids s = 
  let db_ids, db_tags = Db.indexes () in
  let ids, conf_ids = 
    Ui.select ~title:"Select an image to render" pp_str (Some s.S.id) db_ids 
  in
  let tags, set_tags = 
    Ui.mselect ~title:"Filter images matching selected tags"
      pp_str s.S.tags db_tags 
  in
  let id_count, set_id_count = Ui.text ~id:"id-count" "" in 
  let tag_count, set_tag_count = Ui.text ~id:"tag-count" "" in
  let set_tags s = 
    let ids = S.ids s in
    let ts = s.S.tags in
    let sel = if List.mem s.S.id ids then Some s.S.id else None in
    let tag_count = (* if ts = [] then "" else *) str "(%d)" (List.length ts) in
    let id_count = str "(%d)" (List.length ids) in
    set_tag_count tag_count;
    set_id_count id_count;
    conf_ids (`List ids); conf_ids (`Select sel)
  in
  let ids_group = 
    Ui.group ~id:"r-ids" () *>
      (Ui.group () *> Ui.label "Images" *> id_count) *> ids
  in
  let tags_group = 
    Ui.group ~id:"r-tags" () *> 
      (Ui.group () *> Ui.label "Tag filter" *> tag_count) *> tags
  in
  ids, ids_group, tags, tags_group, set_tags

let ui () = 
  let s = S.get () in
  let ids, ids_group, tags, tags_group, set_tags = ui_ids s in
  let rends, _ = 
    Ui.select ~id:"r-rends" ~title:"Select the image renderer"
      pp_renderer (Some s.S.renderer) renderers 
  in 
  let white, _ = Ui.bool s.S.white_bg in
  let res, _ = Ui.menu ~id:"r-res" pp_res s.S.resolution res_ppcm in
  let targets, render, activity = ui_render_targets () in
  let image_info, set_image_info = ui_image_info () in
  let stats, set_stats = ui_render_stats () in
  let log, add_log, clear_log, update_log = ui_log Vgr.pp_warning in 
  let set_white_bg s = Ui.classify targets "white" s.S.white_bg in
  let update ~force o n = 
    let f = force in
    let redraw = ref false in
    let finish dur steps = set_stats dur steps; update_log () in
    if f || o.S.id <> n.S.id then (set_image_info n; redraw := true); 
    if f || o.S.tags <> n.S.tags then set_tags n;
    if f || o.S.renderer <> n.S.renderer then redraw := true;
    if f || o.S.white_bg <> n.S.white_bg then set_white_bg n;
    if f || o.S.resolution <> n.S.resolution then redraw := true; 
    if !redraw then render ~warn:add_log ~start:clear_log ~finish n;
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
        (fst (Ui.text ~id:"r-version" "%%VERSION%%"))
    in
    let ui = 
      Ui.group ~id:"r-ui" () *> 
        ids_group *> 
        tags_group *>
        (Ui.group ~id:"r-rs" () *> 
           (Ui.group () *> Ui.label "Renderer" *> activity) *> rends) *>
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
      Ui.group ~id:"r-image" () *> targets *>
        (Ui.group ~id:"r-info" () *> image_info *> stats *> log)
    in
    Ui.group ~id:"r-app" () *> header *> ui *> image 
  in
  link (); init (); layout ()

let main () = Log.msg "%s loaded" app_name; Ui.show (ui ())
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
