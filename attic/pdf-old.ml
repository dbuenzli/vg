open Gg2;;
open Backend;;

let str = Printf.sprintf
let unsupported s = unsupported (str "pdf: %s" s)
let e_type = "/Type"
let e_title = "/Title"
let e_author = "/Author"
let e_subject = "/Subject"
let e_keywords = "/Keywords"
let e_creator = "/Creator"
let e_creation_date = "/CreationDate"
let e_producer = "/Producer"
let e_size = "/Size"
let e_root = "/Root"
let e_info = "/Info"
let e_pages = "/Pages"
let e_kids = "/Kids"
let e_count = "/Count"
let e_mediabox = "/MediaBox"
let e_cropbox = "/CropBox"
let e_resources = "/Resources"
let e_contents = "/Contents"
let e_parent = "/Parent"
let e_length = "/Length"
let e_filter = "/Filter"
let e_extgstate = "/ExtGState"
let e_colorspace = "/ColorSpace"
let e_cs_linear_rgb = "/CSlinearRGB"
let e_cal_rgb = "/CalRGB"
let e_white_point = "/WhitePoint"
let e_matrix = "/Matrix"
let ev_catalog = "/Catalog"
let ev_pages = "/Pages"
let ev_page = "/Page"
let ev_flate_decode = "/FlateDecode"

type renderer = 
    { o : output; 
      compress : (string -> string) option;
      stream : Buffer.t;
      mutable area_rule : I.area_rule; (* Pdf does not store this in gstate. *)
      mutable gs_id : int;            (* Identifier for graphic state dicts. *)
      mutable gs : (I.param list, int) Hashtbl.t;
      mutable obj_num : int;
      mutable off_page_tree : int;                       (* Offset of obj 1. *)
      mutable off_resource : int;                        (* Offset of obj 2. *)
      mutable off_objs : int list;               (* Offset of other objects. *)
      mutable page_count : int; }

let gs_id r pl = try Hashtbl.find r.gs pl with Not_found ->
  let id = r.gs_id <- r.gs_id + 1; r.gs_id in
  Hashtbl.add r.gs pl id;
  id

(* Output is continuation based. *)

let ( >> ) f g x = f (g x) 
let nop k r = k r
let stop r = ()
let o_eof = stop
let o_str s k r = outs r.o s; k r
let o_chr c k r = outc r.o c; k r
let o_sp = o_chr ' '
let o_nl = o_chr '\n'
let o_float f = if V1.is_zero f then o_chr '0' else o_str (str "%f" f)
let o_v2 v = o_str (str "%f %f" (V2.x v) (V2.y v))
let o_v3 v = o_str (str "%f %f %f" (V3.x v) (V3.y v) (V3.z v))
let o_int i = o_str (str "%d" i)
let o_list out l k r = List.iter (fun e -> out e stop r) l; k r 
let o_ref i = o_str (str "%d 0 R" i)
let o_entry n entry = o_str n >> o_sp >> entry >> o_nl
let o_dict entries = o_str "<<\n" >> entries >> o_str ">>\n"
let o_pdf_str s = o_chr '(' >> o_str s >> o_chr ')'
let o_array contents = o_str "[" >> contents >> o_chr ']'

let obj_num_init = 2
let obj_page_tree = 1
let obj_resource = 2
let o_obj_id id obj = o_str (str "%d 0 obj\n" id) >> obj >> o_str "endobj\n"

let o_obj_page_tree obj k r = 
  r.off_page_tree <- bytes r.o;
  o_obj_id obj_page_tree obj k r

let o_obj_resources obj k r = 
  r.off_resource <- bytes r.o;
  o_obj_id obj_resource obj k r

let o_obj obj k r = 
  r.obj_num <- r.obj_num + 1;
  r.off_objs <- (bytes r.o) :: r.off_objs;
  o_obj_id r.obj_num obj k r 

let o_stream k r = 
  let stream len filter s = 
    o_obj & o_dict ((o_entry e_length (o_int len)) >> filter) >>
    o_str (str "stream\n") >> o_str s >> o_str "endstream\n" 
  in
  let len = Buffer.length r.stream in 
  let s = Buffer.contents r.stream in 
  Buffer.clear r.stream;
  match r.compress with 
  | None -> stream len nop s k r
  | Some compress -> 
      let s = compress (Buffer.contents r.stream) in 
      let len = String.length s in
      stream len (o_entry e_filter (o_str ev_flate_decode)) s k r
      
let so_str s k r = Buffer.add_string r.stream s; k r
let so_v2 v = so_str (str "%f %f" (V2.x v) (V2.y v))
let so_v3 v = so_str (str "%f %f %f" (V3.x v) (V3.y v) (V3.z v))
let so_float f = so_str (str "%f" f) 
let so_sp = so_str (str " ")
let so_gs pl k r = so_str (str "/GS%d gs\n" (gs_id r pl)) k r

let so_gs_param = function (* As much as possible here, rest in via o_gs *)
| `Antialias _ -> nop (* TODO *)
| `Width w -> so_str (str "%f w\n" w)
| `Flatness f -> so_str (str "%f i\n" f) 
| `Miter_limit limit -> so_str (str "%f M\n" limit)
| `Cap cap -> 
    let cap = match cap with `Butt -> 0 | `Round -> 1 | `Square -> 2 in
    so_str (str "%i J\n" cap)
| `Join join -> 
    let join = match join with `Miter -> 0 | `Round -> 1 | `Bevel -> 2 in 
    so_str (str "%i j\n" join)
| `Cloth i -> 
    begin match i with
    | I.Base I.Mono c -> 
	so_v3 (V4.to_v3 c) >> so_sp >> so_str "SCN\n" >>
	so_v3 (V4.to_v3 c) >> so_sp >> so_str "scn\n"
	(* TODO, handle alpha *)
    | I.Base _ -> nop (* TODO *)
    | I.State _ -> nop (* TODO *)
    | I.Cut _ -> nop (* mask *)
    | I.Blend _ -> nop (* TODO *)
    end
| `Dashes (phase, dashes) ->
    so_str "[" >> o_list (fun d -> so_str (str "%f " d)) dashes >>
    so_str (str "] %f d\n" phase)
| `Area rule ->                                          (* Maintained in r. *)
    let set_area_rule rule k r = r.area_rule <- rule; k r in
    set_area_rule rule 
| `Blender _ -> nop (* TODO *)

(*
let so_params = function
| [] -> nop
| pl -> so_gs (I.strip_params pl)
*)

let so_trs = function
| [] -> nop
| tl -> 
    let mult acc tr = M3.mul acc (I.m3_of_tr tr) in
    let m = List.fold_left mult M3.id tl in
    so_str (str "%f %f %f %f %f %f cm\n" 
	      (M3.el m 0 0) (M3.el m 0 1) 
	      (M3.el m 1 0) (M3.el m 1 1)
	      (M3.el m 2 0) (M3.el m 2 1))


let one_div_3 = 1. /. 3. 
let two_div_3 = 2. /. 3. 

let so_path p = 
  let out r o = o stop r in
  let rec seg r last = function
  | P.Start pt -> out r & so_v2 pt >> so_str " m\n"; pt 
  | P.Line pt -> out r & so_v2 pt >> so_str " l\n"; pt
  | P.Qcurve (c, pt) -> 
      let cc = V2.add (V2.smul one_div_3 last) (V2.smul two_div_3 c) in
      let cc' = V2.add (V2.smul two_div_3 c) (V2.smul one_div_3 pt) in
      seg r last (P.Ccurve (cc, cc', pt))
  | P.Ccurve (c, c', pt) -> 
      out r & 
      so_v2 c >> so_sp >> so_v2 c' >> so_sp >> so_v2 pt >> so_str " c\n";
      pt
  | P.Earc(large, cw, radii, angle, pt) -> 
(*
      let out_linear pt r = out r & so_v2 pt >> so_str " l\n"; r in
      ignore (P.elinear 1e-3 out_linear r last large cw radii angle pt); pt
*)

      let out_cubic c c' pt r = 
	out r & 
	so_v2 c >> so_sp >> so_v2 c' >> so_sp >> so_v2 pt >> so_str " c\n";
	r
      in
      (* TODO use tolerance here *)
      ignore (P.ecubic 1e-3 out_cubic r last large cw radii angle pt); pt
  | P.Close -> out r & so_str "h\n"; last
  in
  let sub segs k r = 
    ignore (List.fold_left (seg r) (V2.o) (List.rev segs));
    k r
  in
  o_list sub (List.rev p)

let so_cut part k r = match part with
| I.Outline -> so_str "S\n" k r
| I.Area -> match r.area_rule with
  | `Non_zero -> so_str "f\n" k r
  | `Even_odd -> so_str "f*\n" k r

let rec so_page = function 
| I.State ((pl, tl), i) -> 
    so_trs tl >> o_list so_gs_param (I.strip_params pl) >> so_page i
| I.Base b -> nop
| I.Cut (part, p) -> so_path p >> so_cut part
| I.Blend il -> (* TODO tailrec *)
    let save_gs i = so_str "q\n" >> so_page i >> so_str "Q\n" in
    o_list save_gs (List.rev il)

let o_page (size, view, i) = 
  let size' = V2.smul ((72. *. 100.) /. 2.54) size in (* meters -> 1/72 inch *)
  let o = Rect.o view in
  let scale = V2.div size' (Rect.size view)   in 
  let m = m3 
      (V2.x scale) 0.           (-. V2.x o *. V2.x scale) 
      0.           (V2.y scale) (-. V2.y o *. V2.y scale)
      0.           0.           1. 
  in
  let so_cs = so_str (str "%s CS\n%s cs\n" e_cs_linear_rgb e_cs_linear_rgb) in
  let o_ref_next k r = o_ref (r.obj_num + 1) k r in 
  let page_obj = o_obj & o_dict begin 
    o_entry e_type (o_str ev_page) >>
    o_entry e_parent (o_ref obj_page_tree) >>
    o_entry e_mediabox (o_array (o_str "0 0 " >> o_v2 size'))  >>
    o_entry e_contents o_ref_next
  end
  in
  page_obj >> so_cs >> so_page (I.default & I.affine m & i) >> o_stream



let o_gs k r =  k r
(*                              
  let o_param = function
  | `Antialias _ -> nop (* TODO *)
  | `Flatness f -> o_entry e_fl (o_float f)
  | `Width w -> o_entry e_lw (o_float w)
  | `Cap cap -> 
      let cap = match cap with `Butt -> 0 | `Round -> 1 | `Square -> 2 in
      o_entry e_lc (o_int cap)
  | `Cloth _ -> nop (* TODO *)
  | `Dashes _ -> nop 

  | `Join join -> 
      let join = match join with `Miter -> 0 | `Round -> 1 | `Bevel -> 2 in 
      o_entry e_lj (o_int join)
  | `Miter_limit limit -> o_entry e_ml (o_float limit)
  | `Area _ -> nop                      (* this is handled with r.area_rule *)
  | `Blender _ -> nop (* TODO *)
  in 
  let o_gs_dict pl id = 
    o_entry (str "/GS%d" id) & o_dict (o_list o_param pl)
  in
  Hashtbl.iter (fun pl id -> o_gs_dict pl id stop r) r.gs; k r
*)  

let o_linear_rgb = 
  let white = o_v3 (v3 0.9505 1. 1.089) in (* D65 *) 
  let c1 = v3 0.4124 0.2126 0.0193 in (* columns of linear RGB -> XYZ matrix *)
  let c2 = v3 0.3576 0.7152 0.1192 in
  let c3 = v3 0.1805 0.0722 0.9505 in
  let matrix = o_v3 c1 >> o_sp >> o_v3 c2 >> o_sp >> o_v3 c3 in
  o_array begin 
    o_str e_cal_rgb >> o_sp >> 
    (o_dict & begin 
      o_entry e_white_point (o_array white) >>
      o_entry e_matrix (o_array matrix)
    end)
  end

let o_resources =  o_obj_resources & o_dict begin   
  o_entry e_colorspace (o_dict & o_entry e_cs_linear_rgb o_linear_rgb) >>
  o_entry e_extgstate (o_dict & o_gs)
end

let o_page_tree =
  let o_kids k r = 
    for i = 1 to r.page_count do o_ref (obj_num_init + i) stop r done; k r in 
  let o_page_count k r = o_int r.page_count k r in
  o_obj_page_tree & o_dict begin
    o_entry e_type (o_str ev_pages) >>
    o_entry e_count o_page_count >>
    o_entry e_kids (o_array o_kids) >>
    o_entry e_resources (o_ref obj_resource) 
  end

let o_doc_info ?title ?author ?subject ?keywords ?creator ?date = 
  let o_opt e out v = match v with 
  | None -> nop 
  | Some v -> o_entry e (out v)           (* TODO escape strings \\ ( ) *)
  in
  o_obj & o_dict begin
    o_opt e_title o_pdf_str title >>
    o_opt e_author o_pdf_str author >>
    o_opt e_subject o_pdf_str subject >>
    o_opt e_keywords o_pdf_str keywords >>
    o_opt e_creator o_pdf_str creator >>
    o_opt e_creation_date o_pdf_str date (* >>
    o_entry e_producer (o_pdf_str "OCaml Vg library TODO URL") *)
  end

let o_catalog =
  o_obj & o_dict begin
    o_entry e_type (o_str ev_catalog) >>
    o_entry e_pages (o_ref obj_page_tree)
  end
  
let o_xref =
  let o_xref_size k r = o_str (str "%d\n" (r.obj_num + 1)) k r in
  let o_off off = o_str (str "%010d 00000 n \n" off) in
  let o_offs k r = 
    let offs = r.off_page_tree :: r.off_resource :: (List.rev r.off_objs) in
    (o_list o_off offs) k r
  in
  o_str "xref\n" >>                                     
  o_str "0 " >> o_xref_size >>
  o_str "0000000000 65535 f \n" >>
  o_offs

let o_trailer off_xref = 
  let o_size k r = o_int (r.obj_num + 1) k r in
  let o_ref_last_rel rel k r = o_ref (r.obj_num + rel) k r in  
  o_str "trailer\n" >> o_dict begin 
    o_entry e_size o_size >>
    o_entry e_root (o_ref_last_rel 0) >>
    o_entry e_info (o_ref_last_rel ~-1) 
  end >>
  o_str (str "startxref\n%d\n%%%%EOF" off_xref)

let o_end k r = 
  let off_xref = bytes r.o in                           (* Cross ref offset. *)
  (o_xref >> o_trailer off_xref) k r
  
let output ?compress ?title ?author ?subject ?keywords ?creator ?date 
    o pages =
  let r = { o = o; 
	    compress = compress;
	    stream = Buffer.create 2048;
	    gs_id = 0;
	    gs = Hashtbl.create 512;
	    area_rule = `Non_zero;
	    obj_num = obj_num_init; 
	    off_page_tree = 0;
	    off_resource = 0;
	    off_objs = []; 
	    page_count = 1; } 
  in
  begin 
    o_str "%PDF-1.4\n" >>
    o_list o_page pages >> 
    o_resources  >>
    o_page_tree >>
    o_doc_info ?title ?author ?subject ?keywords ?creator ?date >> 
    o_catalog >>
    o_end
  end o_eof r
  
