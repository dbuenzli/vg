open Gg2;;
open Backend;;

let e_cs_linear_rgb = "/CSlinearRGB"

type pid = int
type renderer = 
    { o : output;
      buf : Buffer.t;
      mutable bytes : int;
      mutable id : int;
      mutable index : (int * int) list; (* object id, bytes offset index. *)
      mutable page_objs : int list; (* Pages object ids *)
      mutable pid : pid;
      paths : (P.t, pid) Hashtbl.t;           (* Maps paths to pids. *)
      mutable area_rule : I.area_rule; (* Pdf does not store this in gstate. *)}

let buf_len = 1024
let bytes r = r.bytes + Buffer.length r.buf
let flush r = 
  r.bytes <- bytes r;
  r.o r.buf;
  Buffer.clear r.buf

let o_fmt r fmt v = 
  if Buffer.length r.buf > buf_len then flush r;
  Printf.bprintf r.buf fmt v

let o_str r s = 
  if Buffer.length r.buf > buf_len then flush r;
  Buffer.add_string r.buf s

let o_chr r c = Buffer.add_char r.buf c

let o_sp r = o_chr r ' ' (* TODO remove *)

let o_v2 r v = o_fmt r "%f %f" (V2.x v) (V2.y v)
let o_v3 r v = o_fmt r "%f %f %f" (V3.x v) (V3.y v) (V3.z v)

let new_path r p = try Hashtbl.find r.paths p with Not_found -> 
  r.pid <- r.pid + 1;
  Hashtbl.add r.paths p r.pid; 
  r.pid
  
let new_id r = 
  r.id <- r.id + 1;
  r.id

let o_obj_start r id = 
  r.index <- (id, bytes r) :: r.index;
  o_fmt r "%d 0 obj\n" id
  
let o_obj_end r = o_str r "endobj\n"


let o_gs_param r p = match p with
| `Antialias _ -> () (* TODO *)
| `Width w -> o_fmt r "%f w\n" w
| `Flatness f -> o_fmt r "%f i\n" f
| `Miter_limit limit -> o_fmt r "%f M\n" limit
| `Cap cap -> 
    let cap = match cap with `Butt -> 0 | `Round -> 1 | `Square -> 2 in
    o_fmt r "%i J\n" cap
| `Join join -> 
    let join = match join with `Miter -> 0 | `Round -> 1 | `Bevel -> 2 in 
    o_fmt r "%i j\n" join
| `Cloth i -> 
    begin match i with
    | I.Base I.Mono c ->
	o_v3 r (V4.to_v3 c); o_str r " SCN\n";
	o_v3 r (V4.to_v3 c); o_str r " scn\n"
	(* TODO, handle alpha *)
    | I.Base _ -> () (* TODO *)
    | I.State _ -> () (* TODO *)
    | I.Cut _ -> () (* mask *)
    | I.Blend _ -> () (* TODO *)
    end
| `Dashes (phase, dashes) ->
    o_chr r '[';
    List.iter (fun d -> o_fmt r "%f " d) dashes;
    o_fmt r "] %f d\n" phase
| `Area rule ->
    r.area_rule <- rule
| `Blender _ -> () (* TODO *)


let o_trs r tl = match tl with 
| [] -> ()
| tl -> 
    let mult acc tr = M3.mul acc (I.m3_of_tr tr) in
    let m = List.fold_left mult M3.id tl in
    o_fmt r "%f %f %f %f %f %f cm\n"
      (M3.el m 0 0) (M3.el m 0 1)
      (M3.el m 1 0) (M3.el m 1 1)
      (M3.el m 2 0) (M3.el m 2 1)


let one_div_3 = 1. /. 3. 
let two_div_3 = 2. /. 3. 

let o_path r p = 
  let o_cubic c c' pt r = 
    o_v2 r c; o_sp r; o_v2 r c'; o_sp r; o_v2 r pt; o_str r " c\n"; r
  in
  let rec seg r last = function
  | P.Start pt -> o_v2 r pt; o_str r " m\n"; pt 
  | P.Line pt -> o_v2 r pt; o_str r " l\n"; pt
  | P.Qcurve (c, pt) -> 
      (* Degree elevation *)
      let cc = V2.add (V2.smul one_div_3 last) (V2.smul two_div_3 c) in
      let cc' = V2.add (V2.smul two_div_3 c) (V2.smul one_div_3 pt) in
      seg r last (P.Ccurve (cc, cc', pt))
  | P.Ccurve (c, c', pt) -> ignore (o_cubic c c' pt r); pt
  | P.Earc(large, cw, radii, angle, pt) -> 
      (* TODO use tolerance here *)
      ignore (P.ecubic 1e-3 o_cubic r last large cw radii angle pt); pt
  | P.Close -> o_str r "h\n"; last
  in
  ignore (P.fold (seg r) Pt.o p)


let o_cut r part = match part with
| I.Outline -> o_str r "S\n"
| I.Area -> match r.area_rule with
  | `Non_zero -> o_str r "f\n"
  | `Even_odd -> o_str r "f*\n"

let rec o_image r i = match i with
| I.State ((pl, tl), i) -> 
    o_trs r tl;
    List.iter (fun p -> o_gs_param r p) (I.strip_params pl);
    o_image r i;
| I.Base b -> () (* TODO *)
| I.Cut (part, p) ->
    o_fmt r "/p%d Do\n" (new_path r p);
    o_cut r part
| I.Blend il -> (* TODO tailrec *)
    let save_gs r i = 
      o_str r "q\n";
      o_image r i;
      o_str r "Q\n"
    in
    List.iter (save_gs r) (List.rev il)

let o_stream r id i = (* TODO compression *)
  let len_obj_id = new_id r in
  o_obj_start r id;
  o_fmt r "<< /Length %d 0 R >>\nstream\n" len_obj_id;
  let stream_start = bytes r in 
  o_fmt r "%s CS\n" e_cs_linear_rgb; (* TODO review that *)
  o_fmt r "%s cs\n" e_cs_linear_rgb;
  o_image r i;
  let stream_size = bytes r - stream_start in
  o_str r "endstream\n";
  o_obj_end r;              
  (* Write size object. *)               
  o_obj_start r len_obj_id; 
  o_fmt r "%i\n" stream_size;
  o_obj_end r


let o_page r parent (size, view, i) = 
  let size' = V2.smul ((72. *. 100.) /. 2.54) size in (* meters -> 1/72 inch *)
  let o = Rect.o view in
  let scale = V2.div size' (Rect.size view)   in 
  let m = m3 
      (V2.x scale) 0.           (-. V2.x o *. V2.x scale) 
      0.           (V2.y scale) (-. V2.y o *. V2.y scale)
      0.           0.           1. 
  in
  let id = new_id r in
  let stream_id = new_id r in
  r.page_objs <- id :: r.page_objs;
  o_obj_start r id;
  o_fmt r "<< /Type /Page\n   \
              /Parent %d 0 R\n   \
              /MediaBox [0 0 %f %f]\n   \
              /Contents %d 0 R >>\n"
    parent (V2.x size') (V2.y size') stream_id;
  o_obj_end r;
  o_stream r stream_id (I.default & I.affine m & i)

let o_linear_rgb r id = 
  o_obj_start r id;
  o_fmt r "<< /CSlinearRGB [ /CalRGB << /WhitePoint [%f %f %f]\n             \
                        /Matrix [ %f %f %f %f %f %f %f %f %f ] >> ] >>\n"
  (* D65 *) 
  0.9505 1. 1.089 
  (* columns of linear RGB -> XYZ matrix *)
  0.4124 0.2126 0.0193
  0.3576 0.7152 0.1192
  0.1805 0.0722 0.9505;
  o_obj_end r

let o_form_xobj r id p =
  let b = P.bounds ~control:true p in
  let min = Rect.min b in 
  let max = Rect.max b in 
  let len_obj_id = new_id r in
  o_obj_start r id;
  o_fmt r "<< /Subtype /Form /BBox [ %f %f %f %f ] /Length %d 0 R >>\nstream\n" 
    (V2.x min) (V2.y min) (V2.x max) (V2.y max) len_obj_id;
  let stream_start = bytes r in 
  o_path r p;
  let stream_size = bytes r - stream_start in
  o_str r "endstream\n";
  o_obj_end r;              
  (* Write size object. *)               
  o_obj_start r len_obj_id; 
  o_fmt r "%i\n" stream_size;
  o_obj_end r
    

let o_resources r id =
  let path_alloc p pid acc = 
    let id = new_id r in
    o_fmt r "/p%d %d 0 R\n" pid id;
    (id, p) :: acc
  in
  let cs_id = new_id r in
  o_linear_rgb r cs_id;
  o_obj_start r id;
  o_fmt r "<< /ColorSpace %d 0 R\n   \ 
              /XObject <<\n" cs_id;
  let l = Hashtbl.fold path_alloc r.paths [] in
  o_str r ">>\n>>\n";
  o_obj_end r;
  List.iter (fun (id,p) -> o_form_xobj r id p) l
    
let o_page_tree r id resources_id =
  let pages = List.rev r.page_objs in 
  let count = List.length pages in
  o_obj_start r id;
  o_fmt r "<< /Type /Pages\n   \
              /Resources %d 0 R\n   \
              /Count %d\n   \
              /Kids [" 
         resources_id count;
  List.iter (fun id -> o_fmt r " %d 0 R" id) pages;
  o_str r "] >>\n";
  o_obj_end r

let o_doc_info r ?title ?author ?subject ?keywords ?creator ?date id = 
  let escape s = s in (* TODO *)
  let o_opt r k = function
  | None -> ()
  | Some v -> o_fmt r "%s (%s)\n" k (escape v)
  in
  o_obj_start r id;
  o_str r "<<";
  o_opt r "/Title" title;
  o_opt r "/Author" author;
  o_opt r "/Subject" subject;
  o_opt r "/Keywords" keywords;
  o_opt r "/Creator" creator;
  o_opt r "/CreationDate" date;
  o_str r ">>\n";
  o_obj_end r

let o_catalog r id page_tree_id =
  o_obj_start r id;
  o_fmt r "<< /Type /Catalog /Pages %d 0 R >>\n" page_tree_id;
  o_obj_end r

let o_end r root_id info_id = 
  let o_off (_, bytes) = o_fmt r "%010d 00000 n \n" bytes in
  let xref_offset = bytes r in 
  o_fmt r "xref\n0 %d\n0000000000 65535 f \n" (r.id + 1);
  
  List.iter o_off (List.sort compare r.index);
  o_fmt r "trailer\n<<\n /Size %d /Root %d 0 R /Info %d 0 R >>\n\
           startxref\n%d\n%%%%EOF"
    (r.id+1) root_id info_id xref_offset

let output ?compress ?title ?author ?subject ?keywords ?creator ?date 
    o pages =
  let r = { o = o; 
	    buf = Buffer.create buf_len;
	    bytes = 0;
	    id = 0;	    
	    index = [];
	    page_objs = [];
	    pid = 0;
	    paths = Hashtbl.create 255;
	    area_rule = `Non_zero; } 
  in  
  let resources_id = new_id r in
  let page_tree_id = new_id r in
  let info_id = new_id r in 
  let catalog_id = new_id r in
  o_str r "%PDF-1.4\n";
  List.iter (o_page r page_tree_id) pages;
  o_resources r resources_id; 
  o_page_tree r page_tree_id resources_id;
  o_doc_info r ?title ?author ?subject ?keywords ?creator ?date info_id;
  o_catalog r catalog_id page_tree_id;
  o_end r catalog_id info_id;
  flush r
