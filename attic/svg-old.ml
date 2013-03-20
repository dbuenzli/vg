(*----------------------------------------------------------------------------
   Copyright (c) %%COPYRIGHTYEAR%%, Daniel C. Bünzli. All rights reserved.     
   Distributed under a BSD license, see ../LICENSE.                            
  ----------------------------------------------------------------------------*)
open Gg2;;
open Backend;;

let str = Printf.sprintf
let unsupported s = unsupported (str "svg: %s" s)
let rad_to_deg a = (a *. 180.) /. pi

let t_svg = "svg"
let t_title = "title"
let t_g = "g"
let t_use = "use"
let t_path = "path"
let t_stop = "stop"
let t_linear_gradient = "linearGradient"
let t_radial_gradient = "radialGradient"
let t_pattern = "pattern"
let t_rect = "rect"
let a_version = "version"
let a_view_box = "viewBox"
let a_color_interpol = "color-interpolation"
let a_color_interpol_filters = "color-interpolation-filter"
let a_preserve_aspect_ratio = "preserveAspectRatio"
let a_width = "width"
let a_height = "height"
let a_xlink_href = "xlink:href" 
let a_id = "id"
let a_d = "d"
let a_offset = "offset"
let a_stop_color = "stop-color" 
let a_stop_opacity = "stop-opacity" 
let a_x = "x"
let a_y = "y"
let a_x1 = "x1"
let a_x2 = "x2"
let a_y1 = "y1"
let a_y2 = "y2"
let a_fx = "fx"
let a_fy = "fy"
let a_cx = "cx"
let a_cy = "cy"
let a_r = "r"
let a_gradient_units = "gradientsUnits" 
let a_transform = "transform"
let a_pattern_units = "patternUnits"
let a_fill = "fill"
let a_stroke = "stroke"
let a_stroke_width = "stroke-width"
let a_stroke_miterlimit = "stroke-miterlimit"
let a_stroke_dashoffset = "stroke-dashoffset"
let a_stroke_dasharray = "stroke-dasharray"
let a_stroke_linecap = "stroke-linecap"
let a_stroke_linejoin = "stroke-linejoin"
let a_fill_rule = "fill-rule" 
let av_linear_rgb = "linearRGB"
let av_user_space = "userSpaceOnUse"
let av_none = "none"
let av_butt = "butt"
let av_round = "round" 
let av_square = "square"
let av_linecap = function
  | `Butt -> av_butt | `Round -> av_round | `Square -> av_square
let av_miter = "miter"
let av_bevel = "bevel"
let av_round = "round"
let av_linejoin = function 
  | `Miter -> av_miter | `Bevel -> av_bevel | `Round -> av_round
let av_evenodd = "evenodd"
let av_nonzero = "nonzero"
let av_fill_rule = function 
  | `Even_odd -> "evenodd" | `Non_zero -> "nonzero"

type iid = int (* Base image id. *)
type pid = int (* Path id. *)
type renderer = 
    { o : output;
      mutable id : int;                                     (* Id generator. *)
      view_rect : P.t;                                     (* Rendered rect. *)
      paths : (P.t, pid) Hashtbl.t;                    (* Maps paths to ids. *)
      imgs : (I.t, iid) Hashtbl.t;               (* Maps base images to ids. *)
      mutable todo : (iid * I.t) list;                  (* Images to output. *)
      mutable g_atts : (string, string) Hashtbl.t;   (* Next <g> attributes. *)
      g_tr : Buffer.t;                                (* Next <g> transform. *)
      mutable depth : int;                                 (* Nesting depth. *)
      mutable tags : string list; }                        (* Tags to close. *)

let new_id r = r.id <- r.id + 1; r.id
let path_id r p = try Hashtbl.find r.paths p with Not_found -> 
  let id = new_id r in 
  Hashtbl.add r.paths p id; 
  id

let img_id r i = try Hashtbl.find r.imgs i with Not_found -> 
  let id = new_id r in 
  Hashtbl.add r.imgs i id; 
  r.todo <- (id, i) :: r.todo;     (* Asking for an image id creates todo's. *)
  id 

let id_str id = str "i%d" id
let img_ref_str img r = str "url(#i%d)" (img_id r img)    (* Creates todo's. *)
let o_str n r = outs r.o n; r
let o_chr c r = outc r.o c; r
let o_sp = o_chr ' '
let o_nop r = r
let o_indent r = for i = 0 to (r.depth - 1) * 2 do outc r.o ' ' done; r
let o_v2 v = o_str & str "%g %g" (V2.x v) (V2.y v)
let o_float f = if V1.is_zero f then o_chr '0' else o_str (str "%g" f)
let o_bool b = if b then o_chr '1' else o_chr '0'
let o_list out l r = List.fold_left (fun r e -> out e r) r l
let o_id id = o_str & id_str id
let o_path_ref path r = r >> o_str (str "#i%d" (path_id r path))
let o_alpha c = o_float & Color.a c
let o_srgb c = 
  let p f = (f c) *. 100. in (* TODO conversion to sRGB *)
  o_str & str "rgb(%g%%,%g%%,%g%%)" (p Color.r) (p Color.g) (p Color.b)

let o_att a value r =                                    (* Output attribute. *)
  r >> o_chr ' ' >> o_str a >> o_str "=\x22" >> value >> o_chr '\x22'

let o_stag n atts r =                                    (* Output start tag. *)
  let r = r >> o_indent >> o_chr '<' >> o_str n >> atts >> o_str ">\n" in
  r.tags <- n :: r.tags;
  r.depth <- r.depth + 1;
  r

let o_etag r = match r.tags with                           (* Output end tag. *)
|  n :: tags -> 
    r.depth <- r.depth - 1;
    r.tags <- tags;
    r >> o_indent >> o_str "</" >> o_str n >> o_str ">\n"
| [] -> assert false

let o_tag n atts r =                                 (* Output empty element. *)
  r >> o_indent >> o_chr '<' >> o_str n >> atts >> o_str "/>\n"

let o_stops stops r =
  let o_stop (offset, c) = 
    o_tag t_stop begin fun r -> r >>
      o_att a_offset (o_float offset) >>
      o_att a_stop_color (o_srgb c) >>
      o_att a_stop_opacity (o_alpha c) 
    end
  in
  r >> o_list o_stop stops
    
let o_base i o_id r = match i with
| I.Mono c -> (* solidColor is mentioned but doesn't seem to exist in 1.1. *)
    let stops = [ (1., c) ] in r >> 
    o_stag t_linear_gradient o_id >> o_stops stops >> o_etag
| I.Axial (p, p', stops) -> r >> 
    o_stag t_linear_gradient begin fun r -> r >> 
      o_id >> 
      o_att a_gradient_units (o_str av_user_space) >>
      o_att a_x1 (o_float & Pt.x p) >> 
      o_att a_y1 (o_float & Pt.y p) >>
      o_att a_x2 (o_float & Pt.x p') >> 
      o_att a_y2 (o_float & Pt.y p')
    end >>
    o_stops stops >>
    o_etag
| I.Radial (f, c, radi, stops) -> r >> 
    o_stag t_radial_gradient begin fun r -> r >>
      o_id >>
      o_att a_gradient_units (o_str av_user_space) >>
      o_att a_fx (o_float & Pt.x f) >> 
      o_att a_fy (o_float & Pt.y f) >> 
      o_att a_cx (o_float & Pt.x c) >> 
      o_att a_cy (o_float & Pt.y c) >> 
      o_att a_r (o_float radi)
    end >>
    o_stops stops >>
    o_etag
| I.Pattern (v, i) -> failwith "unimpl" (* TODO *)
| I.Ri (r, ri) -> unsupported "raster images" (* TODO *)

let o_cut part path r =
  let a = match part with I.Area -> a_stroke | I.Outline -> a_fill in r >> 
  o_tag t_use begin fun r -> r >>
    o_att a_xlink_href (o_path_ref path) >>
    o_att a (o_str av_none)
  end

(* These functions set <g> attributes in r.g_atts, this allows to 
   accumulate state changes and output a <g> only when it
   becomes necessary. *)

let g_param r p = 
  let g_set att value = Hashtbl.replace r.g_atts att value in
  match p with
  | `Width w -> g_set a_stroke_width (str "%g" w)
  | `Cap c -> g_set a_stroke_linecap (av_linecap c)
  | `Join join -> g_set a_stroke_linejoin (av_linejoin join)
  | `Miter_limit limit -> g_set a_stroke_miterlimit (str "%g" limit)
  | `Area arule -> g_set a_fill_rule (av_fill_rule arule) 
  | `Dashes (phase, dashes) -> 
    begin match dashes with
    | [] -> g_set a_stroke_dasharray av_none
    | dl ->
	let d_str = String.concat "," & List.map (fun d -> str "%g" d) dl in
	g_set a_stroke_dashoffset (str "%g" phase);
	g_set a_stroke_dasharray d_str
    end
  | `Cloth i -> 
      let id = img_ref_str i r in 
      g_set a_fill id; 
      g_set a_stroke id
  | `Antialias _ -> () (* TODO *)
  | `Flatness _ -> () (* TODO *)
  | `Blender _ -> () (* TODO *)

let g_params r trl = List.iter (g_param r) trl

let g_tr r tr =                                   (* Accumulates transforms. *)
  let add s = Buffer.add_string r.g_tr s in
  match tr with
  | I.Transl v -> add & str " translate(%g,%g)" (V2.x v) (V2.y v)
  | I.Rot angle -> add & str " rotate(%g)" angle
  | I.Scale s -> add & str " scale(%g,%g)" (V2.x s) (V2.y s)
  | I.Shear s -> add & str " matrix(1,%g,%g,1,0,0)" (V2.y s) (V2.x s)
  | I.Affine m -> 
      add & str " matrix(%g,%g,%g,%g,%g,%g)"
        (M3.el m 0 0) (M3.el m 0 1) (M3.el m 1 0)
	(M3.el m 1 1)  (M3.el m 2 0)  (M3.el m 2 1)

let g_trs r trl = List.iter (g_tr r) trl

let o_g_atts r = (* Outputs <g> attributes according to r.g_atts and r.g_tr. *)
  let tr = Buffer.contents r.g_tr in
  let out_tr = if tr <> "" then o_att a_transform (o_str tr) else o_nop in 
  let out_att_str a v = o_att a (o_str v) in 
  let r = r >> out_tr >> Hashtbl.fold out_att_str r.g_atts in
  Buffer.clear r.g_tr;
  Hashtbl.clear r.g_atts;
  r

(* TODO on State we can now directly output the g. *)
let rec o_img l r = 
  let no_g r = Buffer.length r.g_tr = 0 && Hashtbl.length r.g_atts = 0 in
  let o_g_start r = r >> o_stag t_g o_g_atts in
  let out_in_g out r = r >> o_g_start >> out >> o_etag in 
  match l with
  | [] :: [] -> r
  | [] :: l -> r >> o_etag >> o_img l
  | (img :: l) :: l' -> 
      begin match img with
      | I.State ((pl, tl), i) -> 
	  g_params r pl; g_trs r tl; r >> o_img ((i :: l) :: l')
      | I.Base i -> 
	  let base = o_base i o_nop in 
	  let out = if no_g r then base else out_in_g base in
          r >> out >> o_img (l :: l')
      | I.Cut (part, path) ->
	  let cut = o_cut part path in 
	  let out = if no_g r then cut else out_in_g cut in 
	  r >> out >> o_img (l :: l')
      | I.Blend (i, i') -> 
	  if no_g r then r >> o_img ((i :: i' :: l) :: l') else
	  r >> o_g_start >> o_img ((i :: i' :: []) :: l :: l')
      end
  | [] -> assert false

let rec o_todo r = match r.todo with 
| [] -> r
| (id, i) :: l -> 
    r.todo <- l; 
    match i with
    | I.Base i -> r >> o_base i (o_att a_id (o_id id)) >> o_todo
    | _ -> r >> 
	o_stag t_pattern begin fun r -> r >>
	  o_att a_id (o_id id) >>
	  o_att a_width (o_str "1.2") >>                     (* infinite *)
	  o_att a_height (o_str "1.2") >>                    (* infinite *)
	  o_att a_pattern_units (o_str av_user_space)
	end >> 
	o_img [ [ i ] ] >> 
	o_etag >>
	o_todo

let o_path p id r = 
  let o_seg seg r = match seg with 
  | P.Line pt -> r >> o_chr 'L' >> o_v2 pt
  | P.Qcurve (c, pt) -> r >> o_chr 'Q' >> o_v2 c >> o_sp >> o_v2 pt
  | P.Ccurve (c, c', pt) -> 
      r >> o_chr 'C' >> o_v2 c >> o_sp >> o_v2 c' >> o_sp >> o_v2 pt 
  | P.Close -> r >> o_chr 'Z'
  | P.Earc(large, cw, radii, angle, pt) -> 
      r >> o_chr 'A' >> o_v2 radii >> o_sp >> 
      o_float (rad_to_deg angle) >> o_sp >> o_bool large >> 
      o_sp >> o_bool (not cw) >> o_sp >> o_v2 pt
  in
  let o_sub (pt, segs) r = r >> 
    o_chr 'M' >> o_v2 pt >> o_list o_seg (List.rev segs)
  in r >> 
  o_tag t_path begin fun r -> r >> 
    o_att a_id (o_id id) >> 
    o_att a_d (o_list o_sub (List.rev p))
  end
    
let o_frame_img view i r = 
  let o = Rect.o view in 
  let s = Rect.size view in
  let m = m3 
      1. 0.      (-. Pt.x o)
      0. (-. 1.) (Size.h s +. Pt.y o)
      0. 0.      1. 
  in r >> 
  o_tag t_rect begin fun r -> r >>
    o_att a_width (o_float & Size.w s) >>
    o_att a_height (o_float & Size.h s) >>
    o_att a_fill (o_str (img_ref_str (I.default & I.affine m & i) r))
  end >>
  o_img [ [  ] ] >>
  o_stag "defs" o_nop >>
  o_todo >>
  Hashtbl.fold o_path r.paths >>
  o_etag

let output ?title o (size, view, i) =
  let view_rect = P.rect view P.empty in
  let r = 
    { o = o; 
      id = 0;
      view_rect = view_rect;
      paths = Hashtbl.create 255;
      imgs = Hashtbl.create 255;
      todo = [];
      g_atts = Hashtbl.create 14;
      g_tr = Buffer.create 32;
      depth = 0;
      tags = []; }
  in
  let o_title r = match title with 
  | None -> r >> o_nop 
  | Some t -> r >> 
      o_stag t_title o_nop >> 
      o_indent >> o_str t >> o_chr '\n' >>
      o_etag
  in
  ignore begin r >>
    o_str "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" >>
    o_stag t_svg begin fun r -> r >>
      o_att a_version (o_str "1.1") >>
      o_att "xmlns" (o_str "http://www.w3.org/2000/svg") >>
      o_att "xmlns:xlink" (o_str "http://www.w3.org/1999/xlink") >>
      o_att a_width (o_str & str "%gcm" & 100. *. Size.w size) >>
      o_att a_height (o_str & str "%gcm" & 100. *. Size.h size) >>
      o_att a_view_box (fun r -> r >> o_str "0 0 " >> o_v2 (Rect.size view)) >>
      o_att a_preserve_aspect_ratio (o_str av_none) >>
      o_att a_color_interpol (o_str av_linear_rgb) >>
      o_att a_color_interpol_filters (o_str av_linear_rgb) 
    end >> 
    o_title >>
    o_frame_img view i >>
    o_etag
  end
