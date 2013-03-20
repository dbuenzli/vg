(*----------------------------------------------------------------------------
   Copyright (c) %%COPYRIGHTYEAR%%, Daniel C. Bünzli. All rights reserved.     
   Distributed under a BSD license, see ../LICENSE.                            
  ----------------------------------------------------------------------------*)
open Gg2;;
open Backend;;

let str = Printf.sprintf
let unsupported s = unsupported (str "svg: %s" s)
let rad_to_deg a = (a *. 180.) /. pi

(* Tag names. *)
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
let t_clip_path = "clipPath"

(* Attribute names. *)
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

(* Attribute values. *)
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
let av_fill_rule = function `Even_odd -> av_evenodd | `Non_zero -> av_nonzero

(* Renderer type. *)
type iid = int (* Base image id. *)
type pid = int (* Path id. *)
type renderer = 
    { o : output;
      buf : Buffer.t;
      mutable id : int;                                     (* Id generator. *)
      paths : (P.t, pid) Hashtbl.t;                    (* Maps paths to ids. *)
      imgs : (I.t, iid) Hashtbl.t;                    (* Maps images to ids. *)
      mutable todo : (I.t * iid * I.t) list;
                                    (* Current cloth + id + image to output. *)
      mutable cloth : I.t;                                 (* Current cloth. *)
      mutable depth : int;                                 (* Nesting depth. *)
      mutable tags : string list; }                        (* Tags to close. *)

let new_id r = r.id <- r.id + 1; r.id
let path_id r p = try Hashtbl.find r.paths p with Not_found -> 
  let id = new_id r in 
  Hashtbl.add r.paths p id; 
  id

let img_id r i =             (* N.B. asking for an image id creates todo's. *)
  try Hashtbl.find r.imgs i with Not_found ->
  let id = new_id r in 
  Hashtbl.add r.imgs i id;
  r.todo <- (r.cloth, id, i) :: r.todo;     
  id 

(* Output is continuation based. *)

let buf_len = 1024 * 1024
let flush r = r.o r.buf; Buffer.clear r.buf

(* TODO introduce fmt instead of using str *)
let o_str s k r = 
  if Buffer.length r.buf > buf_len then flush r;
  Buffer.add_string r.buf s

let o_chr c k r = Buffer.add_char r.buf c; k r 

let ( >> ) f g x = f (g x)
let nop k r = k r
let stop r = ()
let o_eof = stop
let o_sp = o_chr ' '
let o_indent k r = 
  for i = 0 to (r.depth - 1) * 2 do 
    Buffer.add_char r.buf ' ' 
  done; 
  k r

let o_v2 v = o_str (str "%g %g" (V2.x v) (V2.y v))
let o_float f = if V1.is_zero f then o_chr '0' else o_str (str "%g" f)
let o_bool b = if b then o_chr '1' else o_chr '0'
let o_list out l k r = List.iter (fun e -> out e stop r) l; k r
let o_alpha c = o_float (Color.a c)
let o_srgb c = 
  let p f = (f c) *. 100. in (* TODO conversion to sRGB *)
  o_str (str "rgb(%g%%,%g%%,%g%%)" (p Color.r) (p Color.g) (p Color.b))

let o_id id = o_str (str "i%d" id) 
let o_id_ref id = o_chr '#' >> o_id id
let o_path_ref path k r = o_id_ref (path_id r path) stop r; k r
let o_img_ref img k r = o_id_ref (img_id r img) stop r; k r
let o_img_url_ref img = o_str "url(" >> o_img_ref img >> o_chr ')'

let o_att a value =                                      (* Output attribute. *)
  o_sp >> o_str a >> o_str "=\x22" >> value >> o_chr '\x22'

let o_stag n atts k r =                                  (* Output start tag. *)
  (o_indent >> o_chr '<' >> o_str n >> atts >> o_str ">\n") stop r;
  r.tags <- n :: r.tags;
  r.depth <- r.depth + 1;
  k r

let o_etag k r = match r.tags with                         (* Output end tag. *)
|  n :: tags -> 
    r.depth <- r.depth - 1;
    r.tags <- tags;
    (o_indent >> o_str "</" >> o_str n >> o_str ">\n") k r
| [] -> assert false

let o_tag n atts =                                   (* Output empty element. *)
  o_indent >> o_chr '<' >> o_str n >> atts >> o_str "/>\n"

let o_stops stops =
  let o_stop (offset, c) = 
    o_tag t_stop begin
      o_att a_offset (o_float offset) >>
      o_att a_stop_color (o_srgb c) >>
      o_att a_stop_opacity (o_alpha c) 
    end
  in
  o_list o_stop stops
  
let o_base id = function 
| I.Mono c -> (* solidColor is mentioned but doesn't seem to exist in 1.1. *)
  let stops = [ (1., c) ] in
  o_stag t_linear_gradient (o_att a_id (o_id id)) >> o_stops stops >> o_etag
| I.Axial (p, p', stops) ->
    o_stag t_linear_gradient begin
      o_att a_id (o_id id) >> 
      o_att a_gradient_units (o_str av_user_space) >>
      o_att a_x1 (o_float & Pt.x p) >> 
      o_att a_y1 (o_float & Pt.y p) >>
      o_att a_x2 (o_float & Pt.x p') >> 
      o_att a_y2 (o_float & Pt.y p')
    end >>
    o_stops stops >>
    o_etag
| I.Radial (f, c, radi, stops) ->
    o_stag t_radial_gradient begin
      o_att a_id (o_id id) >>
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

(*
let o_cut part path r =
  let a = match part with I.Area -> a_stroke | I.Outline -> a_fill in
  o_tag t_use begin
    o_att a_xlink_href (o_path_ref path) >>
    o_att a (o_str av_none)
  end
*)  

let o_param = function 
| `Width w -> o_att a_stroke_width (o_str & str "%g" w)
| `Cap c -> o_att a_stroke_linecap (o_str & av_linecap c)
| `Join join -> o_att a_stroke_linejoin (o_str & av_linejoin join)
| `Miter_limit limit -> o_att a_stroke_miterlimit (o_str & str "%g" limit)
| `Area arule -> o_att a_fill_rule (o_str & av_fill_rule arule) 
| `Dashes (phase, dashes) -> 
    begin match dashes with
    | [] -> o_att a_stroke_dasharray (o_str av_none)
    | dl ->
	let d_str = String.concat "," & List.map (fun d -> str "%g" d) dl in
	o_att a_stroke_dashoffset (o_str & str "%g" phase) >>
	o_att a_stroke_dasharray (o_str d_str)
    end
| `Antialias _ -> nop (* TODO *)
| `Flatness _ -> nop (* TODO *)
| `Blender _ -> nop (* TODO *)
| `Cloth i -> nop

let o_tr = function
| I.Transl v -> o_str & str " translate(%g,%g)" (V2.x v) (V2.y v)
| I.Rot angle -> o_str & str " rotate(%g)" angle
| I.Scale s -> o_str & str " scale(%g,%g)" (V2.x s) (V2.y s)
| I.Shear s -> o_str & str " matrix(1,%g,%g,1,0,0)" (V2.y s) (V2.x s)
| I.Affine m -> 
    o_str & str " matrix(%g,%g,%g,%g,%g,%g)"
      (M3.el m 0 0) (M3.el m 0 1) (M3.el m 1 0)
      (M3.el m 1 1)  (M3.el m 2 0)  (M3.el m 2 1)

let set_cloth pl k r = 
  try match List.find (function `Cloth _ -> true | _ -> false) pl with
  | `Cloth i -> r.cloth <- i; k r 
  | _ -> assert false
  with Not_found -> k r

let rec o_img cloth id = function  (* TODO not tail recursive *)
| I.State ((pl, tl), i) ->
    (* conditionalize on i for Base gradient matrix ?. *)
    let pl = I.strip_params pl in
    set_cloth pl >> 
    o_stag t_g begin 
      o_att a_id (o_id id) >>
      o_list o_param pl >>
      (if tl <> [] then o_att a_transform (o_list o_tr tl) else nop)
    end >>
    o_tag t_use (o_att a_xlink_href (o_img_ref i)) >>
    o_etag
| I.Base i -> o_base id i
| I.Cut (part, path) ->
    begin match cloth with 
    | (I.Base _ as i) | I.State (_, (I.Base _ as i)) ->
	let stroke, fill = match part with
	| I.Area -> o_str av_none, o_img_url_ref i
	| I.Outline -> o_img_url_ref i, o_str av_none
	in
	o_stag t_g (o_att a_id (o_id id)) >>
	o_tag t_use begin
	  o_att a_xlink_href (o_path_ref path) >>
	  o_att a_stroke stroke >> 
	  o_att a_fill fill
	end >>
	o_etag
    | _ -> nop (* TODO *)
    end
| I.Blend il ->
    let use i = o_tag t_use (o_att a_xlink_href (o_img_ref i)) in
    o_stag t_g (o_att a_id (o_id id)) >> o_list use (List.rev il) >> o_etag
      
let rec o_todo k r = match r.todo with 
| [] -> nop k r
| (cloth, id, i) :: l -> r.todo <- l; o_img cloth id i stop r; o_todo k r

let o_path p id =
  let o_seg = function 
  | P.Start pt -> o_chr 'M' >> o_v2 pt 
  | P.Line pt -> o_chr 'L' >> o_v2 pt
  | P.Qcurve (c, pt) -> o_chr 'Q' >> o_v2 c >> o_sp >> o_v2 pt
  | P.Ccurve (c, c', pt) -> 
      o_chr 'C' >> o_v2 c >> o_sp >> o_v2 c' >> o_sp >> o_v2 pt 
  | P.Earc(large, cw, radii, angle, pt) -> 
      o_chr 'A' >> o_v2 radii >> o_sp >> 
      o_float (rad_to_deg angle) >> o_sp >> o_bool large >> 
      o_sp >> o_bool (not cw) >> o_sp >> o_v2 pt
  | P.Close -> o_chr 'Z'
  in
  let o_sub segs = o_list o_seg (List.rev segs) in
  o_tag t_path begin
    o_att a_id (o_id id) >> 
    o_att a_d (o_list o_sub (List.rev p))
  end

let o_frame_img view i =
  let o = Rect.o view in 
  let s = Rect.size view in
  let m = m3 
      1. 0.      (-. Pt.x o)
      0. (-. 1.) (Size.h s +. Pt.y o)
      0. 0.      1. 
  in
  let o_paths k r = (Hashtbl.fold o_path r.paths) k r in
  o_tag t_use begin 
    o_att a_xlink_href (o_img_ref & I.default & I.affine m & i)
  end >>
  o_stag "defs" nop >>
  o_todo >>
  o_paths >>
  o_etag

let output ?title o (size, view, i) =
  let r = 
    { o = o; 
      buf = Buffer.create buf_len;
      id = 0;
      paths = Hashtbl.create 255;
      imgs = Hashtbl.create 255;
      todo = [];
      cloth = I.default_cloth;
      depth = 0;
      tags = []; }
  in
  let o_title = match title with 
  | None -> nop 
  | Some t ->                                            (* TODO escape *)
      o_stag t_title nop >> 
      o_indent >> o_str t >> o_chr '\n' >>
      o_etag
  in
  begin
    o_str "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" >>
    o_stag t_svg begin
      o_att a_version (o_str "1.1") >>
      o_att "xmlns" (o_str "http://www.w3.org/2000/svg") >>
      o_att "xmlns:xlink" (o_str "http://www.w3.org/1999/xlink") >>
      o_att a_width (o_str & str "%gcm" & 100. *. Size.w size) >>
      o_att a_height (o_str & str "%gcm" & 100. *. Size.h size) >>
      o_att a_view_box (o_str "0 0 " >> o_v2 (Rect.size view)) >>
      o_att a_preserve_aspect_ratio (o_str av_none) >>
      o_att a_color_interpol (o_str av_linear_rgb) >>
      o_att a_color_interpol_filters (o_str av_linear_rgb) 
    end >> 
    o_title >>
    o_frame_img view i >>
    o_etag
  end o_eof r
