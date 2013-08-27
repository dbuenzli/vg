(*---------------------------------------------------------------------------
   Copyright 2013 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

open Gg
open Vg
open Vgr.Private.Data

external ( @@ ) : ('a -> 'b) -> 'a -> 'b = "%apply"

let mm_to_pt = 72. /. 25.4

(* Unsafe string byte manipulations. If you don't believe the author's
   invariants, replacing with safe versions makes everything safe in
   the module. He won't be upset. *)

let unsafe_byte s j = Char.code (String.unsafe_get s j)
let unsafe_array_get = Array.unsafe_get
let unsafe_chr = Char.unsafe_chr

type id = int (* PDF object ids *) 
type g_state = { g_tr : M3.t } 
type cmd = Pop of g_state | Draw of Vgr.Private.Data.image

type state = 
  { r : Vgr.Private.renderer;                    (* corresponding renderer. *)
    share : int;                            (* page resource sharing limit. *)
    xmp : string option;                            (* XMP metadata packet. *)
    buf : Buffer.t;                                   (* formatting buffer. *) 
    mutable cost : int;                          (* cost counter for limit. *) 
    mutable view : Gg.box2;           (* current renderable view rectangle. *)
    mutable todo : cmd list;                        (* commands to perform. *) 
    mutable id : int;                (* PDF object id (and name) generator. *) 
    mutable bytes : int;                     (* current output byte offset. *) 
    mutable             (* length obj id and offset of current page stream. *) 
      stream_info : int * int;   
    mutable index : (int * int) list;      (* object id, byte offset index. *)
    mutable id_resources : int;                (* current resource dict id. *) 
    mutable page_objs : int list;                      (* pages object ids. *)
    paths : (Vgr.Private.Data.path, id) Hashtbl.t;                      
    prims : (Vgr.Private.Data.primitive, id) Hashtbl.t;
    outlines : (P.outline, id) Hashtbl.t; 
    alphas : (float, id) Hashtbl.t;
    mutable s_tr : M3.t;  (* current transformation without view transform. *)
    mutable s_alpha : float; 
    mutable s_blender : Vgr.Private.Data.blender; (* current blending mode. *)
    mutable s_outline : P.outline; }       (* current outline stroke state. *) 

let partial = Vgr.Private.partial
let limit s = Vgr.Private.limit s.r 
let warn s w = Vgr.Private.warn s.r w 
let image i = Vgr.Private.I.of_data i 

let n_linear_srgb = "/cs_linear_srgb" 
let id_page_root = 1
let id_linear_srgb = 2
let id_first_resources = 3

let max_buf = 65528 
let byte_offset s = s.bytes + Buffer.length s.buf
let new_obj_id s = s.id <- s.id + 1; s.id
let obj_count s = s.id + 1
let new_page s = 
  let id = new_obj_id s in 
  s.page_objs <- id :: s.page_objs; id

let get_id s h v = try Hashtbl.find h v with 
| Not_found -> 
    let id = new_obj_id s in 
    Hashtbl.add h v id; id

let get_path_id s p = get_id s s.paths p 
let get_outline_id s o = get_id s s.outlines o
let get_alpha_id s alpha = get_id s s.alphas alpha

let pop_gstate s = Pop { g_tr = s.s_tr }
let set_gstate s g = s.s_tr <- g.g_tr 
let uncut_bounds s = 
  Vgr.Private.Data.of_path (P.empty >> P.rect (Box2.tr (M3.inv s.s_tr) s.view))

let rec b_id_list b = function 
| [] -> () | id :: ids -> Printf.bprintf b " %d 0 R" id; b_id_list b ids

let b_fmt s fmt = Printf.bprintf s.buf fmt
let b_str s str = Buffer.add_string s.buf str

let utf_8_len = [| (* uchar byte length according to first UTF-8 byte. *)
  1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 
  1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 
  1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 
  1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 
  1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 1; 
  1; 1; 1; 1; 1; 1; 1; 1; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 
  0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 
  0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 
  0; 0; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 
  2; 2; 2; 2; 2; 2; 2; 2; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 3; 
  4; 4; 4; 4; 4; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0 |]

let u_bom = 0xFEFF                                                   (* BOM. *)
let u_rep = 0xFFFD                                 (* replacement character. *)
let u_lpar = 0x0028           
let u_rpar = 0x0029 
let u_bslash = 0x005C
let b_str_text s str =       (* adds UTF-8 [str] as an UTF-16BE text string. *) 
  let add_utf16be b u =
    let w byte = Buffer.add_char b (unsafe_chr byte) in          (* inlined. *)
    if u < 0x10000 then begin 
      if u = u_lpar || u = u_rpar || u = u_bslash             (* PDF escape. *)
      then (w 0x00; w u_bslash (* escape next *byte* *) ; w u) 
      else (w (u lsr 8); w (u land 0xFF)) 
    end else begin 
      let u' = u - 0x10000 in
      let hi = (0xD800 lor (u' lsr 10)) in
      let lo = (0xDC00 lor (u' land 0x3FF)) in
      w (hi lsr 8); w (hi land 0xFF);
      w (lo lsr 8); w (lo land 0xFF)
    end
  in
  let rec loop str i l =
    if i = l then () else 
    let malformed () = add_utf16be s.buf u_rep in
    let uchar u = add_utf16be s.buf u in 
    match unsafe_array_get utf_8_len (unsafe_byte str i) with
    | 0 -> malformed (); loop str (i + 1) l
    | need -> 
        let rem = l - i in
        if rem < need then malformed () else
        begin 
          begin match need with
          | 1 -> uchar (unsafe_byte str i)
          | 2 -> 
              let b0 = unsafe_byte str i in 
              let b1 = unsafe_byte str (i + 1) in 
              if b1 lsr 6 != 0b10 then malformed () else
              uchar (((b0 land 0x1F) lsl 6) lor (b1 land 0x3F))
          | 3 -> 
              let b0 = unsafe_byte str i in 
              let b1 = unsafe_byte str (i + 1) in 
              let b2 = unsafe_byte str (i + 2) in 
              let c = (((b0 land 0x0F) lsl 12) lor 
                         ((b1 land 0x3F) lsl 6) lor 
		         (b2 land 0x3F))
              in
              if b2 lsr 6 != 0b10 then malformed () else
              begin match b0 with
              | 0xE0 -> if b1 < 0xA0 || 0xBF < b1 then malformed () else uchar c
              | 0xED -> if b1 < 0x80 || 0x9F < b1 then malformed () else uchar c
              | _ -> if b1 lsr 6 != 0b10 then malformed () else uchar c
              end
          | 4 -> 
              let b0 = unsafe_byte str i in 
              let b1 = unsafe_byte str (i + 1) in 
              let b2 = unsafe_byte str (i + 2) in 
              let b3 = unsafe_byte str (i + 3) in 
              let c = (((b0 land 0x07) lsl 18) lor 
                         ((b1 land 0x3F) lsl 12) lor 
		         ((b2 land 0x3F) lsl 6) lor 
                         (b3 land 0x3F))
              in
              if b3 lsr 6 != 0b10 || b2 lsr 6 != 0b10 then malformed () else
              begin match b0 with
              | 0xF0 -> if b1 < 0x90 || 0xBF < b1 then malformed () else uchar c
              | 0xF4 -> if b1 < 0x80 || 0x8F < b1 then malformed () else uchar c
              | _ -> if b1 lsr 6 != 0b10 then malformed () else uchar c
              end
          | _ -> assert false
          end;
          loop str (i + need) l
        end
  in
  add_utf16be s.buf u_bom; (* add BOM *) 
  loop str 0 (String.length str)
  
let b_start s = b_str s "%PDF-1.7\n%\xCF\xC3\xE1\xED\xEC\n"
let b_obj_end s = b_str s "endobj\n"
let b_obj_start s id =
  s.index <- (id, byte_offset s) :: s.index;
  b_fmt s "%d 0 obj\n" id

let b_length_obj s id len = 
  b_obj_start s id; 
  b_fmt s "%i\n" len;
  b_obj_end s

let flush s k r = 
  let clear k r = Buffer.clear s.buf; k r in 
  let len = Buffer.length s.buf in
  s.bytes <- s.bytes + len; 
  Vgr.Private.writebuf s.buf 0 len (clear k) r

let w_buf s k r = if Buffer.length s.buf >= max_buf then flush s k r else k r
let w_clip s a i path_id k r =  
  s.todo <- (Draw i) :: s.todo; k r

let w_primitive_cut s a (p, trs) path_id k r = match p with 
| Const c ->
    let alpha_id = get_alpha_id s (Color.a c) in
    b_fmt s "\n/gs%d gs" alpha_id;
    begin match a with
    | `Anz -> 
        b_fmt s "\n%f %f %f sc" (Color.r c) (Color.g c) (Color.b c);
        b_fmt s "\n/p%d Do f" path_id;
    | `Aeo ->
        b_fmt s "\n%f %f %f sc" (Color.r c) (Color.g c) (Color.b c);
        b_fmt s "\n/p%d Do f*" path_id;
    | `O o ->
        let outline_id = get_outline_id s o in
        b_fmt s "\n%f %f %f SC" (Color.r c) (Color.g c) (Color.b c);
        b_fmt s "\n/gs%d gs" outline_id; 
        b_fmt s "\n/p%d Do S" path_id      
    end; 
    w_buf s k r
| Axial (stops, p1, p2) -> k r
| Radial (stops, f, c, radius) -> k r
| Raster _ -> assert false 

let w_transform s tr k r = 
  begin match tr with 
  | Move pt -> 
      b_fmt s "\n1 0 0 1 %f %f cm" (V2.x pt) (V2.y pt)
  | Rot a ->
      let cos = cos a in 
      let sin = sin a in 
      b_fmt s "\n%f %f %f %f 0 0 cm" cos sin (-. sin) cos
  | Scale sv -> 
      b_fmt s "\n%f 0 0 %f 0 0 cm" (V2.x sv) (V2.y sv)
  | Matrix m ->
      M3.(b_fmt s "\n%f %f %f %f %f %f cm"
            (e00 m) (e10 m) (e01 m) (e11 m) (e02 m) (e12 m))
  end;
  w_buf s k r

(*
let rec w_transforms s trs k r = function
| [] -> k r | tr :: trs -> w_transform s tr (s trs (k 
*)

let tr_primitive i = 
  let rec loop acc = function 
  | Primitive (Raster _) | Blend _ | Cut _ | Cut_glyphs _ -> None 
  | Primitive p -> Some (p, List.rev acc) 
  | Tr (tr, i) -> loop (tr :: acc) i 
  in
  loop [] i

let w_cut s a p i k r =
  let path_id = get_path_id s p in
  match i with 
  | Primitive (Raster _) -> 
      warn s (`Other ("TODO raster unimplemented")); k r
  | Primitive p -> 
      w_primitive_cut s a (p, []) path_id k r
  | Tr _ as i -> 
      begin match tr_primitive i with 
      | None -> w_clip s a i path_id k r 
      | Some p_trs ->     
          w_primitive_cut s a p_trs path_id k r 
      end
  | Blend _ | Cut _ | Cut_glyphs _ as i -> 
      w_clip s a i path_id k r
  
let rec w_image s k r = 
  if s.cost > limit s then (s.cost <- 0; partial (w_image s k) r) else 
  match s.todo with 
  | [] -> 
      let len_id, stream_start = s.stream_info in
      let len = byte_offset s - stream_start in
      b_str s "\nendstream\n"; 
      b_obj_end s;  
      b_obj_start s len_id;                   (* Write object stream length. *) 
      b_fmt s "%i\n" len; 
      b_obj_end s; 
      w_buf s k r
  | Pop gs :: todo ->
      set_gstate s gs; 
      s.todo <- todo; 
      b_fmt s "\nQ"; 
      w_image s k r
  | (Draw i) :: todo -> 
      s.cost <- s.cost + 1; 
      match i with 
      | Primitive _ as i -> (* Uncut primitive just cut to view. *)
          let p = uncut_bounds s in 
          s.todo <- (Draw (Cut (`Anz, p, i))) :: todo; 
          w_image s k r
      | Cut (a, p, i) ->
          s.todo <- todo;
          w_cut s a p i (w_image s k) r
      | Cut_glyphs (a, run, i) -> 
          s.todo <- todo; 
          (* TODO *) 
          w_image s k r
      | Blend (_, _, i, i') -> 
          s.todo <- (Draw i') :: (Draw i) :: todo; 
          w_image s k r
      | Tr (tr, i) -> 
          s.todo <- (Draw i) :: pop_gstate s :: todo;
          b_fmt s "q\n";
          w_transform s tr (w_image s k) r
            
let w_contents s id size k r = 
  let sx = (Size2.w size *. mm_to_pt) /. Box2.w s.view in 
  let sy = (Size2.h size *. mm_to_pt) /. Box2.h s.view in 
  let dx = -. sx *. Box2.ox s.view in 
  let dy = -. sy *. Box2.oy s.view in
  let obj_len_id = new_obj_id s in 
  b_obj_start s id;
  b_fmt s "<< /Length %d 0 R >>\nstream\n" obj_len_id;
  s.stream_info <- (obj_len_id, byte_offset s);
  b_fmt s "%s CS" n_linear_srgb; 
  b_fmt s "\n%s cs" n_linear_srgb; 
  b_fmt s "\n%f 0 0 %f %f %f cm" sx sy dx dy;
  w_image s k r

let w_page size s k r =
  let id = new_page s in 
  let contents_id = new_obj_id s in
  b_obj_start s id;
  b_fmt s "<<\n\
           /Type /Page\n\
           /Parent %d 0 R\n\
           /Resources %d 0 R\n\
           /MediaBox [0 0 %f %f]\n\
           /Contents %d 0 R\n\
           >>\n" 
    id_page_root s.id_resources
    (V2.x size *. mm_to_pt) (V2.y size *. mm_to_pt)
    contents_id;
  b_obj_end s;
  w_contents s contents_id size k r

let w_linear_srgb s k r = 
  let id_icc_stream = new_obj_id s in
  let icc = Color.profile_to_icc Color.p_rgb_l in
  b_obj_start s id_linear_srgb;
  b_fmt s "[/ICCBased %d 0 R]\n" id_icc_stream; 
  b_obj_end s;
  b_obj_start s id_icc_stream; 
  b_fmt s "<<\n\
           /N 3 /Alternate /DeviceRGB\n\
           /Length %d\n\
           >>\nstream\n%s\nendstream\n" (String.length icc) icc;
  b_obj_end s;
  w_buf s k r

let one_div_3 = 1. /. 3. 
let two_div_3 = 2. /. 3. 

let cubic_earc tol cubic acc p0 large cw a r p1 = (* TODO tailrec *)
  match Vgr.Private.P.earc_params p0 large cw a r p1 with
  | None -> (* line with a cubic *)
      let c0 = V2.add (V2.smul two_div_3 p0) (V2.smul one_div_3 p1) in
      let c1 = V2.add (V2.smul one_div_3 p0) (V2.smul two_div_3 p1) in
      cubic c0 c1 p1 acc
  | Some (c, m, t0, t1) -> 
      let mt = (* TODO something better *)
	M2.v (-. (M2.e00 m)) (M2.e10 m) (* gives the tngt to a point *)
             (-. (M2.e01 m)) (M2.e11 m)
      in
      let tol = tol /. max (V2.x r) (V2.y r) in
      let rec loop tol cubic acc p0 t0 p1 t1 = 
      let dt = t1 -. t0 in
      let a = 0.25 *. dt in
      let is_flat = (2.*. (sin a) ** 6.) /. (27.*. (cos a) ** 2.) <= tol in
      if is_flat then 
	let l = (4. *. tan a) /. 3. in
        let c0 = V2.add p0 (V2.smul l (V2.ltr mt (V2.v (sin t0) (cos t0)))) in
        let c1 = V2.sub p1 (V2.smul l (V2.ltr mt (V2.v (sin t1) (cos t1)))) in
        cubic c0 c1 p1 acc
      else
        let t = (t0 +. t1) /. 2. in
	let b = V2.(c + ltr m (V2.v (cos t) (sin t))) in
	loop tol cubic (loop tol cubic acc p0 t0 b t) b t p1 t1
      in
      loop tol cubic acc p0 t0 p1 t1

let w_path s (p, pid) k r =                     (* written in a Form XObject *)
  let b_cubic c0 c1 pt () = 
    V2.(b_fmt s "\n%f %f %f %f %f %f c" 
          (x c0) (y c0) (x c1) (y c1) (x pt) (y pt))
  in
  let rec b_seg last = function 
  | `Sub pt -> 
      b_fmt s "\n%f %f m" (V2.x pt) (V2.y pt); pt
  | `Line pt -> 
      b_fmt s "\n%f %f l" (V2.x pt) (V2.y pt); pt 
  | `Qcurve (c, pt) -> 
      (* Degree elevation *) 
      let c0 = V2.add (V2.smul one_div_3 last) (V2.smul two_div_3 c) in 
      let c1 = V2.add (V2.smul two_div_3 c) (V2.smul one_div_3 pt) in 
      b_cubic c0 c1 pt (); pt
  | `Ccurve (c0, c1, pt) -> 
      b_cubic c0 c1 pt (); pt
  | `Earc (large, cw, angle, radii, pt) -> 
      cubic_earc 1e-3 b_cubic () last large cw angle radii pt; pt 
  | `Close -> 
      b_fmt s "\nh"; last
  in
  let b = P.bounds ~ctrl:true (Vgr.Private.P.of_data p) in  (* TODO *)
  let len_id = new_obj_id s in
  b_obj_start s pid;
  b_fmt s "<< /Subtype /Form /BBox [ %f %f %f %f ] /Length %d 0 R >>\nstream"
    (Box2.minx b) (Box2.miny b) (Box2.maxx b) (Box2.maxy b)
    len_id;
  let stream_start = byte_offset s in 
  ignore (List.fold_left b_seg P2.o (List.rev p));
  let len = byte_offset s - stream_start - 1 (* first NL *) in
  b_fmt s "\nendstream\n";
  b_obj_end s;
  b_length_obj s len_id len;
  w_buf s k r

let rec w_paths s ps k r = match ps with 
| [] -> k r | p :: ps -> w_path s p (w_paths s ps k) r

let w_outline s (o, id) k r = 
  let b_cap b = function 
    | `Butt -> Buffer.add_char b '0' 
    | `Round -> Buffer.add_char b '1' 
    | `Square -> Buffer.add_char b '2'
  in
  let b_join b = function 
  | `Miter -> Buffer.add_char b '0' 
  | `Round -> Buffer.add_char b '1'
  | `Bevel -> Buffer.add_char b '2'
  in
  let b_dashes b = function
  | None -> Buffer.add_string b "[] 0"
  | Some (off, pat) -> 
      let rec b_pat b = function 
      | [] -> () | p :: ps -> Printf.bprintf b " %f" p; b_pat b ps 
      in
      Printf.bprintf b "[%a] %f" b_pat pat off
  in
  b_obj_start s id; 
  b_fmt s "<< /Type /ExtGState\n\
           /LW %f /LC %a /LJ %a /ML %f /D [%a] >>\n" 
    o.P.width b_cap o.P.cap b_join o.P.join (Vgr.Private.P.miter_limit o) 
    b_dashes o.P.dashes;
  b_obj_end s; 
  w_buf s k r
      
let rec w_outlines s os k r = match os with 
| [] -> k r | o :: os -> w_outline s o (w_outlines s os k) r

let w_alpha s (a, id) k r = 
  b_obj_start s id; 
  b_fmt s "<< /Type /ExtGState /ca %f /CA %f >>\n" a a; 
  b_obj_end s; 
  w_buf s k r

let rec w_alphas s alphas k r = match alphas with 
| [] -> k r | alpha :: alphas -> w_alpha s alpha (w_alphas s alphas k) r

let b_paths_refs b paths = 
  let path_ref (_, id) = Printf.bprintf b "/p%d %d 0 R " id id in
  List.iter path_ref paths

let b_gstate_refs b gss = 
  let gs_ref (_, id) = Printf.bprintf b "/gs%d %d 0 R " id id in 
  List.iter gs_ref gss

let w_resources ?(last = false) s k r =
  let release s k r = 
    Hashtbl.clear s.paths; Hashtbl.clear s.prims; Hashtbl.clear s.outlines; 
    Hashtbl.clear s.alphas;
    k r
  in
  let to_list h = Hashtbl.fold (fun v id acc -> (v, id) :: acc) h [] in
  let paths = to_list s.paths in
  let outlines = to_list s.outlines in
  let alphas = to_list s.alphas in 
  b_obj_start s s.id_resources;
  b_fmt s "<<\n\
           /ColorSpace << %s %d 0 R >>\n\
           /XObject << %a>>\n\
           /ExtGState << %a%a>>\n\
           >>\n" 
    n_linear_srgb id_linear_srgb b_paths_refs paths b_gstate_refs outlines
    b_gstate_refs alphas;
  b_obj_end s; 
  if not last then s.id_resources <- new_obj_id s;
  r >>
  w_paths s paths @@
  w_outlines s outlines @@
  w_alphas s alphas @@
  release s @@
  k

let w_page_root s k r = 
  let page_count = List.length s.page_objs in 
  let pages = List.rev s.page_objs in
  b_obj_start s id_page_root; 
  b_fmt s "<<\n\
           /Type /Pages\n\
           /Count %d\n\
           /Kids [%a]\n\
           >>\n" page_count b_id_list pages;
  b_obj_end s; 
  w_buf s k r
  
let w_catalog s id_catalog id_meta k r =
  let b_meta b = function 
  | None -> () 
  | Some id -> Printf.bprintf b "/Metadata %d 0 R\n" id
  in
  b_obj_start s id_catalog;
  b_fmt s "<<\n\
           /Type /Catalog\n\
           /Pages %d 0 R\n\
           %a\
           >>\n" 
    id_page_root b_meta id_meta; 
  b_obj_end s;
  w_buf s k r

let w_xmp_metadata s id_meta k r = match id_meta with 
| None -> k r 
| Some id -> 
    let xmp = match s.xmp with Some xmp -> xmp | None -> assert false in
    let len_id = new_obj_id s in 
    b_obj_start s id;
    b_fmt s 
      "<< /Type /Metadata /Subtype /XML /Length %d 0 R >>\nstream\n" len_id;
    let stream_start = byte_offset s in 
    b_fmt s "<?xpacket begin=\"\xEF\xBB\xBF\" \
                       id=\"W5M0MpCehiHzreSzNTczkc9d\"?>\n\
             %s\n\
             <?xpacket end=\"w\"?>" xmp;
    let len = byte_offset s - stream_start in
    b_str s "\nendstream\n"; 
    b_obj_end s;
    b_length_obj s len_id len;
    w_buf s k r
      
let w_info s id_info k r =       (* just /Producer, rest is handled by XMP. *) 
  b_obj_start s id_info;
  b_str s "<< /Producer (OCaml Vg library %%VERSION%%) >>\n"; 
  b_obj_end s; 
  w_buf s k r

let w_xref_table s k r =                           (* cross reference table *)
  let w_offset s off k r = b_fmt s "%010d 00000 n \n" off; w_buf s k r in
  let rec w_offsets s xref_offset offs k r = match offs with 
  | (_, off) :: offs -> w_offset s off (w_offsets s xref_offset offs k) r
  | [] -> k xref_offset r
  in
  let xref_offset = byte_offset s in
  b_fmt s "xref\n\
           0 %d\n\
           0000000000 65535 f \n" (obj_count s); 
  w_offsets s xref_offset (List.sort compare s.index) k r

let w_trailer s id_catalog id_info k xref_offset r =
  b_fmt s "trailer\n\
           <<\n\
           /Size %d\n\
           /Root %d 0 R\n\
           /Info %d 0 R\n\
           >>\n\
           startxref\n\
           %d\n\
           %%%%EOF" (obj_count s) id_catalog id_info xref_offset;
  w_buf s k r
   
let w_end s k r =                                  
  let id_info = new_obj_id s in
  let id_catalog = new_obj_id s in 
  let id_meta = match s.xmp with None -> None | Some _ -> Some (new_obj_id s) in
  r >> 
  w_resources ~last:true s @@
  w_page_root s @@
  w_linear_srgb s @@
  w_info s id_info @@
  w_catalog s id_catalog id_meta @@ 
  w_xmp_metadata s id_meta @@ 
  w_xref_table s @@
  w_trailer s id_catalog id_info @@ 
  k 

let render s v k r = match v with 
| `End -> w_end s (flush s (Vgr.Private.flush k)) r 
| `Image (size, view, img) ->
    s.todo <- [Draw img]; 
    s.view <- view; 
    s.s_tr <- M3.id;
    s.s_alpha <- 1.0; 
    s.s_blender <- `Over;
    s.s_outline <- P.o;
    begin match List.length s.page_objs with 
    | 0 -> b_start s; w_page size s k r
    | n when n mod s.share = 0 -> w_resources s (w_page size s k) r
    | n -> w_page size s k r
    end
    
let target ?(share = max_int) ?xmp () = 
  let target r _ = 
    true, render { r; 
                   share;
                   xmp;
                   buf = Buffer.create (max_buf + 8);
                   cost = 0; 
                   view = Box2.empty; 
                   todo = []; 
                   id = id_first_resources; 
                   stream_info = (0, 0);
                   bytes = 0; 
                   index = []; 
                   id_resources = id_first_resources;
                   page_objs = []; 
                   paths = Hashtbl.create 255; 
                   prims = Hashtbl.create 255; 
                   outlines = Hashtbl.create 255; 
                   alphas = Hashtbl.create 255;
                   s_tr = M3.id; 
                   s_alpha = 1.; 
                   s_blender = `Over; 
                   s_outline = P.o }
  in
  Vgr.Private.create_target target
  
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
