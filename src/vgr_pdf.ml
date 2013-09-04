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

(* Initially a few things were tried to share paths construction
   operators in XObjects, it turned out it wouldn't work in
   Reader. Path are thus written once per cut unless the *same* path
   is cut repeatedly. This should be less a problem once we get some
   compression in.

   Regarding text rendering a simple approach is taken for now
   according to the arguments given to glyph_cut. If glyph advances
   are explicitly given we draw glyph by glyph using td to position
   them. If glyphs advances are omitted we use the Tj
   operator. Unfortunately PDF still requires use to extract the glyph
   advances from the font which means that the font resolver interface
   is a little bit clumsy at the moment. Depending on how much work
   that entails once Otfm matures we could make it dependent on it and
   do the work internally (but then distribute the PDF backend separately
   in order not to impose Otfm). *)

(* Unsafe string byte manipulations. If you don't believe the author's
   invariants, replacing with safe versions makes everything safe in
   the module. He won't be upset. *)

let unsafe_byte s j = Char.code (String.unsafe_get s j)
let unsafe_array_get = Array.unsafe_get
let unsafe_chr = Char.unsafe_chr

type id = int (* PDF object ids *) 
type gstate =                  (* Subset of the graphics state saved by a q. *) 
  { mutable g_tr : M3.t;           (* current transform with view transform. *) 
    mutable g_scolor : Vgr.Private.Data.primitive; 
    mutable g_fcolor : Vgr.Private.Data.primitive; 
    mutable g_outline : P.outline;
    mutable g_font : Vgr.Private.Data.font;
    mutable g_alpha : float;                              (* unused for now. *) 
    mutable g_blender : Vgr.Private.Data.blender; }       (* unused for now. *)

let dumb_font = Vgr.Private.Data.of_font (Vg.Font.create "dumb" 1.0)
let init_gstate = 
  { g_tr = M3.id; g_scolor = Const Color.black; g_fcolor = Const Color.black;
    g_outline = P.o; g_font = dumb_font; g_alpha = 1.0; g_blender = `Over } 

  
type cmd = Set of gstate | Draw of Vgr.Private.Data.image

type state = 
  { r : Vgr.Private.renderer;                    (* corresponding renderer. *)
    font : Vg.font -> string option;             (* OpenType font resolver. *)
    share : int;                            (* page resource sharing limit. *)
    xmp : string option;                            (* XMP metadata packet. *)
    buf : Buffer.t;                                   (* formatting buffer. *) 
    mutable cost : int;                          (* cost counter for limit. *) 
    mutable view : Gg.box2;           (* current renderable view rectangle. *)
    mutable inv_view_tr : M3.t;      (* view to mediabox inverse transform. *) 
    mutable todo : cmd list;                        (* commands to perform. *) 
    mutable id : int;                (* PDF object id (and name) generator. *) 
    mutable bytes : int;                     (* current output byte offset. *) 
    mutable             (* length obj id and offset of current page stream. *) 
      stream_info : int * int;   
    mutable index : (int * int) list;      (* object id, byte offset index. *)
    mutable id_resources : int;                (* current resource dict id. *) 
    mutable page_objs : int list;                      (* pages object ids. *)
    prims : (Vgr.Private.Data.primitive, (m3 * id) list) Hashtbl.t;
    alphas : (float * [`S | `F], id) Hashtbl.t;
    fonts : (Vgr.Private.Data.font, id) Hashtbl.t; 
    font_programs : (string, id) Hashtbl.t;             
    mutable gstate : gstate; }                    (* current graphic state. *) 

let partial = Vgr.Private.partial
let limit s = Vgr.Private.limit s.r 
let warn s w = Vgr.Private.warn s.r w 
let image i = Vgr.Private.I.of_data i 

let n_linear_srgb = "/srgb_l" 
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

let get_alpha_id s alpha = get_id s s.alphas alpha
let rec get_font_id s font = try Hashtbl.find s.fonts font with 
| Not_found ->
    let program = match s.font (Vgr.Private.Font.of_data font) with 
    | None -> "H"
    | Some otf -> otf 
    in
    let id = try Hashtbl.find s.font_programs program with 
    | Not_found -> get_id s s.font_programs program 
    in
    Hashtbl.add s.fonts font id; id

let get_prim_id s ctm prim = 
  let patts = try Hashtbl.find s.prims prim with 
  | Not_found -> [] 
  in
  let id = new_obj_id s in
  Hashtbl.replace s.prims prim ((ctm, id) :: patts); 
  id

let uncut_bounds s =
  let userspace = M3.mul s.inv_view_tr s.gstate.g_tr in    (* remove view tr *)
  Vgr.Private.Data.of_path 
    (P.empty >> P.rect (Box2.tr (M3.inv userspace) s.view))

let b_c3 b c = Printf.bprintf b "%f %f %f" (Color.r c) (Color.g c) (Color.b c)
let b_v2 b v = Printf.bprintf b "%f %f" (V2.x v) (V2.y v)
let b_m3 b m = 
  Printf.bprintf b "%f %f %f %f %f %f" 
    (M3.e00 m) (M3.e10 m) (M3.e01 m) (M3.e11 m) (M3.e02 m) (M3.e12 m)

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
let b_str_text buf str =     (* adds UTF-8 [str] as an UTF-16BE text string. *) 
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
    let malformed () = add_utf16be buf u_rep in
    let uchar u = add_utf16be buf u in 
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
  add_utf16be buf u_bom; (* add BOM *) 
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

let save_gstate s = 
  b_fmt s "\nq"; Set { s.gstate with g_tr = s.gstate.g_tr (* copy *) } 

let set_gstate s g = b_fmt s "\nQ"; s.gstate <- g

let b_miter_limit s o = b_fmt s "\n%f M" (Vgr.Private.P.miter_limit o)
let b_dashes s = function
| None -> b_fmt s "\n[] 0 d"
| Some (off, pat) ->
    let rec b_pat b = function 
    | [] -> () | p :: ps -> Printf.bprintf b " %f" p; b_pat b ps 
    in
    b_fmt s "\n[%a] %f d" b_pat pat off

let rec set_color s ctm old fill c = match c with 
| Const c -> 
    let set_cs = match old with Const _ -> false | _ -> true in
    let cs_op, c_op, a_op = if fill then "cs", "sc", `F else "CS", "SC", `S in
    let alpha_id = get_alpha_id s (Color.a c, a_op) in 
    if set_cs then b_fmt s "\n%s %s" n_linear_srgb cs_op; 
    b_fmt s "\n/gs%d gs" alpha_id;
    b_fmt s "\n%f %f %f %s" (Color.r c) (Color.g c) (Color.b c) c_op
| Axial (stops, _, _) | Radial (stops, _, _, _) as i -> 
    if stops = [] then set_color s ctm old fill (Const Color.void) else
    let cs_op, op = if fill then "cs", "scn" else "CS", "SCN" in
    let id = get_prim_id s ctm i in
    b_fmt s "\n/Pattern %s" cs_op;
    b_fmt s "\n/sh%d %s" id op
| Raster _ -> assert false
    
let set_fcolor s ctm c =
  let old = s.gstate.g_fcolor in
  if  old <> c then (s.gstate.g_fcolor <- c; set_color s ctm old true c)
    
let set_scolor s ctm c = 
  let old = s.gstate.g_scolor in
  if old <> c then (s.gstate.g_scolor <- c; set_color s ctm old false c)
    
let set_outline s ot = 
  let open P in
  let cap_c = function `Butt -> '0' | `Round -> '1' | `Square -> '2' in
  let join_c = function `Miter -> '0' | `Round -> '1' | `Bevel -> '2' in
  let c = s.gstate.g_outline in
  if c.width <> ot.width then b_fmt s "\n%f w" ot.width; 
  if c.cap <> ot.cap then b_fmt s "\n%c J" (cap_c ot.cap); 
  if c.join <> ot.join then b_fmt s "\n%c j" (join_c ot.join); 
  if c.dashes <> ot.dashes then b_dashes s ot.dashes; 
  if c.miter_angle <> ot.miter_angle then b_miter_limit s ot; 
  s.gstate.g_outline <- ot

let set_font s font = 
  let c = s.gstate in
  if c.g_font <> font && font != dumb_font then begin
    let id = get_font_id s font in 
    b_fmt s "\n/f%d %f Tf" id font.size;
    c.g_font <- font
  end
    
let push_transform s tr =
  let m = Vgr.Private.Data.tr_to_m3 tr in
  b_fmt s "\n%a cm" b_m3 m;
  s.gstate.g_tr <- M3.mul s.gstate.g_tr m

let one_div_3 = 1. /. 3. 
let two_div_3 = 2. /. 3. 
let cubic_earc tol cubic acc p0 large cw a r p1 =
  match Vgr.Private.P.earc_params p0 large cw a r p1 with
  | None -> (* line with a cubic *)
      let c0 = V2.add (V2.smul two_div_3 p0) (V2.smul one_div_3 p1) in
      let c1 = V2.add (V2.smul one_div_3 p0) (V2.smul two_div_3 p1) in
      cubic c0 c1 p1 acc
  | Some (c, m, t0, t1) -> 
      let mt = M2.v (* TODO something better *)
          (-. (M2.e00 m)) (M2.e10 m) (* gives the tngt to a point *)
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
        
let w_path s p op k r =
  let b_cubic c0 c1 pt () = b_fmt s "\n%a %a %a c" b_v2 c0 b_v2 c1 b_v2 pt in
  let rec b_seg last = function 
  | `Sub pt -> 
      b_fmt s "\n%a m" b_v2 pt; pt
  | `Line pt -> 
      b_fmt s "\n%a l" b_v2 pt; pt 
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
  ignore (List.fold_left b_seg P2.o (List.rev p));
  b_str s op;
  w_buf s k r

let tr_primitive ctm i = 
  let rec loop ctm = function
  | Primitive (Raster _) | Blend _ | Cut _ | Cut_glyphs _ -> None
  | Primitive p -> Some (ctm, p) 
  | Tr (tr, i) -> loop (M3.mul ctm (Vgr.Private.Data.tr_to_m3 tr)) i
  in
  loop ctm i

let w_clip s a p i k r = 
  s.todo <- (Draw i) :: save_gstate s :: s.todo;
  let op = match a with 
  | `Anz -> " W n" | `Aeo -> " W* n"
  | `O _ -> warn s (`Unsupported_cut (a, image i)); " W n"
  in
  w_path s p op k r 
    
let w_primitive_cut s ctm a p prim k r = match a with
| `Anz -> set_fcolor s ctm prim; w_path s p " f"  k r
| `Aeo -> set_fcolor s ctm prim; w_path s p " f*" k r
| `O o -> set_scolor s ctm prim; set_outline s o; w_path s p " S" k r

let w_glyph_run s run op k r = 
  set_font s run.Vgr.Private.Data.font; 
  b_fmt s "%s" op;
  begin match run.Vgr.Private.Data.text with 
  | None -> b_fmt s "\nBT"
  | Some t -> b_fmt s "\nBT /Span << /ActualText (%a)>> BDC\n" b_str_text t
  end; 
  begin match run.Vgr.Private.Data.advances with 
  | None | Some _ (* TODO *) -> 
      b_fmt s "(";
      List.iter (fun g -> b_fmt s "%c" (Char.chr g)) 
        run.Vgr.Private.Data.glyphs; 
      b_fmt s ") Tj" 
  end;
  begin match run.text with 
  | None -> b_fmt s "\nET"
  | Some _ -> b_fmt s "\nEMC ET"
  end;
  w_buf s k r

let w_clip_glyph_cut s a run i k r = 
  s.todo <- (Draw i) :: save_gstate s :: s.todo; 
  let op = match a with 
  | `O _ -> warn s (`Unsupported_cut (a, image i)); " 7 Tr" | _ -> " 7 Tr"
  in
  w_glyph_run s run op k r 
  
let w_primitive_glyph_cut s ctm a run prim k r = match a with 
| `O o -> 
    set_scolor s ctm prim; set_outline s o; w_glyph_run s run " 1 Tr" k r 
| `Anz | `Aeo -> 
    set_fcolor s ctm prim; w_glyph_run s run " 0 Tr" k r 
    
let w_cut_glyphs s a run i k r = match i with 
| Primitive (Raster _) -> warn s (`Other ("TODO raster unimplemented")); k r
| Primitive prim -> w_primitive_glyph_cut s s.gstate.g_tr a run prim k r 
| Blend _ | Cut _ | Cut_glyphs _ as i -> w_clip_glyph_cut s a run i k r
| Tr (tr, tr_i) as i ->
    begin match tr_primitive s.gstate.g_tr i with 
    | None -> w_clip_glyph_cut s a run i k r
    | Some (tr, prim) -> w_primitive_glyph_cut s tr a run prim k r 
    end

let rec w_cut s a p i k r = match i with 
| Primitive (Raster _ ) -> warn s (`Other ("TODO raster unimplemented")); k r 
| Primitive prim -> w_primitive_cut s s.gstate.g_tr a p prim k r
| Blend _ | Cut _ | Cut_glyphs _ as i -> w_clip s a p i k r
| Tr (tr, _) as i ->
    begin match tr_primitive s.gstate.g_tr i with
    | None -> w_clip s a p i k r 
    | Some (tr, prim) -> w_primitive_cut s tr a p prim k r
    end

let rec w_image s k r = 
  if s.cost > limit s then (s.cost <- 0; partial (w_image s k) r) else 
  match s.todo with 
  | [] ->  
      let len_id, stream_start = s.stream_info in
      let len = byte_offset s - stream_start in
      b_str s "\nendstream\n"; 
      b_obj_end s;  
      b_length_obj s len_id len;
      w_buf s k r
  | Set gstate :: todo ->
      s.todo <- todo;
      set_gstate s gstate; 
      w_image s k r
  | (Draw i) :: todo -> 
      s.cost <- s.cost + 1; 
      match i with 
      | Primitive _ as i -> (* Uncut primitive just cut to view. *)
          let p = uncut_bounds s in 
          s.todo <- todo;
          w_cut s `Anz p i (w_image s k) r
      | Cut (a, p, i) ->
          s.todo <- todo;
          w_cut s a p i (w_image s k) r
      | Cut_glyphs (a, run, i) -> 
          s.todo <- todo; 
          w_cut_glyphs s a run i (w_image s k) r
      | Blend (_, _, i, i') -> 
          s.todo <- (Draw i') :: (Draw i) :: todo; 
          w_image s k r
      | Tr (tr, i) -> 
          s.todo <- (Draw i) :: save_gstate s :: todo;
          push_transform s tr;
          w_image s k r
  
let w_page size s k r =
  let id = new_page s in 
  let contents_id = new_obj_id s in
  let contents_len_id = new_obj_id s in
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
  b_obj_start s contents_id;
  b_fmt s "<< /Length %d 0 R >>\nstream\n" contents_len_id;
  s.stream_info <- (contents_len_id, byte_offset s);
  b_fmt s "%s CS" n_linear_srgb; 
  b_fmt s "\n%s cs" n_linear_srgb; 
  (* Init. PDF stroke color is opaque black, so is init_gstate.g_scolor *)
  (* Init. PDF fill color is opaque black, so is init_gstate.g_fcolor *) 
  (* Init. PDF line width is 1.0, so is init_gstate.g_outline.width *)
  (* Init. PDF cap is butt, so is init_gstate.g_outline.cap *)
  (* Init. PDF join is miter, so is init_gstate.g_outline.join *) 
  (* Init. PDF dashes is a solid line, so is init_gstate.g_outline.dashes *)
  b_miter_limit s s.gstate.g_outline;
  let sx = (Size2.w size *. mm_to_pt) /. Box2.w s.view in 
  let sy = (Size2.h size *. mm_to_pt) /. Box2.h s.view in 
  let dx = -. sx *. Box2.ox s.view in 
  let dy = -. sy *. Box2.oy s.view in
  let m = M3.v sx 0. dx
               0. sy dy
               0. 0. 1. 
  in
  s.inv_view_tr <- (M3.inv m);
  push_transform s (Matrix m);
  w_image s k r

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

let rec w_alphas s k r = 
  let b_alpha (a, op) id = 
    let op = match op with `S -> "CA" | `F -> "ca" in
    b_obj_start s id;
    b_fmt s "<< /Type /ExtGState /%s %f >>\n" op a;
    b_obj_end s;
  in
  Hashtbl.iter b_alpha s.alphas;
  Hashtbl.clear s.alphas; 
  w_buf s k r

let b_pattern s shade_id (m, id) = 
  b_obj_start s id; 
  b_fmt s "<<\n\
           /PatternType 2\n\
           /Shading %d 0 R\n\
           /Matrix [%a]\n\
           >>\n" shade_id b_m3 m;
  b_obj_end s

let b_shadefun b stops = 
  let bounds = 
    let add_start = function 
    | (0., _) :: _ as stops -> stops
    | (t, c) :: _ as stops -> (0., c) :: stops
    | [] -> assert false
    in
    let rec add_end acc = function 
    | (1., _) as stop :: [] -> List.rev (stop :: acc)
    | (t, c) as stop :: [] -> List.rev ((1., c) :: stop :: acc)
    | stop :: stops -> add_end (stop :: acc) stops 
    | [] -> assert false
    in
    add_start (add_end [] stops)
  in
  let rec b_funs b = function 
  | (_, c0) :: ((_, c1) :: _ as next) -> 
      Printf.bprintf b 
        "<< /FunctionType 2 /N 1 /Domain [0 1] /C0 [%a] /C1 [%a] >> "
        b_c3 c0 b_c3 c1;
      b_funs b next 
  | _ -> () 
  in
  let rec b_bounds b = function 
  | (_, _) :: [] -> () 
  | (t, _) :: next -> Printf.bprintf b "%f " t; b_bounds b next
  | _ -> assert false
  in
  let rec b_encode b = function 
  | (_, c0) :: ((_, c1) :: _ as next) -> 
      Printf.bprintf b "0 1 "; b_encode b next 
  | _ -> () 
  in
  Printf.bprintf b "<<\n\
                    /FunctionType 3\n\
                    /Domain [0 1]\n\
                    /Functions [%a]\n\
                    /Bounds [%a]\n\
                    /Encode [%a]\n\
                    >>"
    b_funs bounds b_bounds (List.tl bounds) b_encode bounds

let b_shade s shade_id prim = 
  b_obj_start s shade_id; 
  begin match prim with 
  | Axial (stops, p0, p1) ->
      b_fmt s "<<\n\
               /ColorSpace %d 0 R\n\
               /ShadingType 2\n\
               /Function %a\n\
               /Coords [%a %a]\n\
               /Extend [true true]\n\
               >>\n"
        id_linear_srgb b_shadefun stops b_v2 p0 b_v2 p1
  | Radial (stops, f, c, r) -> 
      b_fmt s "<<\n\
               /ColorSpace %d 0 R\n\
               /ShadingType 3\n\
               /Function %a\n\
               /Coords [%a 0 %a %f]
               /Extend [true true]\n\
               >>\n"
        id_linear_srgb b_shadefun stops b_v2 f b_v2 c r
  | _ -> assert false
  end;
  b_obj_end s

let rec w_prims s k r = 
  let prims = Hashtbl.fold (fun p shs acc -> (p, shs) :: acc) s.prims [] in
  let rec loop s prims k r = match prims with
  | [] -> Hashtbl.clear s.prims; k r
  | (prim, patts) :: prims -> 
      let shade_id = new_obj_id s in
      b_shade s shade_id prim;
      List.iter (b_pattern s shade_id) patts;
      w_buf s (loop s prims k) r
  in
  loop s prims k r
  
let b_refs op b refs = 
  let ref id = Printf.bprintf b "/%s%d %d 0 R " op id id in 
  List.iter ref refs

let w_resources ?(last = false) s k r =
  let alpha_ids = Hashtbl.fold (fun _ id acc -> id :: acc) s.alphas [] in
  let font_ids = Hashtbl.fold (fun _ id acc -> id :: acc) s.fonts [] in
  let ids _ shs acc = List.fold_left (fun acc (_, id) -> id :: acc) acc shs in
  let prim_ids = Hashtbl.fold ids s.prims [] in
  b_obj_start s s.id_resources;
  b_fmt s "<<\n\
           /ColorSpace << %s %d 0 R >>\n\
           /ExtGState << %a>>\n\
           /Pattern << %a>>\n\
           /Font << %a>>\n\
           >>\n" 
    n_linear_srgb id_linear_srgb (b_refs "gs") alpha_ids (b_refs "sh") 
    prim_ids (b_refs "f") font_ids;
  b_obj_end s;
  if not last then s.id_resources <- new_obj_id s;
  w_alphas s (w_prims s k) r


let w_font s (id, fp) k r = 
  b_obj_start s id; 
  b_fmt s "<<\n\
           /Type /Font\n\
           /Subtype /Type1\n\
           /BaseFont /Helvetica\n\
           /Encoding /WinAnsiEncoding\n\
           >>\n";
  b_obj_end s;
  w_buf s k r

let w_fonts s k r = 
  let rec loop s fs k r = match fs with
  | f :: fs -> w_font s f (loop s fs k) r | [] -> k r
  in
  let fs = Hashtbl.fold (fun fp id acc -> (id, fp) :: acc) s.font_programs [] in
  loop s fs k r

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
  w_fonts s @@
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
    s.gstate <- { init_gstate with g_tr = init_gstate.g_tr }; (* copy *) 
    begin match List.length s.page_objs with 
    | 0 -> b_start s; w_page size s k r
    | n when n mod s.share = 0 -> w_resources s (w_page size s k) r
    | n -> w_page size s k r
    end
    
let target ?(font = fun _ -> None) ?(share = max_int) ?xmp () = 
  let target r _ = 
    true, render { r; 
                   font; 
                   share;
                   xmp;
                   buf = Buffer.create (max_buf + 8);
                   cost = 0; 
                   view = Box2.empty; 
                   inv_view_tr = M3.id;
                   todo = []; 
                   id = id_first_resources; 
                   stream_info = (0, 0);
                   bytes = 0; 
                   index = []; 
                   id_resources = id_first_resources;
                   page_objs = []; 
                   prims = Hashtbl.create 255; 
                   alphas = Hashtbl.create 255;
                   fonts = Hashtbl.create 255; 
                   font_programs = Hashtbl.create 255;
                   gstate = init_gstate; }
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
