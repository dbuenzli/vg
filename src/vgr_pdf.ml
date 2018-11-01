(*---------------------------------------------------------------------------
   Copyright (c) 2013 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Gg
open Vg
open Vgr.Private.Data

(* Unsafe string byte manipulations. If you don't believe the author's
   invariants, replacing with safe versions makes everything safe in
   the module. He won't be upset. *)

let unsafe_chr = Char.unsafe_chr

(* Constants and convenience functions. *)

external ( @@ ) : ('a -> 'b) -> 'a -> 'b = "%apply"

let u_lpar   = 0x0028 (* U+0028 *)
let u_rpar   = 0x0029 (* U+0029 *)
let u_bslash = 0x005C (* U+005C *)

let n_linear_srgb = "/srgb_l"
let max_buf = 65528
let mm_to_pt = 72. /. 25.4
let em_to_userspace = 1000

(* Fonts *)

type otf_font =
  { otf_program : string;
    otf_flavour : [ `CFF | `TTF ];
    otf_postscript_name : string option;
    otf_units_per_em : int;
    otf_xmin : int; otf_ymin : int; otf_xmax : int; otf_ymax : int;
    otf_ascent : int;
    otf_descent : int;
    otf_widths : int list; }

type font_program = [ `Pdf_font of string (* name *) | `Otf of otf_font ]
type font_mode = [ `Otf_glyphs | `Pdf_glyphs | `Pdf_text ]
type font =
  [ `Otf of otf_font | `Serif | `Sans | `Fixed | `Helvetica | `Times
  | `Courier ]

let pdf_helvetica_programs =
  ("Helvetica", "Helvetica-Bold"), ("Helvetica-Oblique","Helvetica-BoldOblique")

let pdf_times_programs =
  ("Times-Roman", "Times-Bold"), ("Times-Italic", "Times-BoldItalic")

let pdf_courier_programs =
  ("Courier", "Courier-Bold"), ("Courier-Oblique", "Courier-BoldOblique")

let pdf_font_program (normal, slant) font =
  let (regular, bold) = if font.Font.slant = `Normal then normal else slant in
  `Pdf_font (if font.Font.weight < `W700 then regular else bold)

let otf_font s =
  let open Result in
  let ( >>= ) x f = match x with Error e -> Error e | Ok v -> f v in
  let add_adv acc _ adv _ = adv :: acc in
  let d = Otfm.decoder (`String s) in
  Otfm.flavour         d >>= fun otf_flavour ->
  Otfm.postscript_name d >>= fun otf_postscript_name ->
  Otfm.head            d >>= fun head ->
  Otfm.hhea            d >>= fun hhea ->
  Otfm.hmtx d add_adv [] >>= fun widths ->
  let otf_units_per_em = head.Otfm.head_units_per_em in
  let otf_xmin = head.Otfm.head_xmin in
  let otf_ymin = head.Otfm.head_ymin in
  let otf_xmax = head.Otfm.head_xmax in
  let otf_ymax = head.Otfm.head_ymax in
  let otf_ascent = hhea.Otfm.hhea_ascender in
  let otf_descent = hhea.Otfm.hhea_descender in
  let otf_widths = List.rev widths in
  Ok (`Otf { otf_program = s; otf_flavour; otf_postscript_name;
             otf_units_per_em; otf_xmin; otf_ymin; otf_xmax; otf_ymax;
             otf_ascent; otf_descent; otf_widths; })

let font font = match font.Font.name with
| "Helvetica" -> `Sans
| "Times" -> `Serif
| "Courier" -> `Fixed
| _ -> `Sans

(* Render state *)

type id = int                                             (* PDF object ids. *)
type gstate =                  (* Subset of the graphics state saved by a q. *)
  { mutable g_tr : M3.t;           (* current transform with view transform. *)
    mutable g_scolor : Vgr.Private.Data.primitive;  (* current stroke color. *)
    mutable g_fcolor : Vgr.Private.Data.primitive;    (* current fill color. *)
    mutable g_outline : P.outline;       (* current path outline parameters. *)
    mutable g_font_id : id; }                            (* current font id. *)

let init_gstate =
  { g_tr = M3.id; g_scolor = Const Color.black; g_fcolor = Const Color.black;
    g_outline = P.o; g_font_id = -1; }

type cmd = Set of gstate | Draw of Vgr.Private.Data.image

type state =
  { r : Vgr.Private.renderer;                    (* corresponding renderer. *)
    font : Vg.font -> font;                               (* font resolver. *)
    xmp : string option;                            (* XMP metadata packet. *)
    buf : Buffer.t;                                   (* formatting buffer. *)
    mutable cost : int;                          (* cost counter for limit. *)
    mutable view : Gg.box2;           (* current renderable view rectangle. *)
    mutable inv_view_tr : M3.t;      (* view to mediabox inverse transform. *)
    mutable todo : cmd list;                        (* commands to perform. *)
    mutable id : int;                (* PDF object id (and name) generator. *)
    mutable bytes : int;                     (* current output byte offset. *)
    mutable          (* length object id and offset of current page stream. *)
      stream_info : int * int;
    mutable index : (int * int) list;      (* object id, byte offset index. *)
    mutable page_objs : int list;                      (* pages object ids. *)
    prims : (Vgr.Private.Data.primitive, (m3 * id) list) Hashtbl.t;
    alphas : (float * [`S | `F], id) Hashtbl.t;
    fonts : (Vg.font, id * font_mode) Hashtbl.t;
    font_programs : (font_program, id) Hashtbl.t;
    mutable gstate : gstate; }                    (* current graphic state. *)

let save_gstate s = Set { s.gstate with g_tr = s.gstate.g_tr (* copy *) }
let set_gstate s g = s.gstate <- g

let image i = Vgr.Private.I.of_data i
let partial = Vgr.Private.partial
let limit s = Vgr.Private.limit s.r
let warn s w = Vgr.Private.warn s.r w
let flush s k r =
  let clear k r = Buffer.clear s.buf; k r in
  let len = Buffer.length s.buf in
  s.bytes <- s.bytes + len;
  Vgr.Private.writebuf s.buf 0 len (clear k) r

let w_buf s k r = if Buffer.length s.buf >= max_buf then flush s k r else k r

let byte_offset s = s.bytes + Buffer.length s.buf
let id_page_root = 1
let id_linear_srgb = 2
let id_resources = 3
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
    let program, mode = match s.font font with
    | `Otf _ as p -> p, `Otf_glyphs
    | `Helvetica  -> pdf_font_program pdf_helvetica_programs font, `Pdf_glyphs
    | `Sans       -> pdf_font_program pdf_helvetica_programs font, `Pdf_text
    | `Times      -> pdf_font_program pdf_times_programs font, `Pdf_glyphs
    | `Serif      -> pdf_font_program pdf_times_programs font, `Pdf_text
    | `Courier    -> pdf_font_program pdf_courier_programs font, `Pdf_glyphs
    | `Fixed      -> pdf_font_program pdf_courier_programs font, `Pdf_text
    in
    let id = try Hashtbl.find s.font_programs program with
    | Not_found -> get_id s s.font_programs program
    in
    let resolution = id, mode in
    Hashtbl.add s.fonts font resolution; resolution

let get_prim_id s ctm prim =
  let patts = try Hashtbl.find s.prims prim with | Not_found -> [] in
  let id = new_obj_id s in
  Hashtbl.replace s.prims prim ((ctm, id) :: patts);
  id

let view_rect s =           (* image view rect in current coordinate system. *)
  let userspace = M3.mul s.inv_view_tr s.gstate.g_tr in    (* remove view tr *)
  Vgr.Private.Data.of_path
    (P.empty |> P.rect (Box2.tr (M3.inv userspace) s.view))

let b_c3 b c = Printf.bprintf b "%f %f %f" (Color.r c) (Color.g c) (Color.b c)
let b_v2 b v = Printf.bprintf b "%f %f" (V2.x v) (V2.y v)
let b_m3 b m =
  M3.(Printf.bprintf b "%f %f %f %f %f %f"
        (e00 m) (e10 m) (e01 m) (e11 m) (e02 m) (e12 m))

let b_utf16_be b u =
  (* Can't use Uutf.Buffer.add_utf16_be because of the PDF escape rules. *)
  let u = Uchar.to_int u in
  let w byte = Buffer.add_char b (unsafe_chr byte) in
  if u < 0x10000 then begin
    if u = u_lpar || u = u_rpar || u = u_bslash           (* PDF escape. *)
    then (w 0x00; w u_bslash (* escape next *byte* *) ; w u)
    else (w (u lsr 8); w (u land 0xFF))
  end else begin
    let u' = u - 0x10000 in
    let hi = (0xD800 lor (u' lsr 10)) in
    let lo = (0xDC00 lor (u' land 0x3FF)) in
    w (hi lsr 8); w (hi land 0xFF);
    w (lo lsr 8); w (lo land 0xFF)
  end

let b_str_text b str =      (* adds UTF-8 [str] as an UTF-16BE text string. *)
  let rec add_utf16_be () i = function
  | `Malformed _ -> add_utf16_be () i (`Uchar Uutf.u_rep)
  | `Uchar u -> b_utf16_be b u
  in
  Buffer.add_string b "\xFE\xFF"; (* BOM *)
  Uutf.String.fold_utf_8 add_utf16_be () str

let b_fmt s fmt = Printf.bprintf s.buf fmt
let b_str s str = Buffer.add_string s.buf str
let b_str_byte s byte =        (* escapes bytes that need to in PDF strings. *)
  let b s byte = Buffer.add_char s.buf (unsafe_chr byte) in
  if byte = u_lpar || byte = u_rpar || byte = u_bslash
  then (b s u_bslash; b s byte)
  else (b s byte)

let b_start s = b_str s "%PDF-1.7\n%\xCF\xC3\xE1\xED\xEC\n"
let b_obj_end s = b_str s "endobj\n"
let b_obj_start s id =
  s.index <- (id, byte_offset s) :: s.index;
  b_fmt s "%d 0 obj\n" id

let b_length_obj s id len =
  b_obj_start s id;
  b_fmt s "%i\n" len;
  b_obj_end s

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
  let id, font_mode = get_font_id s font in
  if c.g_font_id <> id then begin
    b_fmt s "\n/f%d %f Tf" id font.Font.size;
    c.g_font_id <- id;
  end;
  font_mode

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
      let mt = M2.v
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
  let b_seg last = function
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
      cubic_earc 1e-6 b_cubic () last large cw angle radii pt; pt
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
  let op = match a with
  | `Anz -> " W n" | `Aeo -> " W* n"
  | `O _ -> warn s (`Unsupported_cut (a, image i)); " W n"
  in
  s.todo <- (Draw i) :: save_gstate s :: s.todo;
  b_fmt s "\nq";
  w_path s p op k r

let w_primitive_cut s ctm a p prim k r = match a with
| `Anz -> set_fcolor s ctm prim; w_path s p " f"  k r
| `Aeo -> set_fcolor s ctm prim; w_path s p " f*" k r
| `O o -> set_scolor s ctm prim; set_outline s o; w_path s p " S" k r

let rec w_cut s a p i k r = match i with
| Primitive (Raster _ ) -> assert false
| Primitive prim -> w_primitive_cut s s.gstate.g_tr a p prim k r
| Blend _ | Cut _ | Cut_glyphs _ as i -> w_clip s a p i k r
| Tr (tr, _) as i ->
    begin match tr_primitive s.gstate.g_tr i with
    | None -> w_clip s a p i k r
    | Some (tr, prim) -> w_primitive_cut s tr a p prim k r
    end

(* The non straightforward mappings from unicode to Windows Code Page 1252.
   http://www.unicode.org/Public/MAPPINGS/VENDORS/MICSFT/WINDOWS/CP1252.TXT *)
let uchar_to_cp1252 =
  [ 0x20AC, 0x80; 0x201A, 0x82; 0x0192, 0x83; 0x201E, 0x84; 0x2026, 0x85;
    0x2020, 0x86; 0x2021, 0x87; 0x02C6, 0x88; 0x2030, 0x89; 0x0160, 0x8A;
    0x2039, 0x8B; 0x0152, 0x8C; 0x017D, 0x8E; 0x2018, 0x91; 0x2019, 0x92;
    0x201C, 0x93; 0x201D, 0x94; 0x2022, 0x95; 0x2013, 0x96; 0x2014, 0x97;
    0x02DC, 0x98; 0x2122, 0x99; 0x0161, 0x9A; 0x203A, 0x9B; 0x0153, 0x9C;
    0x017E, 0x9E; 0x0178, 0x9F; ]

let glyph_pdf_encode s glyph =           (* glyph is an Unicode scalar value *)
  let glyph = match glyph with        (* translate to Windows Code Page 1252 *)
  | g when g <= 0x007F -> g
  | g when g <= 0x00A0 -> 0
  | g when g <= 0x00FF -> g
  | g -> try List.assoc g uchar_to_cp1252 with Not_found -> 0
  in
  b_str_byte s glyph

let glyph_identity_h_encode s glyph =
  b_str_byte s ((glyph land 0xFF00) lsr 8);
  b_str_byte s ((glyph land 0x00FF))

type block =
  { bk_text : string; bk_glyphs : glyph list; bk_advances : v2 list }

let rec b_glyphs_advs s encode gs advs = match gs, advs with
| g :: gs, a :: advs ->
    b_fmt s "("; (encode s g); b_fmt s ") Tj %f %f Td " (V2.x a) (V2.y a);
    b_glyphs_advs s encode gs advs
| g :: _ as gs, [] -> (* without advances *)
    b_fmt s "("; List.iter (encode s) gs; b_fmt s ") Tj"
| [], _ -> ()

let b_block s encode block =
  let rec b_glyph_block s encode gs advs = match gs, advs with
  | g :: gs, a :: advs ->
      b_fmt s "("; (encode s g); b_fmt s ") Tj %f %f Td " (V2.x a) (V2.y a);
      b_glyph_block s encode gs advs
  | g :: _ as gs, [] -> (* without advances *)
      b_fmt s "("; List.iter (encode s) gs; b_fmt s ") Tj"
  | [], _ -> ()
  in
  b_fmt s "\n/Span << /ActualText (%s)>> BDC\n" block.bk_text;
  b_glyph_block s encode block.bk_glyphs block.bk_advances;
  b_fmt s "\nEMC"

let make_blocks run =
  let b = Buffer.create 255 in
  let uchars = function
  | None -> []
  | Some s ->
      let add_uchar acc _ = function
      | `Malformed _ -> Uutf.u_rep :: acc | `Uchar u -> u :: acc
      in
      List.rev (Uutf.String.fold_utf_8 add_uchar [] s)
  in
  let rec blocks_one_to_one acc us gs = match us, gs with
  | u :: _ as us , g :: [] -> List.rev (((List.length us), 1) :: acc)
  | [], g :: gs -> blocks_one_to_one ((0, 1) :: acc) [] gs
  | u :: us, g :: gs -> blocks_one_to_one ((1,1) :: acc) us gs
  | _, [] -> List.rev acc
  in
  let text_n n us =
    let rec add n us =
      if n <= 0 then Buffer.contents b, us else match us with
      | u :: us -> b_utf16_be b u; add (n - 1) us
      | [] -> add 0 us
    in
    Buffer.clear b; Buffer.add_string b "\xFE\xFF"; (* BOM *) add n us
  in
  let rec glyphs_n n glyphs advances gs advs =
    if n <= 0 then glyphs, advances, gs, advs else match gs, advs with
    | g :: gs, adv :: advs ->
        glyphs_n (n - 1) (g :: glyphs) (adv :: advances) gs advs
    | g :: gs, [] ->
        glyphs_n (n - 1) (g :: glyphs) [] gs []
    | [], _ -> glyphs, advances, gs, advs
  in
  let rec loop rev acc us gs advs = function
  | (cc, gc) :: blocks ->
      let bk_text, us = text_n cc us in
      let bk_glyphs, bk_advances, gs, advs = glyphs_n gc [] [] gs advs in
      let bk_glyphs = if rev then List.rev bk_glyphs else bk_glyphs in
      loop rev ({ bk_text; bk_glyphs; bk_advances } :: acc) us gs advs blocks
  | [] -> if rev then acc else List.rev acc
  in
  let rev, blocks = run.blocks in
  let us = uchars run.text in
  let gs = if rev then List.rev run.glyphs else run.glyphs in
  let bs = if blocks = [] then blocks_one_to_one [] us gs else blocks in
  loop rev [] us gs run.advances bs

let pdf_text_glyphs run = match run.Vgr.Private.Data.text with
| None -> []
| Some text ->
    let add_glyph acc _ dec =
      let u = match dec with
      | `Uchar u -> Uchar.to_int u | `Malformed _ -> Uchar.to_int Uutf.u_rep
      in
      u :: acc
    in
    List.rev (Uutf.String.fold_utf_8 add_glyph [] text)

let w_glyph_run s run op k r =
  let font_mode = set_font s run.Vgr.Private.Data.font in
  b_fmt s "%s BT" op;
  let encode = match font_mode with
  | `Otf_glyphs -> glyph_identity_h_encode
  | `Pdf_glyphs | `Pdf_text -> glyph_pdf_encode
  in
  let run = match font_mode with
  | `Otf_glyphs | `Pdf_glyphs -> run
  | `Pdf_text -> { run with Vgr.Private.Data.glyphs = pdf_text_glyphs run }
  in
  let blocks = make_blocks run in
  List.iter (b_block s encode) blocks;
  b_fmt s "\nET";
  w_buf s k r

let w_clip_glyph_cut s a run i k r =
  let op = match a with
  | `O _ -> warn s (`Unsupported_cut (a, image i)); " 7 Tr" | _ -> " 7 Tr"
  in
  s.todo <- (Draw i) :: save_gstate s :: s.todo;
  b_fmt s "\nq";
  w_glyph_run s run op k r

let w_primitive_glyph_cut s ctm a run prim k r = match a with
| `O o ->
    set_scolor s ctm prim; set_outline s o; w_glyph_run s run " 1 Tr" k r
| `Anz | `Aeo ->
    set_fcolor s ctm prim; w_glyph_run s run " 0 Tr" k r

let w_cut_glyphs s a run i k r = match i with
| Primitive (Raster _) -> assert false
| Primitive prim -> w_primitive_glyph_cut s s.gstate.g_tr a run prim k r
| Blend _ | Cut _ | Cut_glyphs _ as i -> w_clip_glyph_cut s a run i k r
| Tr (tr, tr_i) as i ->
    begin match tr_primitive s.gstate.g_tr i with
    | None -> w_clip_glyph_cut s a run i k r
    | Some (tr, prim) -> w_primitive_glyph_cut s tr a run prim k r
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
      b_fmt s "\nQ";
      w_image s k r
  | (Draw i) :: todo ->
      s.cost <- s.cost + 1;
      match i with
      | Primitive _ as i -> (* Uncut primitive just cut to view. *)
          let p = view_rect s in
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
          b_fmt s "\nq";
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
    id_page_root id_resources
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

let b_shadefun b stops =                              (* assert(stops <> []) *)
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

let w_resources s k r =
  let b_refs op b refs =
    let ref id = Printf.bprintf b "/%s%d %d 0 R " op id id in
    List.iter ref refs
  in
  let alpha_ids = Hashtbl.fold (fun _ id acc -> id :: acc) s.alphas [] in
  let font_ids = Hashtbl.fold (fun _ (id, _) acc -> id :: acc) s.fonts [] in
  let ids _ shs acc = List.fold_left (fun acc (_, id) -> id :: acc) acc shs in
  let prim_ids = Hashtbl.fold ids s.prims [] in
  b_obj_start s id_resources;
  b_fmt s "<<\n\
           /ColorSpace << %s %d 0 R >>\n\
           /ExtGState << %a>>\n\
           /Pattern << %a>>\n\
           /Font << %a>>\n\
           >>\n"
    n_linear_srgb id_linear_srgb (b_refs "gs") alpha_ids (b_refs "sh")
    prim_ids (b_refs "f") font_ids;
  b_obj_end s;
  w_alphas s (w_prims s k) r

let w_pdf_font s id n k r =
  b_obj_start s id;
  b_fmt s "<<\n\
           /Type /Font\n\
           /Subtype /Type1\n\
           /BaseFont /%s\n\
           /Encoding /WinAnsiEncoding\n\
           >>\n" n;
  b_obj_end s;
  w_buf s k r

let w_font s (id, fp) k r = match fp with
| `Pdf_font n -> w_pdf_font s id n k r
| `Otf otf ->
    let rec b_widths u_per_em b = function
    | [] -> ()
    | w :: ws ->
        Printf.bprintf b " %d" ((w * em_to_userspace) / u_per_em);
        b_widths u_per_em b ws
    in
    let cid_id = new_obj_id s in
    let ps_name = match otf.otf_postscript_name with
    | None -> "f" ^ string_of_int cid_id           (* just make up something *)
    | Some n -> n
    in
    let subtype, name = match otf.otf_flavour with
    | `CFF -> "/CIDFontType0", ps_name ^ "-Identity-H"
    | `TTF -> "/CIDFontType2", ps_name
    in
    b_obj_start s id;
    b_fmt s "<<\n\
             /Type /Font\n\
             /Subtype /Type0\n\
             /BaseFont /%s\n\
             /Encoding /Identity-H\n\
             /DescendantFonts [%d 0 R]\n\
             >>\n" name cid_id;
    b_obj_end s;
    let fd_id = new_obj_id s in
    b_obj_start s cid_id;
    b_fmt s "<<\n\
             /Type /Font\n\
             /Subtype %s\n\
             /BaseFont /%s\n\
             /CIDSystemInfo\n\
             << /Registry (Adobe) /Ordering (Identity) /Supplement 0 >>\n\
             /W [ 0 [%a]]\n\
             /FontDescriptor %d 0 R\n\
             >>\n"
      subtype ps_name (b_widths otf.otf_units_per_em) otf.otf_widths fd_id;
    b_obj_end s;
    let fp_id = new_obj_id s in
    b_obj_start s fd_id;
    b_fmt s "<<\n\
             /Type /FontDescriptor\n\
             /FontName /%s\n\
             /Flags 0\n\
             /FontBBox [ %d %d %d %d ]\n\
             /ItalicAngle 0\n\
             /Ascent %d\n\
             /Descent %d\n\
             /StemV 0\n\
             /FontFile3 %d 0 R\n\
             >>\n"
      ps_name
      ((otf.otf_xmin * em_to_userspace) / otf.otf_units_per_em)
      ((otf.otf_ymin * em_to_userspace) / otf.otf_units_per_em)
      ((otf.otf_xmax * em_to_userspace) / otf.otf_units_per_em)
      ((otf.otf_ymax * em_to_userspace) / otf.otf_units_per_em)
      ((otf.otf_ascent * em_to_userspace) / otf.otf_units_per_em)
      ((otf.otf_descent * em_to_userspace) / otf.otf_units_per_em)
      fp_id;
    b_obj_end s;
    let len_id = new_obj_id s in
    b_obj_start s fp_id;
    b_fmt s "<< /Subtype /OpenType /Length %d 0 R >>\nstream\n" len_id;
    let stream_start = byte_offset s in
    b_str s otf.otf_program;
    let len = byte_offset s - stream_start in
    b_str s "\nendstream\n";
    b_obj_end s;
    b_length_obj s len_id len;
    w_buf s k r

let w_fonts s k r =
  let rec loop s fs k r = match fs with
  | f :: fs -> w_font s f (loop s fs k) r
  | [] -> Hashtbl.clear s.fonts; Hashtbl.clear s.font_programs; k r
  in
  let fs = Hashtbl.fold (fun fp id acc -> (id, fp) :: acc) s.font_programs [] in
  loop s fs k r

let w_page_root s k r =
  let rec b_id_list b = function
  | [] -> () | id :: ids -> Printf.bprintf b " %d 0 R" id; b_id_list b ids
  in
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

let w_xmp s id_meta k r = match id_meta with
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
  r |>
  w_resources s @@
  w_fonts s @@
  w_page_root s @@
  w_linear_srgb s @@
  w_info s id_info @@
  w_catalog s id_catalog id_meta @@
  w_xmp s id_meta @@
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
    | n -> w_page size s k r
    end

let target ?(font = font) ?xmp () =
  let target r _ =
    true, render { r;
                   font;
                   xmp;
                   buf = Buffer.create (max_buf + 8);
                   cost = 0;
                   view = Box2.empty;
                   inv_view_tr = M3.id;
                   todo = [];
                   id = id_resources;
                   stream_info = (0, 0);
                   bytes = 0;
                   index = [];
                   page_objs = [];
                   prims = Hashtbl.create 255;
                   alphas = Hashtbl.create 255;
                   fonts = Hashtbl.create 255;
                   font_programs = Hashtbl.create 255;
                   gstate = init_gstate; }
  in
  Vgr.Private.create_target target

(*---------------------------------------------------------------------------
   Copyright (c) 2013 Daniel C. Bünzli

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
