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

type id = int (* object ids *) 
type g_state = { g_tr : M3.t } 
type cmd = Pop of g_state | Draw of Vgr.Private.Data.image

(* At the moment there's a single resource dictionary for all the
   pages. This means that all the resources needed by the images need
   to be in memory at the same time.  It wouldn't be to hard to add a
   new meta key to specify that resources should be shared and flushed
   every n pages. *)

type state = 
  { r : Vgr.Private.renderer;                    (* corresponding renderer. *)
    share : int;                            (* page resource sharing limit. *)
    xmp : string option;                            (* XMP metadata packet. *)
    buf : Buffer.t;                                   (* formatting buffer. *) 
    mutable cost : int;                          (* cost counter for limit. *) 
    mutable view : Gg.box2;           (* current renderable view rectangle. *)
    mutable todo : cmd list;                        (* commands to perform. *) 
    mutable id : int;                    (* object id (and name) generator. *) 
    mutable bytes : int;                     (* current output byte offset. *) 
    mutable                  (* length obj id and offset of current stream. *) 
      stream_info : int * int;   
    mutable index : (int * int) list;      (* object id, byte offset index. *)
    mutable page_nodes : int list;                 (* page node object ids. *) 
    mutable page_objs : int list;                      (* pages object ids. *)
    paths : (P.t, id) Hashtbl.t;                      
    prims : (Vgr.Private.Data.primitive, id) Hashtbl.t;
    outlines : (P.outline, id) Hashtbl.t; 
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
let id_first_page_node = 3

let max_buf = 65536 
let byte_offset s = s.bytes + Buffer.length s.buf
let new_obj_id s = s.id <- s.id + 1; s.id
let obj_count s = s.id + 1
let new_page s = 
  let id = new_obj_id s in 
  (s.page_objs <- id :: s.page_objs; id)

let pop_gstate s = Pop { g_tr = s.s_tr }
let set_gstate s g = s.s_tr <- g.g_tr 
let uncut_bounds s = 
  Vgr.Private.Data.of_path (P.empty >> P.rect (Box2.tr (M3.inv s.s_tr) s.view))

let flush s k r = 
  let clear k r = Buffer.clear s.buf; k r in 
  let len = Buffer.length s.buf in
  s.bytes <- s.bytes + len; 
  Vgr.Private.writebuf s.buf 0 len (clear k) r

let w_buf s k r = 
  if Buffer.length s.buf > max_buf then flush s k r else 
  k r

let rec pr_id_list b = function 
| [] -> () | id :: ids -> Printf.bprintf b " %d 0 R" id; pr_id_list b ids

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

let rec w_image s k r = 
  if s.cost > limit s then (s.cost <- 0; partial (w_image s k) r) else 
  match s.todo with 
  | [] -> 
      let len_id, stream_start = s.stream_info in
      let len = byte_offset s - stream_start in
      b_str s "endstream\n"; 
      b_obj_end s;  
      b_obj_start s len_id;                   (* Write object stream length. *) 
      b_fmt s "%i\n" len; 
      b_obj_end s; 
      w_buf s k r
  | Pop gs :: todo ->
      set_gstate s gs; 
      s.todo <- todo; 
      (* TODO *)
      w_image s k r
  | (Draw i) :: todo -> 
      s.cost <- s.cost + 1; 
      s.todo <- todo;
      (* TODO *) 
      w_image s k r
      
let w_contents s id k r = 
  let obj_len_id = new_obj_id s in 
  b_obj_start s id;
  b_fmt s "<< /Length %d 0 R >>\nstream\n" obj_len_id;
  s.stream_info <- (obj_len_id, byte_offset s);
  b_fmt s "%s CS\n" n_linear_srgb; 
  b_fmt s "%s cs\n" n_linear_srgb; 
  w_image s k r

let w_page size s k r = 
  let id = new_page s in 
  let contents_id = new_obj_id s in
  b_obj_start s id;
  b_fmt s "<<\n\
           /Type /Page\n\
           /Parent %d 0 R\n\
           /MediaBox [0 0 %f %f]\n\
           /Contents %d 0 R\n\
           >>\n" 
    (List.hd s.page_nodes)
    (V2.x size *. mm_to_pt) (V2.y size *. mm_to_pt)
    contents_id;
  b_obj_end s;
  w_contents s contents_id k r

let w_linear_srgb s k r = 
  b_obj_start s id_linear_srgb;
  b_obj_end s;
  w_buf s k r

let w_resources s id_resources k r =
  (* let b_form p id = b_fmt s "/o%d %d 0 R\n" id id in *)
  b_obj_start s id_resources;
  b_fmt s "<<\n\
           /ColorSpace << %s %d 0 R >>\n\
           /XObject <<>>>>\n" n_linear_srgb id_linear_srgb; 
  b_obj_end s; 
  w_buf s k r

let w_page_node_and_resources ?(last = false) s  k r =
  let page_count = List.length s.page_objs in 
  let pages = List.rev s.page_objs in
  let id_resources = new_obj_id s in
  b_obj_start s (List.hd s.page_nodes); 
  b_fmt s "<<\n\
           /Type /Pages\n\
           /Resources %d 0 R\n\
           /Count %d\n\
           /Kids [%a]\n\
           >>\n" id_resources page_count pr_id_list pages;
  b_obj_end s;
  s.page_objs <- [];
  if not last then s.page_nodes <- new_obj_id s :: s.page_nodes;
  w_buf s (w_resources s id_resources k) r

let w_page_root s k r = 
  let node_count = List.length s.page_nodes in 
  let nodes = List.rev s.page_nodes in
  b_obj_start s id_page_root; 
  b_fmt s "<<\n\
           /Type /Pages\n\
           /Count %d\n\
           /Kids [%a]\n\
           >>\n" node_count pr_id_list nodes;
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
    let obj_len_id = new_obj_id s in 
    b_obj_start s id;
    b_fmt s "<< /Type /Metadata /Subtype /XML /Length %d 0 R >>\nstream\n" 
      obj_len_id;
    let stream_start = byte_offset s in 
    b_fmt s "<?xpacket begin=\"\xEF\xBB\xBF\" \
                       id=\"W5M0MpCehiHzreSzNTczkc9d\"?>\n\
             %s\n\
             <?xpacket end=\"w\"?>\n" xmp;
    let len = byte_offset s - stream_start in
    b_str s "endstream\n"; 
    b_obj_end s;  
    b_obj_start s obj_len_id;                 (* Write object stream length. *) 
    b_fmt s "%i\n" len; 
    b_obj_end s; 
    w_buf s k r

let w_info s id_info k r =       (* just /Producer, rest is handled by XMP. *) 
  b_obj_start s id_info;
  b_str s "<< /Producer (OCaml Vg library %%VERSION%%) >>\n"; 
  b_obj_end s; 
  w_buf s k r

let w_xref_table s k r =                           (* cross reference table *)
  let b_offset (_, offset) = b_fmt s "%010d 00000 n\n" offset in
  let xref_offset = byte_offset s in
  let obj_count = s.id + 1 in
  b_fmt s "xref\n\
           0 %d\n\
           0000000000 65535 f\n" obj_count; 
  List.iter b_offset (List.sort compare s.index);
  w_buf s (k xref_offset) r

let w_trailer s id_catalog id_info k xref_offset r =
  b_fmt s "trailer\n\
           <<\n\
           /Size %d\n\
           /Root %d 0 R\n\
           /Info %d 0 R\n\
           >>\n\
           startxref\n\
           %d\n\
           %%EOF" (obj_count s) id_catalog id_info xref_offset;
  w_buf s k r
   
let w_end s k r =                                  
  let id_info = new_obj_id s in
  let id_catalog = new_obj_id s in 
  let id_meta = match s.xmp with None -> None | Some _ -> Some (new_obj_id s) in
  r >> 
  w_page_node_and_resources ~last:true s @@
  w_page_root s @@
  w_info s id_info @@
  w_catalog s id_catalog id_meta @@ 
  w_xmp_metadata s id_meta @@ 
  w_xref_table s @@
  w_trailer s id_catalog id_info @@ k 

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
    | n when n mod s.share = 0 -> 
        w_page_node_and_resources s (w_page size s k) r
    | n -> w_page size s k r
    end
    
let target ?(share = max_int) ?xmp () = 
  let target r _ = 
    true, render { r; 
                   share;
                   xmp;
                   buf = Buffer.create 2048;
                   cost = 0; 
                   view = Box2.empty; 
                   todo = []; 
                   id = id_first_page_node; 
                   stream_info = (0, 0);
                   bytes = 0; 
                   index = []; 
                   page_nodes = [id_first_page_node];
                   page_objs = []; 
                   paths = Hashtbl.create 255; 
                   prims = Hashtbl.create 255; 
                   outlines = Hashtbl.create 255; 
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
