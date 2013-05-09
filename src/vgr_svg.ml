(*---------------------------------------------------------------------------
   Copyright 2013 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

open Gg
open Vg
open Vgr.Private.Data

(* Tag names. *) 

let t_svg = "svg"
let t_title = "title"
let t_descr = "descr"
let t_g = "g"
let t_defs = "defs"
let t_use = "use"
let t_path = "path"

(* Attribute names. *)

let a_xmlns = "xmlns"
let a_xmlns_xlink = "xmlns:l"
let a_xlink_href = "l:href" 
let a_id = "id" 
let a_version = "version"
let a_base_profile = "baseProfile" 
let a_width = "width"
let a_height = "height"
let a_view_box = "viewBox"
let a_preserve_aspect_ratio = "preserveAspectRatio"
let a_color_interp = "color-interpolation"
let a_color_interp_filters = "color-interpolation-filters"
let a_transform = "transform"

(* Attribute values. *)

let av_svg_ns = "http://www.w3.org/2000/svg"
let av_xlink_ns = "http://www.w3.org/1999/xlink"
let av_svg_version = "1.1"
let av_basic_profile = "basic"
let av_linearRGB = "linearRGB"
let av_none = "none"
let av_linecap = function 
| `Butt -> "butt" | `Round -> "round" | `Square -> "square"

let av_linejoin = function 
| `Miter -> "miter" | `Bevel -> "bevel" | `Round -> "round"

let av_fill_rule = function 
| `Aeo -> "evenodd" | `Anz -> "nonzero"

(* Metadata keys. *)

let xml_decl = Vgm.key "xml_decl" Format.pp_print_bool

(* Renderer *)

type gstate = 
  { g_alpha : float; 
    g_blender : I.blender; 
    g_outline : P.outline; } 
    
type svg_prim = Gradient of int | Color of string 

type cmd = Pop of unit | Draw of Vgr.Private.Data.image
type state = 
  { r : Vgr.Private.renderer; 
    buf : Buffer.t;                                   (* formatting buffer. *)
    timeout : float;   
    mutable cost : int;                        (* cost counter for timeout. *)
    mutable view : Gg.box2;                              (* view rectangle. *)
    mutable todo : cmd list;                        (* commands to perform. *)
    mutable tags : string list;                           (* tags to close. *) 
    (* Cached primitives and paths *)
    mutable id : int;
    prims : (Vgr.Private.Data.primitive, svg_prim) Hashtbl.t; 
    paths : (path, int) Hashtbl.t; 
    (* Current graphics state. *)
    mutable s_alpha : float; 
    mutable s_blender : I.blender; 
    mutable s_outline : P.outline; } 

let max_cost = max_int
let warn s w i = Vgr.Private.warn s.r w i 
let save_gstate s = Pop ()
let set_gstate s gs = () 

let new_id s = s.id <- s.id + 1; s.id

external ( & ) : ('a -> 'b) -> 'a -> 'b = "%apply"

let writeb = Vgr.Private.writeb
let writes = Vgr.Private.writes 
let writebuf = Vgr.Private.writebuf 

let w_nop k r = k r
let w_chr c k r = writeb (Char.code c) k r
let w_str str k r = writes str 0 (String.length str) k r
let w_fmt_str s k fmt = 
  let flush_buf buf = 
    Printf.eprintf "FLUSH:%d:%d:%s\n%!" 0 (Buffer.length buf) (Buffer.contents buf);
    writebuf buf 0 (Buffer.length buf) k 
  in
  Buffer.clear s.buf; Printf.kbprintf flush_buf s.buf fmt

let w_int s i k = w_fmt_str s k "%d" i
let w_mm s v k = w_fmt_str s k "%gmm" v
let w_id_att s id k = w_fmt_str s k "id=\x22i%d\x22" id
let w_id_ref s id k = w_fmt_str s k "#i%d" id

let w_view_box s v k = 
  w_fmt_str s k "%g %g %g %g" 0. 0. (Box2.w v) (Box2.h v)

let w_att a w_value k r = 
  (w_chr ' ' & w_str a  & w_str "=\x22" & w_value & w_chr '\x22' & k) r
    
let w_tag s n w_atts k r = 
  (w_chr '<' & w_str n & w_atts & w_str "/>" & k) r

let w_stag s n w_atts k r = 
  s.tags <- n :: s.tags;
  (w_chr '<' & w_str n & w_atts & w_chr '>' & k) r
    
let w_etag s k r = match s.tags with 
| [] -> assert false 
| n :: tags ->
    s.tags <- tags; 
    (w_str "</" & w_str n & w_chr '>' & k) r

let w_ssvg s size view k r = 
  w_stag s t_svg begin fun k ->
    w_att a_xmlns (w_str av_svg_ns) &
    w_att a_xmlns_xlink (w_str av_xlink_ns) &
    w_att a_version (w_str av_svg_version) & 
    w_att a_base_profile (w_str av_basic_profile) &
    w_att a_width (w_mm s (Size2.w size)) &
    w_att a_height (w_mm s (Size2.h size)) &
    w_att a_view_box (w_view_box s view) & 
    w_att a_preserve_aspect_ratio (w_str av_none) & 
    w_att a_color_interp (w_str av_linearRGB) & 
    w_att a_color_interp_filters (w_str av_linearRGB) &
    k
  end k r

let w_title s title k r = match title with 
| None -> w_nop k r
| Some t -> (w_stag s t_title w_nop & w_str t & w_etag s & k) r 

let w_descr s descr k r = match descr with 
| None -> w_nop k r
| Some d -> (w_stag s t_descr w_nop & w_str d & w_etag s & k) r 

let w_transform s tr k = match tr with
| Move v -> w_fmt_str s k "translate(%g,%g)" (V2.x v) (V2.y v)
| Rot a -> w_fmt_str s k "rotate(%g)" a
| Scale sv -> w_fmt_str s k "scale(%g,%g)" (V2.x sv) (V2.y sv)
| Matrix m -> 
    M3.(w_fmt_str s k "transform(%g,%g,%g,%g,%g,%g)"
        (e00 m) (e10 m) (e01 m) (e11 m) (e02 m) (e12 m))

let rec w_path_data s p k r = failwith "TODO"
(*
match p with 
| [] -> w_chr '\x22' k r
| seg :: p -> 
    match seg with 
    | `Sub pt -> 
        (w_fmt_str s & w_path_data s p k) "M%g %g" (V2.x pt) (V2.y pt)
    | `Line pt -> 
        (w_fmt_str s & w_path_data s p k) "L%g %g" (V2.x pt) (V2.y pt)
    | `Qcurve (c, pt) -> 
        (w_fmt_str s & w_path_data s p k)
           "Q%g %g %g %g" (V2.x c) (V2.y c) (V2.x pt) (V2.y pt)) r
    | `Ccurve (c, c', pt) ->
        (w_fmt_str s & w_path_data s p k
           "C%g %g %g %g %g %g" 
           (V2.x c) (V2.y c) (V2.x c') (V2.y c') (V2.x pt) (V2.y pt)
    | `Earc (large, cw, a, r, pt) -> 
        let large = if large then 1 else 0 in
        let sweep = if cw then 0 else 1 in
        (w_fmt_str s & w_path_data s p k
           "A %g %g %g %d %d %g %g" 
           (V2.x r) (V2.y r) a large sweep (V2.x pt) (V2.y pt)) r
    | `Close -> 
        (w_chr 'Z' & w_path_data s p k) r
  *)
let w_path s p k r = try k (Hashtbl.find s.paths p) r with
| Not_found -> 
    let id = new_id s in
    begin 
      w_stag s t_defs w_nop &
      w_tag s t_path begin fun k -> 
        w_id_att s id & 
        w_str "path=\x22" & 
        w_path_data s p &
        k
      end & 
      w_etag s &  
      w_etag s & 
      (k id)    
    end r

let rec w_cut s a p k r = match s.todo with 
| [] | Pop _ :: _ -> assert false 
| (Draw i) :: todo -> 
    match i with 
    | Primitive (Raster _) -> 
        begin match a with 
        | `O _ -> warn s (`Unsupported_cut a) (Some i); s.todo <- todo; k r
        | `Aeo | `Anz -> 
            warn s (`Other "TODO raster unimplemented") (Some i); 
            s.todo <- todo; k r
        end
    | Primitive pr -> 
        s.todo <- todo; k r
    | Meta _ -> s.todo <- todo; w_cut s a p k r
    | Tr (tr, i) -> 
        s.todo <- (Draw i) :: (save_gstate s) :: todo; 
        k r
    | i -> 
        s.todo <- todo; k r
        
let rec w_image s k r =
  if s.cost > max_cost then (s.cost <- 0; Vgr.Private.partial (w_image s k) r) 
  else match s.todo with 
  | [] -> Hashtbl.reset s.prims; Hashtbl.reset s.paths; k r
  | Pop gs :: todo -> 
      set_gstate s gs; 
      s.todo <- todo; 
      (w_etag s & w_image s k) r
  | (Draw i) :: todo -> 
      s.cost <- s.cost + 1; 
      match i with 
      | Primitive _ -> 
          (* Uncut primitive, just cut to view. Need CTM *) 
          warn s (`Other "TODO, uncut primitive not implemented") (Some i);
          s.todo <- todo; 
          w_image s k r
      | Cut (a, p, i) -> 
          s.todo <- (Draw i) :: todo;
          (w_path s p & (fun k id -> w_cut s a id k) & w_image s k) r 
      | Blend (blender, alpha, i, i') -> 
          (* TODO blender and alpha *) 
          s.todo <- (Draw i') :: (Draw i) :: todo; 
          if blender = `Over && alpha = None then w_image s k r else
          begin
            warn s (`Other "TODO, blend mode and group opacity") (Some i);
            w_image s k r
          end
      | Tr (tr, i) ->
          s.todo <- (Draw i) :: save_gstate s :: todo; 
          begin 
            w_stag s t_g (w_att a_transform (w_transform s tr)) & 
            w_image s k
          end r
      | Meta (m, i) -> 
          s.todo <- (Draw i) :: todo; 
          w_image s k r

let render s v k r = match v with 
| `End -> Vgr.Private.flush k r
| `Image (size, view, i) -> 
    let m = Vgr.Private.meta r in
    let xml_decl = match Vgm.find m xml_decl with None -> true | Some b -> b in
    let title = Vgm.find m Vgm.title in 
    let descr = Vgm.find m Vgm.subject in
    s.todo <- [ Draw i ];
    begin
      begin 
        if xml_decl 
        then w_str "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" 
        else w_nop
      end & 
      w_ssvg s size view & 
      w_title s title & 
      w_descr s descr &
      w_stag s t_g begin fun k ->
        let m = M3.v     (* map top-left corner to bottom right of viewbox. *)
            1. 0.       (-. Box2.ox view)
            0. (-. 1.)  (Box2.h view +. Box2.oy view)
            0. 0.       1. 
        in           
        w_att a_transform (w_transform s (Matrix m)) & k
        (* stroke-width is 1. initially, so is P.o *)
        (* stroke-linecap is butt initially, so is P.o *) 
        (* stroke-linejoin is miter initially, so is P.o *)
        (* stroke-dasharray is none initially, so is P.o *)
        (* stroke-miterlimit is 4 initially, TODO *)
      end &
      w_image s &
      w_etag s & 
      w_etag s &
      k 
    end r

let target ?xml_decl () = 
  let target r _ = 
    false, render { r;
                    buf = Buffer.create 241;
                    timeout = 0.005;
                    cost = 0;
                    view = Box2.empty; 
                    todo = [];
                    id = 0; 
                    prims = Hashtbl.create 241; 
                    paths = Hashtbl.create 241;
                    s_alpha = 1.; 
                    s_blender = `Over; 
                    s_outline = P.o;
                    tags = [] }
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
