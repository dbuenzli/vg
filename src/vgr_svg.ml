(*---------------------------------------------------------------------------
   Copyright (c) 2013 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Gg
open Vg
open Vgr.Private.Data

type path_id = int
type clip_id = int
type svg_font = string
type svg_prim = Gradient of int | Color of string * string
type gstate =               (* subset of the graphics state saved by a <g>. *)
  { mutable g_tr : M3.t } (* current transformation without view transform. *)

let init_gstate = { g_tr = M3.id }

type cmd = Set of gstate | Draw of Vgr.Private.Data.image
type state =
  { r : Vgr.Private.renderer;                    (* corresponding renderer. *)
    xmp : string option;
    buf : Buffer.t;                                   (* formatting buffer. *)
    t_buf : Buffer.t;                                  (* temporary buffer. *)
    mutable cost : int;                          (* cost counter for limit. *)
    mutable view : Gg.box2;           (* current renderable view rectangle. *)
    mutable todo : cmd list;                        (* commands to perform. *)
    mutable id : int;                                     (* uid generator. *)
    fonts : (font, svg_font) Hashtbl.t;                    (* cached fonts. *)
    prims :                                           (* cached primitives. *)
      (Vgr.Private.Data.tr list * Vgr.Private.Data.primitive,
       svg_prim) Hashtbl.t;
    paths : (path, path_id) Hashtbl.t;                     (* cached paths. *)
    clips : (path_id * P.area, clip_id) Hashtbl.t;         (* cached clips. *)
    mutable gstate : gstate; }                    (* current graphic state. *)

let max_buf = 65000
let new_id s = s.id <- s.id + 1; s.id
let save_gstate s = Set { g_tr = s.gstate.g_tr }
let set_gstate s g = s.gstate <- g
let view_rect s =          (* image view rect in current coordinate system. *)
  let tr = M3.inv s.gstate.g_tr in
  Vgr.Private.Data.of_path (P.empty |> P.rect (Box2.tr tr s.view))

let image i = Vgr.Private.I.of_data i
let partial = Vgr.Private.partial
let limit s = Vgr.Private.limit s.r
let warn s w = Vgr.Private.warn s.r w
let flush s k r =
  let clear k r = Buffer.clear s.buf; k r in
  Vgr.Private.writebuf s.buf 0 (Buffer.length s.buf) (clear k) r

let w_buf s k r = if Buffer.length s.buf > max_buf then flush s k r else k r
let b_fmt s fmt = Printf.bprintf s.buf fmt
let b_str s str = Buffer.add_string s.buf str
let b_esc_str s str = Vgr.Private.add_xml_data s.buf str

let area_str = function `Aeo -> "evenodd" | `Anz -> "nonzero"
let pr b fmt = Printf.bprintf b fmt
let pr_color buf c =
  let srgb = Color.to_srgb c in
  let r = Float.int_of_round (Color.r srgb *. 255.) in
  let g = Float.int_of_round (Color.g srgb *. 255.) in
  let b = Float.int_of_round (Color.b srgb *. 255.) in
  pr buf "#%02X%02X%02X" r g b

let pr_tr b = function
| Move v -> pr b "translate(%g %g)" (V2.x v) (V2.y v); M3.move2 v
| Rot a -> pr b "rotate(%g)" (Float.deg_of_rad a); M3.rot2 a
| Scale sv -> pr b "scale(%g %g)" (V2.x sv) (V2.y sv); M3.scale2 sv
| Matrix m ->
    pr b "transform(%g %g %g %g %g %g)"
      (M3.e00 m) (M3.e10 m) (M3.e01 m) (M3.e11 m) (M3.e02 m) (M3.e12 m); m

let get_font s font = try Hashtbl.find s.fonts font with
| Not_found ->
    (* N.B. it seems that specifying using style="font:..." or font="..."
       results in broken behaviour in one or other of the browsers. *)
    let font_str =
      Buffer.clear s.t_buf;
      pr s.t_buf "font-family=\"";
      Vgr.Private.add_xml_data s.t_buf font.Font.name;
      pr s.t_buf
        "\" font-style=\"%s\" font-weight=\"%s\" font-size=\"%g\""
        (Vgr.Private.Font.css_slant font) (Vgr.Private.Font.css_weight font)
        font.Font.size;
      Buffer.contents s.t_buf
    in
    Hashtbl.add s.fonts font font_str; font_str

let b_svg s xml_decl size =
  b_fmt s
     "%s\
      <svg xmlns=\"http://www.w3.org/2000/svg\" \
           xmlns:l=\"http://www.w3.org/1999/xlink\" \
           version=\"1.1\" \
           width=\"%gmm\" \
           height=\"%gmm\" \
           viewBox=\"0 0 %g %g\" \
           color-profile=\"auto\" \
           color-interpolation=\"linearRGB\" \
           color-interpolation-filters=\"linearRGB\">"
     (if xml_decl then "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" else "")
     (Size2.w size) (Size2.h size) (Size2.w size) (Size2.h size)

let b_xmp s = match s.xmp with
| None -> ()
| Some xmp ->
    b_fmt s "<?xpacket begin=\"\xEF\xBB\xBF\" \
                       id=\"W5M0MpCehiHzreSzNTczkc9d\"?>\
               <x:xmpmeta xmlns:x=\"adobe:ns:meta/\">%s</x:xmpmeta>\
             <?xpacket end=\"w\"?>" xmp

let b_init s size view =
  let sx = Size2.w size /. Box2.w view in
  let sy = Size2.h size /. Box2.h view in
  let dx = -. Box2.ox view *. sx in
  let dy = Size2.h size +. Box2.oy view *. sy in
  (* stroke-width is 1. initially, so is P.o *)
  (* stroke-linecap is butt initially, so is P.o *)
  (* stroke-linejoin is miter initially, so is P.o *)
  (* stroke-dasharray is none initially, so is P.o *)
  (* fill is black initially, we set it to none *)
  b_fmt s "<g fill=\"none\" stroke-miterlimit=\"%g\" \
              transform=\"matrix(%g %g %g %g %g %g)\">"
    (Vgr.Private.P.miter_limit P.o)
    sx 0. 0. (-. sy) dx dy (* map view rect -> viewport *)

let w_path s p k r = match Hashtbl.find s.paths p with
| id -> k id r
| exception Not_found ->
    let b_seg s = function
    | `Sub pt ->
        b_fmt s "M%g %g" (V2.x pt) (V2.y pt)
    | `Line pt ->
        b_fmt s "L%g %g" (V2.x pt) (V2.y pt)
    | `Qcurve (c, pt) ->
        b_fmt s "Q%g %g %g %g" (V2.x c) (V2.y c) (V2.x pt) (V2.y pt)
    | `Ccurve (c, c', pt) ->
        b_fmt s "C%g %g %g %g %g %g"
          (V2.x c) (V2.y c) (V2.x c') (V2.y c') (V2.x pt) (V2.y pt)
    | `Earc (large, cw, a, radii, pt) ->
        let large = if large then 1 else 0 in
        let sweep = if cw then 0 else 1 in
        b_fmt s "A %g %g %g %d %d %g %g"
          (V2.x radii) (V2.y radii) (Float.deg_of_rad a) large sweep
          (V2.x pt) (V2.y pt)
    | `Close ->
        b_fmt s "Z"
    in
    let id = new_id s in
    Hashtbl.add s.paths p id;
    b_fmt s "<defs><path id=\"i%d\" d=\"" id;
    List.iter (b_seg s) (List.rev p);
    b_str s "\"/></defs>";
    w_buf s (k id) r

let b_dashes s = function
| None -> ()
| Some (offset, dashes) ->
    let rec pr_dashes b ds = match ds with
    | [] -> ()
    | d :: [] -> pr b "%g" d
    | d :: ds -> pr b "%g," d; pr_dashes b ds
    in
    if offset <> 0. then b_fmt s " stroke-dashoffset=\"%g\"" offset;
    b_fmt s " stroke-dasharray=\"%a\"" pr_dashes dashes

let b_outline s o =
  let cap_str = function
  | `Butt -> "butt" | `Round -> "round" | `Square -> "square"
  in
  let join_str = function
  | `Miter -> "miter" | `Bevel -> "bevel" | `Round -> "round"
  in
  let w = o.P.width in
  let c = o.P.cap in
  let j = o.P.join in
  let ma = o.P.miter_angle in
  if w <> P.o.P.width then b_fmt s " stroke-width=\"%g\"" w;
  if c <> P.o.P.cap then b_fmt s " stroke-linecap=\"%s\"" (cap_str c);
  if j <> P.o.P.join then b_fmt s " stroke-linejoin=\"%s\"" (join_str j);
  if ma <> P.o.P.miter_angle then
    b_fmt s " stroke-miterlimit=\"%g\"" (Vgr.Private.P.miter_limit o);
  b_dashes s o.P.dashes

let b_svg_prim s op = function
| Color (c, "") -> b_fmt s " %s=\"%s\"" op c
| Color (c, a) -> b_fmt s " %s=\"%s\" %s-opacity=\"%s\"" op c op a
| Gradient id -> b_fmt s " %s=\"url(#i%d)\"" op id

let b_stop s (t, c) =
  let alpha = Color.a c in
  b_fmt s "<stop offset=\"%g\" stop-color=\"%a\"" t pr_color c;
  if alpha = 1.0 then b_fmt s "/>" else
  b_fmt s " stop-opacity=\"%g\"/>" alpha

let pr_gradient_transforms b ts =
  let rec pr_trs b = function
  | [] -> () | t :: ts -> ignore (pr_tr b t); pr_trs b ts
  in
  if ts = [] then () else
  (pr b " gradientTransform=\"%a\"" pr_trs ts)

let w_primitive s trs_p k r = match Hashtbl.find s.prims trs_p with
| id -> k id r
| exception Not_found ->
    let create = function
    | _, Const c ->
        let clear () = Buffer.clear s.t_buf in
        let contents () = Buffer.contents s.t_buf in
        let cstr = clear (); pr_color s.t_buf c; contents () in
        let astr =
          let a = Color.a c in
          if a = 1.0 then "" else (clear (); pr s.t_buf "%g" a; contents ())
        in
        Color(cstr, astr)
    | trs, Axial (stops, p1, p2) ->
        let id = new_id s in
        b_fmt s "<defs><linearGradient gradientUnits=\"userSpaceOnUse\" \
                   id=\"i%d\" x1=\"%g\" y1=\"%g\" x2=\"%g\" y2=\"%g\" %a>"
          id (V2.x p1) (V2.y p1) (V2.x p2) (V2.y p2) pr_gradient_transforms trs;
        List.iter (b_stop s) stops;
        b_fmt s "</linearGradient></defs>";
        Gradient id
    | trs, Radial (stops, f, c, r) ->
        let id = new_id s in
        b_fmt s "<defs><radialGradient gradientUnits=\"userSpaceOnUse\" \
                   id=\"i%d\" fx=\"%g\" fy=\"%g\" cx=\"%g\" cy=\"%g\" \
                   r=\"%g\" %a>"
          id (V2.x f) (V2.y f) (V2.x c) (V2.y c) r pr_gradient_transforms trs;
        List.iter (b_stop s) stops;
        b_fmt s "</radialGradient></defs>";
        Gradient id
    | _, Raster _ -> assert false
    in
    let svg_prim = create trs_p in
    Hashtbl.add s.prims trs_p svg_prim; w_buf s (k svg_prim) r

let w_primitive_cut s a path_id k svg_prim r = match a with
| `O o ->
    b_fmt s "<use l:href=\"#i%d\"" path_id;
    b_outline s o;
    b_svg_prim s "stroke" svg_prim;
    b_str s "/>";
    w_buf s k r
| `Anz | `Aeo ->
    let rule = if a = `Anz then "" else " fill-rule=\"evenodd\"" in
    b_fmt s "<use l:href=\"#i%d\"%s" path_id rule;
    b_svg_prim s "fill" svg_prim;
    b_str s "/>";
    w_buf s k r

let w_clip_path s a i path_id k r =
  let clip = path_id, a in
  match Hashtbl.find s.clips clip with
  | id -> k id r
  | exception Not_found ->
      let id = new_id s in
      let astr = match a with
      | `O _ -> warn s (`Unsupported_cut (a, image i)); area_str `Anz
      | `Anz | `Aeo as a -> area_str a
      in
      Hashtbl.add s.clips clip id;
      b_fmt s "<defs><clipPath id=\"i%d\" clip-rule=\"%s\">\
               <use l:href=\"#i%d\"/></clipPath></defs>" id astr path_id;
      w_buf s (k id) r

let w_clip s a i path_id k r =
  let w_clip k clip_id r =
    s.todo <- (Draw i) :: (save_gstate s) :: s.todo;
    b_fmt s "<g clip-path=\"url(#i%d)\">" clip_id;
    w_buf s k r
  in
  w_clip_path s a i path_id (w_clip k) r

let tr_primitive i =
  let rec loop acc = function
  | Primitive (Raster _) | Blend _ | Cut _ | Cut_glyphs _ -> None
  | Primitive p -> Some (List.rev acc, p)
  | Tr (tr, i) -> loop (tr :: acc) i
  in
  loop [] i

let w_cut s a i k path_id r = match i with
| Primitive (Raster _) -> assert false
| Primitive p -> w_primitive s ([], p) (w_primitive_cut s a path_id k) r
| Tr _ as i ->
    begin match tr_primitive i with
    | None -> w_clip s a i path_id k r
    | Some trs_p -> w_primitive s trs_p (w_primitive_cut s a path_id k) r
    end
| Blend _ | Cut _ | Cut_glyphs _ as i -> w_clip s a i path_id k r

let w_cut_glyphs s a run i k r = match i with
| Primitive (Raster _) -> assert false
| Tr _ | Blend _ | Cut _ | Cut_glyphs _ as i ->
    warn s (`Unsupported_glyph_cut (a, image i)); k r
| Primitive p as i ->
    begin match run.text with
    | None -> warn s (`Textless_glyph_cut (image (Cut_glyphs (a, run, i)))); k r
    | Some text ->
        let font = get_font s run.font in
        w_primitive s ([], p) begin fun svg_prim r ->
          (* font attribute doesn't seem to work !? *)
          b_fmt s "<text transform=\"scale(1,-1)\" %s " font;
          begin match a with
          | `O o -> b_outline s o; b_svg_prim s "stroke" svg_prim
          | `Anz | `Aeo -> b_svg_prim s "fill" svg_prim
          end;
          b_str s ">";
          b_esc_str s text;
          b_str s "</text>";
          w_buf s k r
        end r
    end

let w_transforms s i k r =         (* collapses nested Tr in single <g>. *)
  let rec loop acc = function
  | Tr (tr, i) ->
      let tr = pr_tr s.buf tr in
      loop (M3.mul acc tr) i
  | i ->
      b_str s "\">";
      s.todo <- (Draw i) :: save_gstate s :: s.todo;
      s.gstate.g_tr <- M3.mul s.gstate.g_tr acc;
  in
  b_str s "<g transform=\"";
  loop M3.id i;
  w_buf s k r

let rec w_image s k r =
  if s.cost > limit s then (s.cost <- 0; partial (w_image s k) r) else
  match s.todo with
  | [] ->
      Hashtbl.reset s.prims; Hashtbl.reset s.paths; Hashtbl.reset s.clips;
      Hashtbl.reset s.fonts; k r
  | Set gs :: todo ->
      set_gstate s gs;
      s.todo <- todo;
      b_str s "</g>";
      w_image s k r
  | (Draw i) :: todo ->
      s.cost <- s.cost + 1;
      match i with
      | Primitive _ as i ->           (* Uncut primitive, just cut to view. *)
          let p = view_rect s in
          s.todo <- (Draw (Cut (`Anz, p, i))) :: todo;
          w_image s k r
      | Cut (a, p, i) ->
          s.todo <- todo;
          w_path s p (w_cut s a i (w_image s k)) r
      | Cut_glyphs (a, run, i) ->
          s.todo <- todo;
          w_cut_glyphs s a run i (w_image s k) r
      | Blend (_, _, i, i') ->
          s.todo <- (Draw i') :: (Draw i) :: todo;
          w_image s k r
      | Tr _ as i ->
          s.todo <- todo;
          w_transforms s i (w_image s k) r

let render xml_decl s v k r = match v with
| `End -> b_str s "</g></svg>"; flush s (Vgr.Private.flush k) r
| `Image (size, view, i) ->
    b_svg s xml_decl size;
    b_xmp s;
    b_init s size view;
    s.todo <- [Draw i];
    s.view <- view;
    s.gstate <- { g_tr = init_gstate.g_tr }; (* copy *)
    w_buf s (w_image s k) r

let target ?(xml_decl = true) ?xmp () =
  let target r _ =
    false,
    render xml_decl { r;
                      xmp;
                      buf = Buffer.create 2048;
                      t_buf = Buffer.create 512;
                      cost = 0;
                      view = Box2.empty;
                      todo = [];
                      id = 0;
                      fonts = Hashtbl.create 17;
                      prims = Hashtbl.create 241;
                      paths = Hashtbl.create 241;
                      clips = Hashtbl.create 241;
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
