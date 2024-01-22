(*---------------------------------------------------------------------------
   Copyright (c) 2013 The vg programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Gg

let stdlib_compare = compare

(* Invalid_arg strings *)

let err_empty = "empty path"
let err_bounds j l = Printf.sprintf "invalid bounds (index %d, length %d)" j l
let err_exp_await = "`Await expected"
let err_end = "`End rendered, render can't be used on renderer"
let err_once = "a single `Image can be rendered"

(* Unsafe string byte manipulations. If you don't believe the authors's
   invariants, replacing with safe versions makes everything safe in the
   module. He won't be upset. *)

let unsafe_blit = Bytes.unsafe_blit
let unsafe_set_byte s j byte = Bytes.unsafe_set s j (Char.unsafe_chr byte)

(* A few useful definitions *)

external ( >> ) : 'a -> ('a -> 'b) -> 'b = "%revapply"
let eps = 1e-9
let io_buffer_size = 65536                          (* IO_BUFFER_SIZE 4.0.0 *)

(* Pretty printing *)

let pp ppf fmt = Format.fprintf ppf fmt
let pp_space = Format.pp_print_space
let pp_float ppf v = pp ppf "%g" v
let rec pp_list ?(pp_sep = Format.pp_print_cut) pp_v ppf = function
| [] -> ()
| v :: vs ->
    pp_v ppf v; if vs <> [] then (pp_sep ppf (); pp_list ~pp_sep pp_v ppf vs)

let to_string_of_formatter pp v =                       (* NOT thread safe. *)
  Format.fprintf Format.str_formatter "%a" pp v;
  Format.flush_str_formatter ()

(* Fonts *)

module Font = struct
  type slant = [ `Normal | `Italic | `Oblique ]
  type weight =
    [ `W100 | `W200 | `W300 | `W400 | `W500 | `W600 | `W700 | `W800 | `W900 ]

  type t = { name : string; slant : slant; weight : weight; size : float; }

  (* Predicates and comparisons *)

  let equal = ( = )
  let equal_f eq font font' =
    font.name = font'.name && eq font.size font'.size &&
    font.weight = font'.weight && font.slant = font'.slant

  let compare = stdlib_compare
  let compare_f cmp font font' =
    let c = stdlib_compare font.name font'.name in
    if c <> 0 then c else
    let c = stdlib_compare font.slant font'.slant in
    if c <> 0 then c else
    let c = stdlib_compare font.weight font'.weight in
    if c <> 0 then c else
    let c = cmp font.size font'.size in
    c

  (* Printers *)

  let weight_to_str = function
  | `W100 -> "100" | `W200 -> "200" | `W300 -> "300" | `W400 -> "400"
  | `W500 -> "500" | `W600 -> "600" | `W700 -> "700" | `W800 -> "800"
  | `W900 -> "900"

  let slant_to_str = function
  | `Normal -> "normal" | `Italic -> "italic" | `Oblique -> "oblique"

  let pp ppf font =
    pp ppf "@[<1>(font@ (name %s)@ (weight %s)@ (slant %s)@ (size %g))@]"
      font.name (weight_to_str font.weight) (slant_to_str font.slant) font.size

  let to_string p = to_string_of_formatter pp p
end

type glyph = int
type font = Font.t

(* Paths *)

module P = struct

  (* Path outline caps *)

  type cap = [ `Butt | `Round | `Square ]

  let pp_cap ppf = function
  | `Butt -> pp ppf "Butt" | `Round -> pp ppf "Round"
  | `Square -> pp ppf "Square"

  (* Path outline joins *)

  type join = [ `Miter | `Round | `Bevel ]

  let pp_join ppf = function
  | `Bevel -> pp ppf "Bevel" | `Miter -> pp ppf "Miter"
  | `Round -> pp ppf "Round"

  (* Path outline dashes *)

  type dashes = float * float list

  let eq_dashes eq d d' = match d, d' with
  | Some (f, ds), Some (f', ds') ->
      eq f f' && (try List.for_all2 eq ds ds' with Invalid_argument _ -> false)
  | d, d' -> d = d'

  let cmp_dashes cmp d d' = match d, d' with
  | Some (f, ds), Some (f', ds') ->
      let rec dashes ds ds' = match ds, ds' with
      | d :: ds, d' :: ds' ->
          let c = cmp d d' in
          if c <> 0 then c else dashes ds ds'
      | ds, ds' -> stdlib_compare ds ds'
      in
      let c = cmp f f' in
      if c <> 0 then c else dashes ds ds'
  | d, d' -> stdlib_compare d d'

  let pp_dashes pp_f ppf = function
  | None -> () | Some (f, ds) ->
      let pp_dashes ppf ds = pp_list ~pp_sep:pp_space pp_f ppf ds in
      pp ppf "@ (dashes %a @[<1>(%a)@])" pp_f f pp_dashes ds

  (* Path outlines *)

  type outline =
    { width : float; cap : cap; join : join; miter_angle : float;
      dashes : dashes option }

  let o = { width = 1.; cap = `Butt; join = `Miter;
            miter_angle = Float.rad_of_deg 11.5;
            dashes = None }

  let eq_outline eq o o' =
    eq o.width o'.width && o.cap = o'.cap && o.join = o'.join &&
    eq o.miter_angle o'.miter_angle && eq_dashes eq o.dashes o'.dashes

  let cmp_outline cmp o o' =
    let c = cmp o.width o'.width in
    if c <> 0 then c else
    let c = stdlib_compare o.cap o'.cap in
    if c <> 0 then c else
    let c = stdlib_compare o.join o'.join in
    if c <> 0 then c else
    let c = cmp o.miter_angle o'.miter_angle in
    if c <> 0 then c else cmp_dashes cmp o.dashes o'.dashes

  let pp_outline_f pp_f ppf o =
    pp ppf "@[<1>(outline@ (width %a)@ (cap %a)@ (join %a)\
            @ (miter-angle %a)%a)@]"
      pp_f o.width pp_cap o.cap pp_join o.join pp_f o.miter_angle
      (pp_dashes pp_f) o.dashes

  let pp_outline ppf o = pp_outline_f pp_float ppf o

  (* Path areas *)

  type area = [ `Aeo | `Anz | `O of outline ]

  let eq_area eq a a' = match a, a' with
  | `O o, `O o' -> eq_outline eq o o'
  | a, a' -> a = a'

  let cmp_area cmp a a' = match a, a' with
  | `O o, `O o' -> cmp_outline cmp o o'
  | a, a' -> stdlib_compare a a'

  let pp_area_f pp_f ppf = function
  | `Anz -> pp ppf "@[<1>anz@]"
  | `Aeo -> pp ppf "@[<1>aeo@]"
  | `O o -> pp ppf "%a" (pp_outline_f pp_f) o

  let pp_area ppf a = pp_area_f pp_float ppf a

  (* Paths *)

  type segment =
    [ `Sub of p2                          (* subpath start, "empty" segment *)
    | `Line of p2
    | `Qcurve of p2 * p2
    | `Ccurve of p2 * p2 * p2
    | `Earc of bool * bool * float * size2 * p2
    | `Close ]

  type t = segment list
  (* The list is reversed. The following invariants hold. The last
     element of the list is always `Sub. Between any two `Sub there is
     always at least one element different from `Sub. If there's an
     element preceding a `Close it's a `Sub. *)

  let empty = []

  (* This function finds the initial point of the subpath
     matching a `Close. The invariants on the datatype ensure it
     exists. *)
  let rec find_sub_start = function
  | `Sub pt :: _ -> pt
  | _ :: ss -> find_sub_start ss
  | [] -> assert false

  let last_pt = function
  | [] -> None
  | s :: ss ->
      match s with
      | `Sub pt | `Line pt | `Qcurve (_, pt) | `Ccurve (_, _, pt)
      | `Earc (_, _, _, _, pt) -> Some pt
      | `Close -> Some (find_sub_start ss)

  (* Subpath and segments *)

  let abs_origin p = match last_pt p with None -> P2.o | Some o -> o
  let abs p pt = match last_pt p with None -> pt | Some o -> V2.(o + pt)
  let close_empty_sub = function
  | (`Sub _ as s) :: p -> `Close :: s :: p
  | p -> p

  let push seg = function
  | [] | `Close :: _  as p -> seg :: `Sub P2.o :: p
  | p  -> seg :: p

  let sub ?(rel = false) pt p =
    let pt = if rel then abs p pt else pt in
    `Sub pt :: (close_empty_sub p)

  let line ?(rel = false) pt p =
    let pt = if rel then abs p pt else pt in
    push (`Line pt) p

  let qcurve ?(rel = false) c pt p =
    if not rel then push (`Qcurve (c, pt)) p else
    let o = abs_origin p in
    push (`Qcurve (V2.(o + c), V2.(o + pt))) p

  let ccurve ?(rel = false) c c' pt p =
    if not rel then push (`Ccurve (c, c', pt)) p else
    let o = abs_origin p in
    push (`Ccurve (V2.(o + c), V2.(o + c'), V2.(o + pt))) p

  let earc ?(rel = false) ?(large = false) ?(cw = false) ?(angle = 0.) r pt p =
    let pt = if rel then abs p pt else pt in
    push (`Earc (large, cw, angle, r, pt)) p

  let close p = push `Close p

  (* Derived subpaths *)

  let circle ?(rel = false) c r p =
    let c = if rel then abs p c else c in
    let cx = P2.x c in
    let cy = P2.y c in
    let a0 = P2.v (cx +. r) cy in
    let api = P2.v (cx -. r) cy in
    let r = V2.v r r in
    p |> sub a0 |> earc r api |> earc r a0 |> close

  let ellipse ?(rel = false) ?(angle = 0.) c r p =
    let c = if rel then abs p c else c in
    let cx = P2.x c in
    let cy = P2.y c in
    let xx = (if angle = 0. then 1.0 else cos angle) *. V2.x r in
    let xy = (if angle = 0. then 0.0 else sin angle) *. V2.x r in
    let a0 = P2.v (cx +. xx) (cy +. xy) in
    let api = P2.v (cx -. xx) (cy -. xy) in
    p |> sub a0 |> earc r ~angle api |> earc r ~angle a0 |> close

  let rect ?(rel = false) r p =
    if Box2.is_empty r then p else
    let lb = if rel then abs p (Box2.o r) else (Box2.o r) in
    let size = Box2.size r in
    let l = P2.x lb in
    let r = l +. Size2.w size in
    let b = P2.y lb in
    let t = b +. Size2.h size in
    p |> sub lb |> line (P2.v r b) |> line (P2.v r t) |> line (P2.v l t) |>
    close

  let rrect ?(rel = false) r cr p =
    if Box2.is_empty r then p else
    let lb = if rel then abs p (Box2.o r) else (Box2.o r) in
    let size = Box2.size r in
    let rx = V2.x cr in
    let ry = V2.y cr in
    let l = P2.x lb in
    let l_inset = l +. rx in
    let r = l +. Size2.w size in
    let r_inset = r -. rx in
    let b = P2.y lb in
    let b_inset = b +. ry in
    let t = b +. Size2.h size in
    let t_inset = t -. ry in
    p |> sub (P2.v l b_inset) |>
    earc cr (P2.v l_inset b) |> line (P2.v r_inset b) |>
    earc cr (P2.v r b_inset) |> line (P2.v r t_inset) |>
    earc cr (P2.v r_inset t) |> line (P2.v l_inset t) |>
    earc cr (P2.v l t_inset) |> close

  let last_control_point kind = function
  | [] ->
      (* The invariant on the path ensures the last element of a path
         is `Sub. To preserve this invariant, the function [push] always
         adds an element [`Sub P2.o] if the path is empty. Therefore,
         for consistency, we return [P2.o] too. *)
      P2.o
  | s :: ss ->
      match s with
      | `Qcurve (c, _) when kind = `Quadratic -> c
      | `Ccurve (_, c', _) when kind = `Cubic -> c'
      | `Sub pt | `Line pt | `Qcurve (_, pt) | `Ccurve (_ ,_ , pt)
      | `Earc (_, _, _, _, pt) -> pt
      | `Close -> find_sub_start ss

  let smooth_qcurve ?(rel = false) pt p =
    let o = abs_origin p in
    let c =
      last_control_point `Quadratic p |> P2.tr (M3.rot2 ~pt:o Float.pi)
    in
    if not rel then push (`Qcurve (c, pt)) p else
    push (`Qcurve (c, V2.(o + pt))) p

  let smooth_ccurve ?(rel = false) c' pt p =
    let o = abs_origin p in
    let c = last_control_point `Cubic p |> P2.tr (M3.rot2 ~pt:o Float.pi) in
    if not rel then push (`Ccurve (c, c', pt)) p else
    push (`Ccurve (c, V2.(o + c'), V2.(o + pt))) p

  (* Geometry *)

  (* See Vgr.Private.P.earc_params in mli file for the doc. The center is
     found by first transforming the points on the ellipse to points on
     a unit circle (i.e. we rotate by -a and scale by 1/rx 1/ry). *)

  let earc_params p0 ~large ~cw a r p1 =
    let rx = V2.x r in let ry = V2.y r in
    let x0 = V2.x p0 in let y0 = V2.y p0 in
    let x1 = V2.x p1 in let y1 = V2.y p1 in
    if Float.is_zero ~eps rx || Float.is_zero ~eps ry then None else
    let sina = Float.round_zero ~eps (sin a) in
    let cosa = Float.round_zero ~eps (cos a) in
    let x0' = (cosa *. x0 +. sina *. y0) /. rx in(* transform to unit circle *)
    let y0' = (-. sina *. x0 +. cosa *. y0) /. ry in
    let x1' = (cosa *. x1 +. sina *. y1) /. rx in
    let y1' = (-. sina *. x1 +. cosa *. y1) /. ry in
    let vx = x1' -. x0' in
    let vy = y1' -. y0' in
    let nx = vy in                                       (* normal to p0'p1' *)
    let ny = -. vx in
    let nn = (nx *. nx) +. (ny *. ny) in
    if Float.is_zero ~eps nn then None (* points coincide *) else
    let d2 = Float.round_zero ~eps (1. /. nn -. 0.25) in
    if d2 < 0. then None (* points are too far apart *) else
    let d = sqrt d2 in
    let d = if (large && cw) || (not large && not cw) then -. d else d in
    let cx' = 0.5 *. (x0' +. x1') +. d *. nx  in            (* circle center *)
    let cy' = 0.5 *. (y0' +. y1') +. d *. ny in
    let t0 = atan2 (y0' -. cy') (x0' -. cx') in              (* angle of p0' *)
    let t1 = atan2 (y1' -. cy') (x1' -. cx') in
    let dt = (t1 -. t0) in
    let adjust =
      if dt > 0. && cw then -. 2. *. Float.pi else
      if dt < 0. && not cw then 2. *. Float.pi else
      0.
    in
    let t1 = t0 +. (dt +. adjust) in                         (* angle of p1' *)
    let e1x = rx *. cosa in
    let e1y = rx *. sina in
    let e2x = -. ry *. sina in
    let e2y = ry *. cosa in
    let cx = e1x *. cx' +. e2x *. cy' in            (* transform center back *)
    let cy = e1y *. cx' +. e2y *. cy' in
    let m = M2.v e1x e2x
                 e1y e2y
    in
    Some ((P2.v cx cy), m, t0, t1)

  let casteljau pt c c' pt' t =
    let b00 = V2.mix pt c t in
    let b01 = V2.mix c c' t in
    let b02 = V2.mix c' pt' t in
    let b10 = V2.mix b00 b01 t in
    let b11 = V2.mix b01 b02 t in
    let b = V2.mix b10 b11 t in
    b

  (* Functions *)

  let last_pt p = match last_pt p with
  | None -> invalid_arg err_empty | Some pt -> pt

  let append p' p =
    let p = close_empty_sub p in
    List.rev_append (List.rev p') p

  let tr m p =
    let det = lazy (M3.det m) in
    let tr_seg m = function
    | `Sub pt -> `Sub (P2.tr m pt)
    | `Line pt -> `Line (P2.tr m pt)
    | `Qcurve (c, pt) -> `Qcurve (P2.tr m c, P2.tr m pt)
    | `Ccurve (c, c', pt) -> `Ccurve (P2.tr m c, P2.tr m c', P2.tr m pt)
    | `Earc (l, cw, a, r, pt) ->
        let sina = sin a in
        let cosa = cos a in
        let rx = V2.x r in
        let ry = V2.y r in
        let ax = V2.v (cosa *. rx) (sina *. rx) in
        let ay = V2.v (-. sina *. ry) (cosa *. ry) in
        (* FIXME this won't work, ax' and ay' may no longer be orthnormal
           with m shears or doesn't scale uniformly. Need to go through
           center parameterization transform there and then back to end point
           parameterization. *)
        let ax' = V2.tr m ax in
        let ay' = V2.tr m ay in
        let a' = atan2 (V2.y ax') (V2.x ax') in
        let rx' = V2.norm ax' in
        let ry' = V2.norm ay' in
        let cw = if Lazy.force det < 0. then not cw else cw in
        `Earc (l, cw, a', (V2.v rx' ry'), (P2.tr m pt))
    | `Close -> `Close
    in
    List.rev (List.rev_map (tr_seg m) p)

  (* Traversal *)

  type fold = segment
  let fold ?(rev = false) f acc p =
    List.fold_left f acc (if rev then p else List.rev p)

  (* Predicates and comparisons *)

  let is_empty = function [] -> true | _ -> false
  let equal p p' = p = p'
  let rec equal_f eq p p' =
    let equal_seg eq s s' = match s, s' with
    | `Sub pt, `Sub pt'
    | `Line pt, `Line pt' ->
        V2.equal_f eq pt pt'
    | `Qcurve (c0, pt), `Qcurve (c0', pt') ->
        V2.equal_f eq c0 c0' && V2.equal_f eq pt pt'
    | `Ccurve (c0, c1, pt), `Ccurve (c0', c1', pt') ->
        V2.equal_f eq c0 c0' && V2.equal_f eq c1 c1' && V2.equal_f eq pt pt'
    | `Earc (l, cw, a, r, pt), `Earc (l', cw', a', r', pt') ->
        l = l' && cw = cw' && eq a a' && V2.equal_f eq r r' &&
        V2.equal_f eq pt pt'
    | `Close, `Close -> true
    | _, _ -> false
    in
    match p, p' with
    | s :: p, s' :: p' -> if equal_seg eq s s' then equal_f eq p p' else false
    | [], [] -> true
    | _ -> false

  let compare p p' = stdlib_compare p p'
  let rec compare_f cmp p p' =
    let compare_seg cmp s s' = match s, s' with
    | `Sub pt, `Sub pt'
    | `Line pt, `Line pt' ->
        V2.compare_f cmp pt pt'
    | `Qcurve (c0, pt), `Qcurve (c0', pt') ->
        let c = V2.compare_f cmp c0 c0' in
        if c <> 0 then c else V2.compare_f cmp pt pt'
    | `Ccurve (c0, c1, pt), `Ccurve (c0', c1', pt') ->
        let c = V2.compare_f cmp c0 c0' in
        if c <> 0 then c else
        let c = V2.compare_f cmp c1 c1' in
        if c <> 0 then c else V2.compare_f cmp pt pt'
    | `Earc (l, cw, a, r, pt), `Earc (l', cw', a', r', pt') ->
        let c = stdlib_compare l l' in
        if c <> 0 then c else
        let c = stdlib_compare cw cw' in
        if c <> 0 then c else
        let c = cmp a a' in
        if c <> 0 then c else
        let c = V2.compare_f cmp r r' in
        if c <> 0 then c else V2.compare_f cmp pt pt'
    | s, s' -> stdlib_compare s s'
    in
    match p, p' with
    | s :: p, s' :: p' ->
        let c = compare_seg cmp s s' in
        if c <> 0 then c else compare_f cmp p p'
    | p, p' -> stdlib_compare p p'

  (* Printers *)

  let pp_seg pp_f pp_v2 ppf = function
  | `Sub pt ->
      pp ppf "@ S@ %a" pp_v2 pt
  | `Line pt ->
      pp ppf "@ L@ %a" pp_v2 pt
  | `Qcurve (c, pt) ->
      pp ppf "@ Qc@ %a@ %a" pp_v2 c pp_v2 pt
  | `Ccurve (c, c', pt) ->
      pp ppf "@ Cc@ %a@ %a@ %a" pp_v2 c pp_v2 c' pp_v2 pt
  | `Earc (l, cw, a, r, pt) ->
      let l = if l then "large" else "small" in
      let cw = if cw then "cw" else "ccw" in
      pp ppf "@ E@ %s@ %s@ %a@ %a@ %a" l cw pp_f a pp_v2 r pp_v2 pt
  | `Close ->
      pp ppf "@ Z"

  let pp_path pp_f ppf p =
    let pp_v2 = V2.pp_f pp_f in
    let pp_segs ppf ss = List.iter (pp_seg pp_f pp_v2 ppf) ss in
    pp ppf "@[<1>(path%a)@]" pp_segs (List.rev p)

  let pp_f pp_f ppf p = pp_path pp_f ppf p
  let pp ppf p = pp_path pp_float ppf p
  let to_string p = to_string_of_formatter pp p
end

type path = P.t

(* Images *)

module I = struct

  (* Blenders *)

  type blender = [ `Atop | `In | `Out | `Over | `Plus | `Copy | `Xor ]

  let pp_blender ppf = function
  | `Atop -> pp ppf "Atop" | `Copy -> pp ppf "Copy" | `In -> pp ppf "In"
  | `Out -> pp ppf "Out" | `Over -> pp ppf "Over" | `Plus -> pp ppf "Plus"
  | `Xor -> pp ppf "Xor"

  (* Transforms *)

  type tr = Move of v2 | Rot of float | Scale of v2 | Matrix of m3

  let eq_tr eq tr tr' = match tr, tr' with
  | Move v, Move v' -> V2.equal_f eq v v'
  | Rot r, Rot r' -> eq r r'
  | Scale s, Scale s' -> V2.equal_f eq s s'
  | Matrix m, Matrix m' -> M3.equal_f eq m m'
  | _, _ -> false

  let cmp_tr cmp tr tr' = match tr, tr' with
  | Move v, Move v' -> V2.compare_f cmp v v'
  | Rot r, Rot r' -> cmp r r'
  | Scale s, Scale s' -> V2.compare_f cmp s s'
  | Matrix m, Matrix m' -> M3.compare_f cmp m m'
  | tr, tr' -> compare tr tr'

  let pp_tr pp_f ppf = function
  | Move v -> pp ppf "(move %a)" (V2.pp_f pp_f) v
  | Rot a -> pp ppf "(rot %a)" pp_f a
  | Scale s -> pp ppf "(scale %a)" (V2.pp_f pp_f) s
  | Matrix m -> pp ppf "%a" (M3.pp_f pp_f) m

  (* Color stops *)

  let pp_stops pp_f ppf ss =
    let pp_stop ppf (s, c) = pp ppf "@ %a@ %a" pp_f s (V4.pp_f pp_f) c in
    pp ppf "@[<1>(stops%a)@]" (fun ppf ss -> List.iter (pp_stop ppf) ss) ss

  let rec eq_stops eq ss ss' = match ss, ss' with
  | (s, c) :: ss, (s', c') :: ss' ->
      eq s s' && V4.equal_f eq c c' && eq_stops eq ss ss'
  | [], [] -> true
  | _, _ -> false

  let rec cmp_stops cmp ss ss' = match ss, ss' with
  | (s, sc) :: ss, (s', sc') :: ss' ->
      let c = cmp s s' in
      if c <> 0 then c else
      let c = V4.compare_f cmp sc sc' in
      if c <> 0 then c else cmp_stops cmp ss ss'
  | ss, ss' -> stdlib_compare ss ss'

  (* Primitives *)

  type primitive =
    | Const of color
    | Axial of Color.stops * p2 * p2
    | Radial of Color.stops * p2 * p2 * float
    | Raster of box2 * raster

  let eq_primitive eq i i' = match i, i' with
  | Const c, Const c' ->
      V4.equal_f eq c c'
  | Axial (stops, p1, p2), Axial (stops', p1', p2') ->
      V2.equal_f eq p1 p1' && V2.equal_f eq p2 p2' && eq_stops eq stops stops'
  | Radial (stops, p1, p2, r), Radial (stops', p1', p2', r') ->
      V2.equal_f eq p1 p1' && V2.equal_f eq p2 p2' && eq r r' &&
      eq_stops eq stops stops'
  | Raster (r, ri), Raster (r', ri') ->
      Box2.equal_f eq r r' && Raster.equal ri ri'
  | _, _ -> false

  let cmp_primitive cmp i i' = match i, i' with
  | Const c, Const c' ->
      V4.compare_f cmp c c'
  | Axial (stops, p1, p2), Axial (stops', p1', p2') ->
      let c = cmp_stops cmp stops stops' in
      if c <> 0 then c else
      let c = V2.compare_f cmp p1 p1' in
      if c <> 0 then c else V2.compare_f cmp p2 p2'
  | Radial (stops, p1, p2, r), Radial (stops', p1', p2', r') ->
      let c = cmp_stops cmp stops stops' in
      if c <> 0 then c else
      let c = V2.compare_f cmp p1 p1' in
      if c <> 0 then c else
      let c = V2.compare_f cmp p2 p2' in
      if c <> 0 then c else cmp r r'
  | Raster (r, ri), Raster (r', ri') ->
      let c = Box2.compare_f cmp r r' in
      if c <> 0 then c else Raster.compare ri ri'
  | i, i' -> stdlib_compare i i'

  let pp_primitive pp_f ppf = function
  | Const c ->
      pp ppf "@[<1>(i-const@ %a)@]" (V4.pp_f pp_f) c
  | Axial (stops, p, p') ->
      pp ppf "@[<1>(i-axial@ %a@ %a@ %a)@]"
        (pp_stops pp_f) stops (V2.pp_f pp_f) p (V2.pp_f pp_f) p'
  | Radial (stops, p, p', r) ->
      pp ppf "@[<1>(i-radial@ %a@ %a@ %a@ %a)@]"
        (pp_stops pp_f) stops (V2.pp_f pp_f) p (V2.pp_f pp_f) p' pp_f r
  | Raster (r, ri) ->
      pp ppf "@[<1>(i-raster %a@ %a)@]" (Box2.pp_f pp_f) r Raster.pp ri

  (* Glyph runs *)

  type glyph_run =
    { font : font;
      text : string option;
      o : p2;
      blocks : bool * (int * int) list;
      advances : v2 list;
      glyphs : glyph list; }

  let eq_blocks (rev0, bs0) (rev1, bs1) = rev0 = rev1 && bs0 = bs1
  let cmp_blocks b0 b1 = stdlib_compare b0 b1

  let eq_advances eq r1 r2 =
    try List.for_all2 (V2.equal_f eq) r1.advances r2.advances with
    | Invalid_argument _ -> false

  let cmp_advances cmp a1s a2s =
    let rec adv a1s a2s = match a1s, a2s with
    | a1 :: a1s, a2 :: a2s ->
        let c = V2.compare_f cmp a1 a2 in
        if c <> 0 then c else adv a1s a2s
    | a1s, a2s -> stdlib_compare a1s a2s
    in
    adv a1s a2s

  let eq_glyph_run eq r1 r2 =
    Font.equal_f eq r1.font r2.font && r1.text = r2.text &&
    V2.equal_f eq r1.o r2.o && eq_blocks r1.blocks r2.blocks &&
    eq_advances eq r1 r2 && r1.glyphs = r2.glyphs

  let cmp_glyph_run cmp r1 r2 =
    let c = Font.compare_f cmp r1.font r2.font in
    if c <> 0 then c else
    let c = stdlib_compare r1.text r2.text in
    if c <> 0 then c else
    let c = V2.compare_f cmp r1.o r2.o in
    if c <> 0 then c else
    let c = cmp_blocks r1.blocks r2.blocks in
    if c <> 0 then c else
    let c = cmp_advances cmp r1.advances r2.advances in
    if c <> 0 then c else
    stdlib_compare r1.glyphs r2.glyphs

  let pp_glyph_run ppf r =
    let pp_text ppf = function
    | None -> ()
    | Some t -> pp ppf "@ @[<1>(text \"%s\")@]" t
    in
    let pp_blocks ppf (rev, blocks) =
      pp ppf "@ @[<1>(blocks@ (rev %b)" rev;
      List.iter (fun (ul, gl) -> pp ppf "@ (%d,%d)" ul gl) blocks;
      pp ppf ")@]"
    in
    let pp_advances ppf advs =
      pp ppf "@ @[<1>(advances";
      List.iter (fun a -> pp ppf "@ %a" V2.pp a) advs;
      pp ppf ")@]"
    in
    let pp_glyphs ppf glyphs =
      pp ppf "@ @[<1>(glyphs";
      List.iter (fun g -> pp ppf "@ %d" g) r.glyphs;
      pp ppf ")@]"
    in
    pp ppf "@[<1>(glyph_run %a%a@ @[<1>(o %a)@]%a%a%a)@]"
      Font.pp r.font pp_text r.text V2.pp r.o pp_blocks r.blocks
      pp_advances r.advances pp_glyphs r.glyphs

  (* Images *)

  type t =
    | Primitive of primitive
    | Cut of P.area * P.t * t
    | Cut_glyphs of P.area * glyph_run * t
    | Blend of blender * float option * t * t
    | Tr of tr * t

  (* Primitive images *)

  let const c = Primitive (Const c)
  let void = const Color.void
  let axial stops pt pt' = Primitive (Axial (stops, pt, pt'))
  let raster b r = Primitive (Raster (b, r))
  let radial stops ?f c r =
    let f = match f with None -> c | Some f -> f in
    Primitive (Radial (stops, f, c, r))

  (* Cutting images *)

  let cut ?(area = `Anz) p i = Cut (area, p, i)
  let cut_glyphs ?area ?text ?(blocks = (false, [])) ?(advances = [])
      font glyphs i =
    let area = match area with None -> `Anz | Some o -> (o :> P.area) in
    let run = { font; text; o = P2.o; blocks; advances; glyphs; } in
    Cut_glyphs (area, run , i)

  (* Blending images *)

  let blend src dst = Blend (`Over, None, src, dst)

  (* Transforming images *)

  let move v i = Tr (Move v, i)
  let rot a i = Tr (Rot a, i)
  let scale s i = Tr (Scale s, i)
  let tr m i = Tr (Matrix m, i)

  (* Predicates and comparisons *)

  let is_void i = i == void
  let equal i i' = i = i'
  let equal_f eq i i' =
    let eq_alpha eq a a' = match a, a' with
    | Some a, Some a' -> eq a a'
    | a, a' -> a = a'
    in
    let rec loop = function
    | [] -> false
    | (i, i') :: acc ->
        match i, i' with
        | Primitive i, Primitive i' ->
            eq_primitive eq i i'
        | Cut (a, p, i), Cut (a', p', i') ->
            P.eq_area eq a a' && P.equal_f eq p p' && loop ((i, i') :: acc)
        | Cut_glyphs (a, r, i), Cut_glyphs (a', r', i') ->
            P.eq_area eq a a' && eq_glyph_run eq r r' && loop ((i, i') :: acc)
        | Blend (b, a, i1, i2), Blend (b', a', i1', i2') ->
            b = b' && eq_alpha eq a a' && loop ((i1, i1') :: (i2, i2') :: acc)
        | Tr (tr, i), Tr (tr', i') ->
            eq_tr eq tr tr' && loop ((i, i') :: acc)
        | _, _ -> false
    in
    loop [(i, i')]

  let compare i i' = stdlib_compare i i'
  let compare_f cmp i i' =
    let cmp_alpha cmp a a' = match a, a' with
    | Some a, Some a' -> cmp a a'
    | a, a' -> stdlib_compare a a'
    in
    let rec loop = function
    | [] -> assert false
    | (i, i') :: acc ->
        match i, i' with
        | Primitive i, Primitive i' ->
            cmp_primitive cmp i i'
        | Cut (a, p, i), Cut (a', p', i') ->
            let c = P.cmp_area cmp a a' in
            if c <> 0 then c else
            let c = P.compare_f cmp p p' in
            if c <> 0 then c else loop ((i, i') :: acc)
        | Cut_glyphs (a, r, i), Cut_glyphs (a', r', i') ->
            let c = P.cmp_area cmp a a' in
            if c <> 0 then c else
            let c = cmp_glyph_run cmp r r' in
            if c <> 0 then c else loop ((i, i') :: acc)
        | Blend (b, a, i1, i2), Blend (b', a', i1', i2') ->
            let c = stdlib_compare b b' in
            if c <> 0 then c else
            let c = cmp_alpha cmp a a' in
            if c <> 0 then c else
            loop ((i1, i1') :: (i2, i2') :: acc)
        | Tr (tr, i), Tr (tr', i') ->
            let c = cmp_tr cmp tr tr' in
            if c <> 0 then c else
            loop ((i, i') :: acc)
        | i, i' -> stdlib_compare i i'
    in
    loop [(i, i')]

  (* Printers *)

  let pp_image pp_f ppf i =
    let pp_alpha pp_f ppf = function
    | None -> () | Some a -> pp ppf "@ (alpha@ %a)" pp_f a
    in
    let rec loop = function
    | [] -> ()
    | `Pop :: todo -> pp ppf ")@]"; loop todo
    | `Sep :: todo -> pp ppf "@ "; loop todo
    | `I i :: todo ->
        match i with
        | Primitive prim ->
            pp ppf "%a" (pp_primitive pp_f) prim;
            loop todo
        | Cut (a, p, i) ->
            pp ppf "@[<1>(i-cut@ %a@ %a@ "(P.pp_area_f pp_f) a (P.pp_f pp_f) p;
            loop (`I i :: `Pop :: todo)
        | Cut_glyphs (a, r, i) ->
            pp ppf "@[<1>(i-cut-glyphs %a@ %a@ "
              (P.pp_area_f pp_f) a pp_glyph_run r;
            loop (`I i :: `Pop :: todo)
        | Blend (b, a, i, i') ->
            pp ppf "@[<1>(i-blend@ %a%a@ " pp_blender b (pp_alpha pp_f) a;
            loop (`I i :: `Sep :: `I i' :: `Pop :: todo)
        | Tr (tr, i) ->
            pp ppf "@[<1>(i-tr@ %a@ " (pp_tr pp_f) tr;
            loop (`I i :: `Pop :: todo)
    in
    loop [`I i]

  let pp_f pp_f ppf i = pp_image pp_f ppf i
  let pp ppf i = pp_image pp_float ppf i
  let to_string p = to_string_of_formatter pp p
end

type image = I.t

(* Image renderers *)

module Vgr = struct

  (* Render warnings *)

  type warning =
    [ `Unsupported_cut of P.area * I.t
    | `Unsupported_glyph_cut of P.area * I.t
    | `Textless_glyph_cut of I.t
    | `Other of string ]

  type warn = warning -> unit

  let pp_warning ppf w =
    let pp_area ppf = function
    | `Aeo -> pp ppf "even-odd"
    | `Anz -> pp ppf "non-zero"
    | `O _ -> pp ppf "outline"
    in
    match w with
    | `Other o ->
        pp ppf "%s" o
    | `Unsupported_cut (a, _) ->
        pp ppf "Unsupported cut: %a" pp_area a
    | `Unsupported_glyph_cut (a, _) ->
        pp ppf "Unsupported glyph cut: %a" pp_area a
    | `Textless_glyph_cut _ ->
        pp ppf "Missing text in glyph cut"

  (* Render metadata *)

  let decompose_posix_time pt =              (* (YYYY, MM, DD), (hh, mm, ss) *)
    let mjd_origin_jd = 2_400_001 in         (* origin of mjd in julian day. *)
    let posix_epoch = 40587 in              (* origin of posix epoch in mjd. *)
    let day = 86_400_000. in
    let hour = 3600_000 in
    let minute = 60_000 in
    let sec = 1_000 in
    let to_gregorian mjd =                             (* cf. calendar FAQ. *)
      let jd = mjd + mjd_origin_jd in                      (* to julian day *)
      let a = jd + 32044 in
      let b = (4 * a + 3) / 146097 in
      let c = a - ((146097 * b) / 4) in
      let d = (4 * c + 3) / 1461 in
      let e = c - ((1461 * d) / 4) in
      let m = (5 * e + 2) / 153 in
      let gd = e - ((153 * m + 2) / 5) + 1 in
      let gm = m + 3 - (12 * (m / 10)) in
      let gy = 100 * b + d - 4800 + (m / 10) in
      (gy, gm, gd)
    in
    let to_hhmmss dt =
      let hh = dt / hour in
      let rem = dt mod hour in
      let mm = rem / minute in
      let ss = (rem mod minute) / sec in
      (hh, mm, ss)
    in
    let ms = floor (pt *. 1000.) in              (* work with milliseconds. *)
    if ms >= 0. then
      let days = truncate (ms /. day) in
      let dt = truncate (mod_float ms day) in
      (to_gregorian (posix_epoch + days), (to_hhmmss dt))
    else
      let ms = ms +. 1. in
      let days = truncate (ms /. day) - 1 in
      let dt = truncate (day +. (mod_float ms day)) - 1 in
      (to_gregorian (posix_epoch + days), (to_hhmmss dt))

  let add_xml_data b str =
    let len = String.length str in
    let start = ref 0 in
    let last = ref 0 in
    let escape e =
      Buffer.add_substring b str !start (!last - !start);
      Buffer.add_string b e;
      incr last;
      start := !last
    in
    while (!last < len) do match String.get str !last with
    | '<' -> escape "&lt;"         (* Escape markup delimiters. *)
    | '>' -> escape "&gt;"
    | '&' -> escape "&amp;"
    (* | '\'' -> escape "&apos;" *) (* Not needed we use \x22 for attributes. *)
    | '\x22' -> escape "&quot;"
    | '\n' | '\t' | '\r' -> incr last
    | c when c < ' ' -> escape "\xEF\xBF\xBD" (* illegal, subst. by U+FFFD *)
    | _ -> incr last
    done;
    Buffer.add_substring b str !start (!last - !start)

  let xmp ?title ?(authors = []) ?(subjects = []) ?description ?rights
      ?creator_tool ?create_date () =
    let fmt = Printf.bprintf in
    let esc = add_xml_data in
    let rec b_list b = function
    | [] -> () | v :: vs -> fmt b "<r:li>%a</r:li>" esc v; b_list b vs in
    let b_title b = function
    | None -> ()
    | Some t -> fmt b "<d:title><r:Alt><r:li xml:lang=\"x-default\">%a\
                       </r:li></r:Alt></d:title>" esc t
    in
    let b_authors b = function
    | [] -> () | l -> fmt b "<d:creator><r:Seq>%a</r:Seq></d:creator>" b_list l
    in
    let b_subjects b = function
    | [] -> () | l -> fmt b "<d:subject><r:Bag>%a</r:Bag></d:subject>" b_list l
    in
    let b_description b = function
    | None -> ()
    | Some d -> fmt b "<d:description><r:Alt><r:li xml:lang=\"x-default\">%a\
                       </r:li></r:Alt></d:description>" esc d
    in
    let b_rights b = function
    | None -> ()
    | Some r -> fmt b "<d:rights><r:Alt><r:li xml:lang=\"x-default\">%a\
                       </r:li></r:Alt></d:rights>" esc r
    in
    let b_creator_tool b = function
    | None -> () | Some c -> fmt b "<x:CreatorTool>%a</x:CreatorTool>" esc c
    in
    let b_create_date b = function
    | None -> ()
    | Some t ->
        let (y, m, d), (hh, mm, ss) = decompose_posix_time t in
        fmt b "<x:CreateDate>%04d-%02d-%02dT%02d:%02d:%02dZ\
               </x:CreateDate>" y m d hh mm ss
    in
    let b = Buffer.create 1024 in
    fmt b "<r:RDF xmlns:r=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\" \
                  xmlns:d=\"http://purl.org/dc/elements/1.1/\" \
                  xmlns:x=\"http://ns.adobe.com/xap/1.0/\">\
            <r:Description r:about=\"\">%a%a%a%a%a%a%a</r:Description>\
           </r:RDF>"
      b_title title b_authors authors b_subjects subjects
      b_description description b_rights rights
      b_creator_tool creator_tool b_create_date create_date;
    Buffer.contents b

  (* Renderable *)

  type renderable = size2 * box2 * image

  (* Rendering *)

  type dst_stored =
    [ `Buffer of Buffer.t | `Channel of out_channel | `Manual ]

  type dst = [ dst_stored | `Other ]

  type t =
    { dst : dst;                                     (* output destination. *)
      mutable o : bytes;             (* current output chunk (stored dsts). *)
      mutable o_pos : int;                (* next output position to write. *)
      mutable o_max : int;             (* maximal output position to write. *)
      limit : int;                                         (* render limit. *)
      warn : warn;                                     (* warning callback. *)
      mutable k :                                   (* render continuation. *)
        [`Await | `End | `Image of size2 * box2 * image ] -> t ->
        [ `Ok | `Partial ] }

  type k = t -> [ `Ok | `Partial ]
  type render_fun = [`End | `Image of size2 * box2 * image ] -> k -> k
  type 'a target = t -> 'a -> bool * render_fun constraint 'a = [< dst]

  let expect_await k v r = match v with
  | `Await -> k r | _ -> invalid_arg err_exp_await

  let expect_none v r = match v with
  | `Await | `End | `Image _ -> invalid_arg err_end

  let ok k r = r.k <- k; `Ok
  let partial k r = r.k <- expect_await k; `Partial

  let rec r_once (rfun : render_fun) v r = match v with
  | `End -> rfun `End (ok expect_none) r
  | (`Image _) as i ->
      let rec render_end v r = match v with
      | `End -> rfun `End (ok expect_none) r
      | `Image _ -> invalid_arg err_once
      | `Await -> ok render_end r
      in
      rfun i (ok render_end) r
  | `Await -> ok (r_once rfun) r

  let rec r_loop (rfun : render_fun) v r = match v with
  | `End -> rfun `End (ok expect_none) r
  | `Image _ as i -> rfun i (ok (r_loop rfun)) r
  | `Await -> ok (r_loop rfun) r

  let create ?(limit = max_int) ?(warn = fun _ -> ()) target dst =
    let o, o_pos, o_max = match dst with
    | `Manual | `Other -> Bytes.empty, 1, 0        (* implies [o_rem e = 0]. *)
    | `Buffer _
    | `Channel _ -> Bytes.create io_buffer_size, 0, io_buffer_size - 1
    in
    let k _ _ = assert false in
    let r = { dst = (dst :> dst); o; o_pos; o_max; limit; warn; k} in
    let multi, rfun = target r dst in
    r.k <- if multi then r_loop rfun else r_once rfun;
    r

  let render r v = r.k (v :> [ `Await | `End | `Image of renderable ]) r
  let renderer_dst r = r.dst
  let renderer_limit r = r.limit

  (* Manual rendering destinations *)

  module Manual = struct
    let dst r s j l =                                (* set [r.o] with [s]. *)
      if (j < 0 || l < 0 || j + l > Bytes.length s)
      then invalid_arg (err_bounds j l);
      r.o <- s; r.o_pos <- j; r.o_max <- j + l - 1

    let dst_rem r = r.o_max - r.o_pos + 1   (* rem bytes to write in [r.o]. *)
  end

  (* Implementing renderers. *)

  module Private = struct

    (* Internal data *)

    module Data = struct

      (* Path representation *)

      type segment = P.segment
      type path = P.t
      external of_path : P.t -> path = "%identity"

      (* Image representation *)

      type tr = I.tr = Move of v2 | Rot of float | Scale of v2 | Matrix of m3

      let tr_to_m3 = function
      | Move v -> M3.move2 v
      | Rot a -> M3.rot2 a
      | Scale s -> M3.scale2 s
      | Matrix m -> m

      let inv_tr_to_m3 = function
      | Move v -> M3.move2 (V2.neg v)
      | Rot a -> M3.rot2 (-. a)
      | Scale s -> M3.scale2 (V2.v (1. /. V2.x s) (1. /. V2.y s))
      | Matrix m -> M3.inv m

      type blender = I.blender

      type primitive = I.primitive =
        | Const of color
        | Axial of Color.stops * p2 * p2
        | Radial of Color.stops * p2 * p2 * float
        | Raster of box2 * raster

      type glyph_run = I.glyph_run =
        { font : font;
          text : string option;
          o : p2;
          blocks : (bool * (int * int) list);
          advances : v2 list;
          glyphs : glyph list; }

      type image = I.t =
        | Primitive of primitive
        | Cut of P.area * P.t * image
        | Cut_glyphs of P.area * glyph_run * image
        | Blend of I.blender * float option * image * image
        | Tr of tr * image

      external of_image : I.t -> image = "%identity"
    end

    (* Font helpers *)

    module Font = struct
      let css_slant font = Font.slant_to_str font.Font.slant
      let css_weight font = Font.weight_to_str font.Font.weight
      let css_font ~unit font =
        let slant = css_slant font in
        let weight = css_weight font in
        Printf.sprintf "%s %s %g%s \"%s\"" slant weight font.Font.size unit
          font.Font.name

    end

    (* Path helpers *)

    module P = struct
      external of_data : Data.path -> P.t = "%identity"
      let earc_params = P.earc_params
      let miter_limit o =
        let angle = Float.clamp ~min:0. ~max:Float.pi o.P.miter_angle in
        let angle = if angle = 0. then Float.rad_of_deg 0.05 else angle in
        1. /. sin (angle /. 2.)
    end

    (* Image helpers *)

    module I = struct
      external of_data : Data.image -> I.t = "%identity"
    end

    (* Renderers *)

    type renderer = t

    type k = renderer -> [ `Ok | `Partial ]
    type render_fun = [`End | `Image of size2 * box2 * Data.image ] -> k -> k
    type 'a render_target = renderer -> 'a -> bool * render_fun
    constraint 'a = [< dst]

    let renderer r = r
    let create_target t = t
    let limit r = r.limit
    let warn r w = r.warn w
    let partial = partial
    let o_rem = Manual.dst_rem
    let flush k r =               (* get free space in [r.o] and [k]ontinue. *)
      match r.dst with
      | `Manual -> partial k r
      | `Buffer b -> Buffer.add_subbytes b r.o 0 r.o_pos; r.o_pos <- 0; k r
      | `Channel oc -> output oc r.o 0 r.o_pos; r.o_pos <- 0; k r
      | `Other -> assert false

    let rec writeb b k r =                 (* write byte [b] and [k]ontinue. *)
      if r.o_pos > r.o_max then flush (writeb b k) r else
      (unsafe_set_byte r.o r.o_pos b; r.o_pos <- r.o_pos + 1; k r)

    let rec writes s j l k r =  (* write [l] bytes from [s] starting at [j]. *)
      let b = Bytes.unsafe_of_string s in
      let rem = o_rem r in
      if rem >= l
      then (unsafe_blit b j r.o r.o_pos l; r.o_pos <- r.o_pos + l; k r)
      else begin
        unsafe_blit b j r.o r.o_pos rem; r.o_pos <- r.o_pos + rem;
        flush (writes s (j + rem) (l - rem) k) r
      end

    let rec writebuf buf j l k r = (* write [l] bytes from [buf] start at [j].*)
      let rem = o_rem r in
      if rem >= l
      then (Buffer.blit buf j r.o r.o_pos l; r.o_pos <- r.o_pos + l; k r)
      else begin
        Buffer.blit buf j r.o r.o_pos rem; r.o_pos <- r.o_pos + rem;
        flush (writebuf buf (j + rem) (l - rem) k) r
      end

    let add_xml_data = add_xml_data
  end
end

type renderer = Vgr.t

(*---------------------------------------------------------------------------
   Copyright (c) 2013 The vg programmers

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
