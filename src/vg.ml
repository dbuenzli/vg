(*---------------------------------------------------------------------------
   Copyright (c) %%COPYRIGHT%%. All rights reserved.
   Distributed under a BSD3 license, see license at the end of the file.
   %%PROJECTNAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

open Gg;;

(* Invalid_arg strings *)

let err_empty = "empty path"

(* A few useful definitions. *)

let pr = Format.fprintf 
let to_string_of_formatter pp v =                        (* NOT thread safe. *)
  Format.fprintf Format.str_formatter "%a" pp v; 
  Format.flush_str_formatter ()

external ( >> ) : 'a -> ('a -> 'b) -> 'b = "%revapply"
let eps = 1e-9

(* Paths *)

module P = struct

  (* Path areas *)

  type cap = [ `Butt | `Round | `Square ]
  type join = [ `Miter | `Round | `Bevel ]
  type dashes = float * float list
  type outline = 
    { width : float; cap : cap; join : join; miter_angle : float;
      dashes : dashes option }

  let o = 
    { width = 1.; cap = `Butt; join = `Miter; miter_angle = 0.; dashes = None }

  type area = [ `Aeo | `Anz | `O of outline ]

  (* Paths *)
  
  type segment = 
    [ `Sub of p2                          (* subpath start, "empty" segment *)
    | `Line of p2
    | `Qcurve of p2 * p2 
    | `Ccurve of p2 * p2 * p2 
    | `Earc of bool * bool * size2 * float * p2
    | `Close ]
	
  type t = segment list
  (* The list is reversed. The following invariants hold. The last
     element of the list is always `Sub. Between any two `Sub there is
     always at least one element different from `Sub. If there's an
     element preceding a `Close it's a `Sub. *)

  let empty = []
  let last_pt = function 
  | [] -> None
  | s :: p -> 
      match s with 
      | `Sub pt | `Line pt | `Qcurve (_, pt) | `Ccurve (_, _, pt) 
      | `Earc (_, _, _, _, pt) -> Some pt 
      | `Close -> 
          let rec find_sub = function
          | `Sub pt :: _ -> pt
          | _ :: p -> find_sub p
          | [] -> assert false
          in
          Some (find_sub p)
            
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

  let earc ?(rel = false) ?(large = false) ?(ccw = false) r angle pt p = 
    let pt = if rel then abs p pt else pt in
    push (`Earc (large, ccw, r, angle, pt)) p
      
  let close p = push `Close p
        
  (* Derived subpaths *)

  let circle ?rel c r p =
    let cx = P2.x c in
    let cy = P2.y c in
    let a0 = P2.v (cx +. r) cy in 
    let api = P2.v (cx +. r) cy in
    let r = V2.v r r in
    p >> sub ?rel a0 >> earc r 0. api >> earc r 0. a0 >> close
    
  let ellipse ?rel c r p = 
    let cx = P2.x c in
    let cy = P2.y c in
    let rx = V2.x r in
    let a0 = P2.v (cx +. rx) cy in
    let api = P2.v (cx -. rx) cy in
    p >> sub ?rel a0 >> earc r 0. api >> earc r 0. api >> close
      
  let rect ?rel r p = 
    if Box2.is_empty r then p else
    let lb = Box2.o r in
    let size = Box2.size r in
    let l = P2.x lb in
    let r = l +. Size2.w size in
    let b = P2.y lb in 
    let t = b +. Size2.h size in
    p >> sub ?rel lb >> 
    line (P2.v r b) >> line (P2.v r t) >> line (P2.v l t) >> close
      
  let rrect ?rel r cr p = 
    if Box2.is_empty r then p else
    let lb = Box2.o r in
    let size = Box2.size r in
    let rx = V2.x cr in
    let ry = V2.y cr in
    let l = P2.x lb in
    let l_inset = l +. rx in 
    let r = l +. Size2.w size in
    let r_inset = r -. rx in 
    let b = P2.y lb in 
    let b_inset = l +. ry in 
    let t = b +. Size2.h size in 
    let t_inset = t -. ry in 
    p >> sub ?rel (P2.v l b_inset) >>
    earc cr 0. (P2.v l_inset b) >> line (P2.v r_inset b) >>
    earc cr 0. (P2.v r b_inset) >> line (P2.v r t_inset) >>
    earc cr 0. (P2.v r_inset t) >> line (P2.v l_inset t) >>
    earc cr 0. (P2.v l t_inset) >> close

  (* Geometry *)

  (* Returns the center of the ellipse, a transform matrix mapping
   the unit circle to the ellipse and the angles on the unit circle
   corresponding to the first and last point of the arc. None is returned
   if the parameters do not define a valid arc. 

   The center is found by first transforming the points on the ellipse to
   points on a unit circle (i.e. we rotate by -a and scale by 1/rx 1/ry). *)
  let ellipse_param p0 large ccw r a p1 = 
    let rx = V2.x r in let ry = V2.y r in
    let x0 = V2.x p0 in let y0 = V2.y p0 in
    let x1 = V2.x p1 in let y1 = V2.y p1 in
    if Float.is_zero ~eps rx || Float.is_zero ~eps ry then None else
    let sina = Float.round_zero ~eps (sin a) in
    let cosa = Float.round_zero ~eps (cos a) in
    let x0' = (cosa *. x0 +. sina *. y0) /. rx in (* transform to unit circle *)
    let y0' = (-. sina *. x0 +. cosa *. y0) /. ry in
    let x1' = (cosa *. x1 +. sina *. y1) /. rx in
    let y1' = (-. sina *. x1 +. cosa *. y1) /. ry in
    let vx = x1' -. x0' in
    let vy = y1' -. y0' in
    let nx = vy in                                        (* normal to p0'p1' *)
    let ny = -. vx in 
    let nn = (nx *. nx) +. (ny *. ny) in
    if Float.is_zero ~eps nn then None (* points coincide *) else 
    let d2 = Float.round_zero ~eps (1. /. nn -. 0.25) in
    if d2 < 0. then None (* points are too far apart *) else
    let d = sqrt d2 in
    let d = if (large && not ccw) || (not large && ccw) then -. d else d in
    let cx' = 0.5 *. (x0' +. x1') +. d *. nx  in             (* circle center *)
    let cy' = 0.5 *. (y0' +. y1') +. d *. ny in
    let t0 = atan2 (y0' -. cy') (x0' -. cx') in               (* angle of p0' *)
    let t1 = atan2 (y1' -. cy') (x1' -. cx') in
    let dt = (t1 -. t0) in
    let adjust = 
      if dt > 0. && not ccw then -. 2. *. Float.pi else
      if dt < 0. && ccw then 2. *. Float.pi else
      0.
    in
    let t1 = t0 +. (dt +. adjust) in                          (* angle of p1' *)
    let e1x = rx *. cosa in 
    let e1y = rx *. sina in
    let e2x = -. ry *. sina in
    let e2y = ry *. cosa in
    let cx = e1x *. cx' +. e2x *. cy' in             (* transform center back *)
    let cy = e1y *. cx' +. e2y *. cy' in 
    let m = M2.v e1x e2x 
                 e1y e2y
    in
    Some ((V2.v cx cy), m, t0, t1)

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
      
  let bounds ?(ctrl = false) = function
  | [] -> Box2.empty
  | p ->
      let xmin = ref max_float in
      let ymin = ref max_float in
      let xmax = ref ~-.max_float in
      let ymax = ref ~-.max_float in
      let update pt = 
	let x = P2.x pt in
        let y = P2.y pt in
	if x < !xmin then xmin := x;
	if x > !xmax then xmax := x;
	if y < !ymin then ymin := y;
	if y > !ymax then ymax := y
      in
      let rec seg_ctrl = function
      | `Sub pt :: l -> update pt; seg_ctrl l
      | `Line pt :: l -> update pt; seg_ctrl l
      | `Qcurve (c, pt) :: l -> update c; update pt; seg_ctrl l
      | `Ccurve (c, c', pt) :: l -> update c; update c'; update pt; seg_ctrl l
      | `Earc (large, ccw, radii, angle, pt) :: l ->
	  let last = last_pt l in
          begin match ellipse_param last large ccw radii angle pt with
          | None -> update pt; seg_ctrl l
          | Some (c, m, a1, a2) ->
              (* TODO wrong in general. *)
	      let t = (a1 +. a2) /. 2. in
              let b = V2.add c (V2.ltr m (V2.v (cos t) (sin t))) in
              update b; update pt; seg_ctrl l
	    end
	| `Close :: l -> seg_ctrl l
	| [] -> ()
      in
      let rec seg = function 
      | `Sub pt :: l -> update pt; seg l
      | `Line pt :: l -> update pt; seg l
      | `Qcurve (c, pt) :: l -> (* TODO *) update c; update pt; seg l
      | `Ccurve (c, c', pt) :: l -> 
	  let last = last_pt l in
          let update_z dim = (* Kallay, computing thight bounds *)
	      let fuzz = 1e-12 in
	      let solve a b c f = 
		let d = b *. b -. a *. c in
		if (d <= 0.) then () else 
		begin
		  let d = sqrt d in
		  let b = -. b in
		  let b = if (b > 0.) then b +. d else b -. d in
		  if (b *. a > 0.) then f (b /. a);
		  let a = d *. c in
		  let b = c *. c *. fuzz in 
		  if (a > b || -. a < -. b) then f (c /. d);
		end
	      in
	      let a = dim last in 
	      let b = dim c in
	      let cc = dim c' in
	      let d = dim pt in
	      if (a < b && b < d) && (a < cc && cc < d) then () else
	      let a = b -. a in
	      let b = cc -. b in
	      let cc = d -. cc in
	      let fa = abs_float a in
	      let fb = abs_float b *. fuzz in
	      let fc = abs_float cc in
	      if (fa < fb && fc < fb) then () else
	      if (fa > fc) then
		let upd s = 
                  update (casteljau last c c' pt (1.0 /. (1.0 +. s))) 
                in
		solve a b cc upd;		
	      else
		let upd s = update (casteljau last c c' pt (s /. (1.0 +. s))) in
		solve cc b a upd
	    in
	    update_z V2.x; update_z V2.y; update pt; seg l
	| `Earc (large, ccw, radii, angle, pt) :: l ->
	    let last = last_pt l in
	    begin match ellipse_param last large ccw radii angle pt with
	    | None -> update pt; seg l
	    | Some (c, m, a1, a2) ->
		(* TODO wrong in general. *)
		let t = (a1 +. a2) /. 2. in
		let b = V2.add c (V2.ltr m (V2.v (cos t) (sin t))) in
		update b;  update pt; seg l
	    end
	| `Close :: l -> seg l
	| [] -> ()
      in
      if ctrl then seg_ctrl p else seg p;
      Box2.v (P2.v !xmin !ymin) (Size2.v (!xmax -. !xmin) (!ymax -. !ymin))

  let tr m p = 
    let tr_seg m = function 
    | `Sub pt -> `Sub (P2.tr m pt)
    | `Line pt -> `Line (P2.tr m pt) 
    | `Qcurve (c, pt) -> `Qcurve (P2.tr m c, P2.tr m pt) 
    | `Ccurve (c, c', pt) -> `Ccurve (P2.tr m c, P2.tr m c', P2.tr m pt)
    | `Earc (l, ccw, r, a, pt) -> (* TODO recheck that *)
	let sina = sin a in
        let cosa = cos a in
        let rx = V2.x r in
        let ry = V2.y r in
        let ax = V2.v (cosa *. rx) (sina *. rx) in 
        let ay = V2.v (-. sina *. ry) (cosa *. ry) in 
	let ax' = V2.tr m ax in
	let ay' = V2.tr m ay in
	let a' = atan2 (V2.y ax') (V2.x ax') in 
	let rx' = V2.norm ax' in
	let ry' = V2.norm ay' in
        `Earc (l, ccw, (V2.v rx' ry'), a', (P2.tr m pt))
    | `Close -> `Close 
    in
    List.rev (List.rev_map (tr_seg m) p)

  (* Traversal *)

  type fold = segment 
  type linear_fold = [ `Sub of p2 | `Line of p2 | `Close ]
  type sampler = [ `Sub of p2 | `Sample of p2 | `Close ]

  let fold ?(rev = false) f acc p = 
    List.fold_left f acc (if rev then p else List.rev p) 

  (* TODO the linear_* curves are not t.r. but doesn't the recursion
     converge rapidly ? *)

  let linear_qcurve tol line acc p0 p1 p2 = 
    let tol = 16. *. tol *. tol in
    let rec loop tol line acc p0 p1 p2 = 
      let is_flat =                          (* adapted from the cubic case. *)
	let ux = 2. *. P2.x p1 -. P2.x p0 -. P2.x p2 in 
        let uy = 2. *. P2.y p1 -. P2.y p0 -. P2.y p2 in
	let ux = ux *. ux in
	let uy = uy *. uy in
	ux +. uy <= tol
      in
      if is_flat then line acc p2 else
      let p01 = P2.mid p0 p1 in
      let p12 = P2.mid p1 p2 in
      let p012 = P2.mid p01 p12 in
      loop tol line (loop tol line acc p0 p01 p012) p012 p12 p2
    in
    loop tol line acc p0 p1 p2

  let rec linear_ccurve tol line acc p0 p1 p2 p3 =   
    let tol = 16. *. tol *. tol in 
    let rec loop tol line acc p0 p1 p2 p3 = 
      let is_flat = (* cf. Kaspar Fischer according to R. Willocks. *)
	let ux = 3. *. P2.x p1 -. 2. *. P2.x p0 -. P2.x p3 in
	let uy = 3. *. P2.y p1 -. 2. *. P2.y p0 -. P2.y p3 in 
	let ux = ux *. ux in
	let uy = uy *. uy in
	let vx = 3. *. P2.x p2 -. 2. *. P2.x p3 -. P2.x p0 in
	let vy = 3. *. P2.y p2 -. 2. *. P2.y p3 -. P2.y p0 in
	let vx = vx *. vx in
	let vy = vy *. vy in
	let mx = if ux > vx then ux else vx in
	let my = if uy > vy then uy else vy in
	mx +. my <= tol
      in
      if is_flat then line acc p3 else    
      let p01 = P2.mid p0 p1 in 
      let p12 = P2.mid p1 p2 in 
      let p23 = P2.mid p2 p3 in
      let p012 = P2.mid p01 p12 in
      let p123 = P2.mid p12 p23 in
      let p0123 = P2.mid p012 p123 in    
      loop tol line (loop tol line acc p0 p01 p012 p0123) p0123 p123 p23 p3
    in
    loop tol line acc p0 p1 p2 p3
  
  let linear_earc tol line acc p0 large ccw r a p1 =
    match ellipse_param p0 large ccw r a p1 with
    | None -> line acc p1
    | Some (c, m, t0, t1) -> 
	let tol2 = tol *. tol in
	let rec loop tol line acc p0 t0 p1 t1 = 
	  let t = (t0 +. t1) /. 2. in
	  let b = V2.add c (V2.ltr m (V2.v (cos t) (sin t))) in
	  let is_flat =               (* cf. Drawing elliptic... L. Maisonbe *)
	    let x0 = V2.x p0 in 
	    let y0 = V2.y p0 in 
	    let px = V2.y p1 -. y0 in
	    let py = -. (V2.x p1 -. x0) in
	    let vx = V2.x b -. x0 in
	    let vy = V2.y b -. y0 in
	    let dot = (px *. vx +. py *. vy) in
	    let d = dot *. dot /. (vx *. vx +. vy *. vy) in
	    d <= tol
	  in
	  if is_flat then line acc p1 else
	  loop tol line (loop tol line acc p0 t0 b t) b t p1 t1 
	in
	loop tol2 line acc p0 t0 p1 t1
	  
  let linear_fold ?(tol = 1e-3) f acc p =
    let line acc pt = f acc (`Line pt) in
    let linear (acc, last) = function 
    | `Sub pt -> f acc (`Sub pt), pt
    | `Line pt -> line acc pt, pt
    | `Qcurve (c, pt) ->  linear_qcurve tol line acc last c pt, pt
    | `Ccurve (c, c', pt) -> linear_ccurve tol line acc last c c' pt, pt
    | `Earc (l, ccw, r, a, pt) -> linear_earc tol line acc last l ccw r a pt, pt
    | `Close -> f acc `Close, (* ignored, `Sub or end follows *) last 
    in
    fst (fold linear (acc, P2.o) p)

  let sample ?tol period f acc p =
    let sample (acc, last, residual) = function
    | `Sub pt -> f acc (`Sub pt), pt, 0.
    | `Line pt ->
        let seg_len = V2.(norm (pt - last)) in
        let first_pt = period -. residual in
        let to_walk = seg_len -. first_pt in
        let pt_count = int_of_float (to_walk /. period) in
        let residual' = to_walk -. (float pt_count) *. period in
        let acc = ref acc in
        for i = 0 to pt_count do 
	  let t = (first_pt +. (float i) *. period) /. seg_len in
          acc := f !acc (`Sample (V2.mix last pt t))
        done;
        (!acc, pt, residual')
    | `Close -> f acc `Close, (* ignored `Sub or end follows *) last, 0. 
    in
    let acc, _, _ = linear_fold ?tol sample (acc, P2.o, 0.) p in
    acc

  (* TODO This is needed by the PDF backend to approximate elliptical arcs. 
     To we add something like Vg.P.cubic_fold or just move that to 
     the pdf backend ? *)
  
  let one_div_3 = 1. /. 3. 
  let two_div_3 = 2. /. 3. 
  let cubic_earc tol cubic acc p0 large ccw r a p1 = (* TODO tailrec *)
    match ellipse_param p0 large ccw r a p1 with
    | None -> (* line with a cubic *)
	let c = V2.add (V2.smul two_div_3 p0) (V2.smul one_div_3 p1) in
        let c' = V2.add (V2.smul one_div_3 p0) (V2.smul two_div_3 p1) in
        cubic c c' p1 acc
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
	    let c = V2.add p0 (V2.smul l (V2.ltr mt (V2.v (sin t0) (cos t0)))) 
            in
	    let c' = V2.sub p1 (V2.smul l (V2.ltr mt (V2.v (sin t1) (cos t1))))
            in
	    cubic c c' p1 acc
	  else
	    let t = (t0 +. t1) /. 2. in
	    let b = V2.(c + ltr m (V2.v (cos t) (sin t))) in
	    loop tol cubic (loop tol cubic acc p0 t0 b t) b t p1 t1
      in
      loop tol cubic acc p0 t0 p1 t1

  (* Predicates and comparisons *)

  let is_empty = function [] -> true | _ -> false 
  let equal p p' = p = p' 
  let rec equal_f eq p p' = 
    let equal_seg eq s s' = match s, s' with 
    | `Sub pt, `Sub pt' | `Line pt, `Line pt' -> 
        V2.equal_f eq pt pt' 
    | `Qcurve (c0, pt), `Qcurve (c0', pt') -> 
        V2.equal_f eq c0 c0' && V2.equal_f eq pt pt'
    | `Ccurve (c0, c1, pt), `Ccurve (c0', c1', pt') -> 
        V2.equal_f eq c0 c0' && V2.equal_f eq c1 c1' && V2.equal_f eq pt pt'
    | `Earc (l, ccw, r, a, pt), `Earc (l', ccw', r', a', pt') ->
        l = l' && ccw = ccw' && V2.equal_f eq r r' && eq a a' && 
        V2.equal_f eq pt pt'
    | `Close, `Close -> true
    | _, _ -> false 
    in
    match p, p' with 
    | s :: p, s' :: p' -> if equal_seg eq s s' then equal_f eq p p' else false
    | [], [] -> true
    | _ -> false

  let compare p p' = Pervasives.compare p p'
  let rec compare_f cmp p p' = 
    let compare_seg cmp s s' = match s, s' with 
    | `Sub pt, `Sub pt' | `Line pt, `Line pt' -> 
        V2.compare_f cmp pt pt' 
    | `Qcurve (c0, pt), `Qcurve (c0', pt') -> 
        let c = V2.compare_f cmp c0 c0' in 
        if c <> 0 then c else V2.compare_f cmp pt pt'
    | `Ccurve (c0, c1, pt), `Ccurve (c0', c1', pt') -> 
        let c = V2.compare_f cmp c0 c0' in 
        if c <> 0 then c else 
        let c = V2.compare_f cmp c1 c1' in 
        if c <> 0 then c else V2.compare_f cmp pt pt'
    | `Earc (l, ccw, r, a, pt), `Earc (l', ccw', r', a', pt') ->
        let c = Pervasives.compare l l' in 
        if c <> 0 then c else
        let c = Pervasives.compare ccw ccw' in 
        if c <> 0 then c else 
        let c = V2.compare_f cmp r r' in 
        if c <> 0 then c else 
        let c = cmp a a' in 
        if c <> 0 then c else V2.compare_f cmp pt pt'
    | s, s' -> Pervasives.compare s s'
    in
    match p, p' with 
    | s :: p, s' :: p' ->
        let c = compare_seg cmp s s' in 
        if c <> 0 then c else compare_f cmp p p' 
    | p, p' -> Pervasives.compare p p'
                 
  (* Printers *)

  let pp_seg pp_f pp_v2 ppf = function
  | `Sub pt -> 
      pr ppf "@ @[<1>S@ %a@]" pp_v2 pt 
  | `Line pt -> 
      pr ppf "@ @[<1>L@ %a@]" pp_v2 pt
  | `Qcurve (c, pt) -> 
      pr ppf "@ @[<1>Qc@ %a@ %a@]" pp_v2 c pp_v2 pt
  | `Ccurve (c, c', pt) -> 
      pr ppf "@ @[<1>Cc@ %a@ %a@ %a@]" pp_v2 c pp_v2 c' pp_v2 pt
  | `Earc (l, ccw, r, a, pt) -> 
      pr ppf "@ @[<1>Ea@ %B@ %B@ %a@ %a@ %a@]" l ccw pp_v2 r pp_f a pp_v2 pt
  | `Close ->
      pr ppf "@ C"
               
  let pp_path pp_f pp_v2 ppf p = 
    let pp_segs ppf ss = List.iter (pp_seg pp_f pp_v2 ppf) ss in 
    pr ppf "@[<1><path %a>@]" pp_segs (List.rev p)

  let pp_f pp_f ppf p = pp_path pp_f (V2.pp_f pp_f) ppf p    
  let pp ppf p =
    let pp_f ppf f = pr ppf "%g" f in
    pp_path pp_f V2.pp ppf p     

  let to_string p = to_string_of_formatter pp p 
end

(* Images *)

module I = struct
  
  type tr = Move of v2 | Rot of float | Scale of v2 | Shear of v2 | Matrix of m3
  type blender = [ `Atop | `In | `Out | `Over | `Plus | `Copy | `Xor ]

  type prim = 
    | Mono of color
    | Axial of Color.stops * p2 * p2
    | Radial of Color.stops * p2 * p2 * float
    | Raster of box2 * Raster.t
    
  type t = 
    | Prim of prim
    | Cut of P.area * t * P.t
    | Blend of blender * float option * t * t
    | Tr of tr list * t

  (* Primitive images. *)

  let mono c = Prim (Mono c) 
  let void = mono Color.void 
  let axial stops p p' = Prim (Axial (stops, p, p'))
  let radial stops ?f c r = 
    let f = match f with None -> c | Some f -> f in
    Prim (Radial (stops, f, c, r))
  let raster b r = Prim (Raster (b, r))

  (* Cutting images. *)

  let cut ?(area = `Anz) p i = Cut (area, i, p)  
  type glyph = int * v2
  type text = string * (int * int) list * bool (* reverse *)
  let cut_glyphs ?text ar gl i = failwith "unimplemented"

  let blend ?a ?(blender = `Over) i i' = Blend (blender, a, i, i')

  let push_tr tr = function (* collects sucessive transforms in a single Tr. *)
    | Tr (tl, i) -> Tr (tr :: tl, i)
    | i -> Tr ([tr], i) 

  let move v i = push_tr (Move v) i 
  let rot a i = push_tr (Rot a) i
  let scale s i = push_tr (Scale s) i 
  let shear s i = push_tr (Shear s) i
  let tr m i = push_tr (Matrix m) i

  let is_void i = i == void 
  let equal i i' = i = i' 
  let equal_f i i' = failwith "TODO"
  let compare i i' = Pervasives.compare i i' 
  let compare_f cmp i i' = failwith "TODO"
  let pp fmt i = failwith "TODO"
  let pp_f fmt i = failwith "TODO"
  let to_string i = failwith "TODO"
end

type path = P.t
type image = I.t


module Vgr = struct
  type surface = size2 * box2  * image
  type page = size2 * box2 * image
  type meta_data
  type dest = [ 
    | `Buffer of Buffer.t | `Channel of Pervasives.out_channel 
    | `Fun of int -> unit ]
  type format = [ `PDF | `SVG_1_2 ]
  type svg_warning = [ `Blend ]
  type html5_warning = [ `Dashes | `Aeo ]
  type output       
  let make_output d = failwith "TODO"
  let output_html5 a = failwith "TODO"
  let output_pdf a = failwith "TODO"
  let output_svg a = failwith "TODO"
  let create_output o = failwith "TODO"
  let compact_warnings o = failwith "TODO"
      
  module Meta = struct
    type t
  end
  module Backend = struct
  end
end

(*---------------------------------------------------------------------------
   Copyright %%COPYRIGHT%%
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

   3. Neither the name of the Daniel C. Bünzli nor the names of
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
