(*----------------------------------------------------------------------------
   Copyright (c) %%COPYRIGHTYEAR%%, Daniel C. Bünzli. All rights reserved.     
   Distributed under a BSD license, see ../LICENSE.                            
  ----------------------------------------------------------------------------*)
open Gg2;;

let ( >> ) v f = f v
let ( & ) f v = f v 

let str = Printf.sprintf 

module P = struct
  let err_close = "Close can only appear at the end of a subpath"
  let err_empty = "empty path"
    
  type segment = 
    | Start of pt
    | Line of pt
    | Qcurve of pt * pt 
    | Ccurve of pt * pt * pt 
    | Earc of bool * bool * size * float * v2
    | Close
	
  type subpath = segment list
  (* Invariants: Start always the last element and doesn't appear
     somewhere else. Closure can only appear as the first element. *)
     
  type path = subpath list
  type t = path
	
  let empty = []
  let is_empty = function [] -> true | _ -> false 

  let path p = List.rev_map List.rev p

  let fold seg v p = 
    let subp v segs = List.fold_left seg v segs in
    List.fold_left subp v (path p)

  let iter seg p = List.iter (fun segs -> List.iter seg segs) (path p)

  let rec subpath_start l = match l with
  | Start pt :: [] -> pt
  | _ :: l -> subpath_start l
  | _ -> assert false

  let last_pt = function
    | [] -> raise Not_found
    | (Start pt :: []) :: _ 
    | (Line pt :: _) :: _
    | (Ccurve (_, _, pt) :: _) :: _
    | (Qcurve (_, pt) :: _) :: _
    | (Earc (_, _, _, _, pt) :: _) :: _ -> pt
    | (Close :: l) :: _ -> subpath_start l
    | _ -> assert false 
	  
  let absol p pt = try V2.add (last_pt p) pt with Not_found -> pt 
      
  let start ?(rel = false) pt p = 
    let pt = if rel then absol p pt else pt in 
    (Start pt :: []) :: p
		 
  let push seg = function
    | [] | ((Close :: _) :: _) as p -> (seg :: Start V2.zero :: []) :: p
    | segs :: p -> (seg :: segs) :: p
					       
  let line ?(rel = false) pt p =
    let line = Line (if rel then absol p pt else pt) in 
    push line p
      
  let ccurve ?(rel = false) c c' pt p = 
    let cc = if rel then Ccurve (absol p c, absol p c', absol p pt) 
    else Ccurve (c, c', pt) 
    in
    push cc p
      
  let qcurve ?(rel = false) c pt p =
    let qc = if rel then Qcurve (absol p c, absol p pt) else Qcurve (c, pt) in
    push qc p 
      
  let earc ?(rel = false) ?(large = false) ?(cw = false) radii angle pt p = 
    let ea = Earc (large, cw, radii, angle, if rel then (absol p pt) else pt) in
    push ea p
      
  let close p = match p with
  | [] | (Close :: _) :: _ -> p
  | segs :: p -> (Close :: segs) :: p

  (* Returns the center of the ellipse, a transform matrix mapping
   the unit circle to the ellipse and the angles on the unit circle
   corresponding to the first and last point of the arc. None is returned
   if the parameters do not define a valid arc. 

   The center is found by first transforming the points on the ellipse to
   points on a unit circle (i.e. we rotate by -a and scale by 1/rx 1/ry). *)
  let ellipse_param p0 large cw r a p1 = 
    let rx = V2.x r in let ry = V2.y r in
    let x0 = V2.x p0 in let y0 = V2.y p0 in
    let x1 = V2.x p1 in let y1 = V2.y p1 in
    if V1.is_zero rx || V1.is_zero ry then None else
    let sina = V1.round_zero (sin a) in
    let cosa = V1.round_zero (cos a) in
    let x0' = (cosa *. x0 +. sina *. y0) /. rx in (* transform to unit circle *)
    let y0' = (-. sina *. x0 +. cosa *. y0) /. ry in
    let x1' = (cosa *. x1 +. sina *. y1) /. rx in
    let y1' = (-. sina *. x1 +. cosa *. y1) /. ry in
    let vx = x1' -. x0' in
    let vy = y1' -. y0' in
    let nx = vy in                                        (* normal to p0'p1' *)
    let ny = -. vx in 
    let nn = (nx *. nx) +. (ny *. ny) in
    if V1.is_zero nn then None (* points coincide *) else 
    let d2 = V1.round_zero (1. /. nn -. 0.25) in
    if d2 < 0. then None (* points are too far apart *) else
    let d = sqrt d2 in
    let d = if (large && cw) || (not large && not cw) then -. d else d in
    let cx' = 0.5 *. (x0' +. x1') +. d *. nx  in             (* circle center *)
    let cy' = 0.5 *. (y0' +. y1') +. d *. ny in
    let t0 = atan2 (y0' -. cy') (x0' -. cx') in               (* angle of p0' *)
    let t1 = atan2 (y1' -. cy') (x1' -. cx') in
    let dt = (t1 -. t0) in
    let adjust = 
      if dt > 0. && cw then -. pi2 else
      if dt < 0. && not cw then pi2 else
      0.
    in
    let t1 = t0 +. (dt +. adjust) in                          (* angle of p1' *)
    let e1x = rx *. cosa in 
    let e1y = rx *. sina in
    let e2x = -. ry *. sina in
    let e2y = ry *. cosa in
    let cx = e1x *. cx' +. e2x *. cy' in             (* transform center back *)
    let cy = e1y *. cx' +. e2y *. cy' in 
    let m = m2 e1x e2x 
               e1y e2y
    in
    Some ((v2 cx cy), m, t0, t1)
      

  let append ?(tr = M3.id) p p' = 
    let add p = function
      | Start pt -> start pt p
      | Line pt -> line pt p
      | Qcurve (c, pt) -> qcurve c pt p
      | Ccurve (c, c', pt) -> ccurve c c' pt p
      | Earc (large, cw, r, a, pt) -> earc ~large ~cw r a pt p
      | Close -> close p
    in
    let add_tr m p = function
      | Start pt -> start (M3.tr2 m pt) p
      | Line pt -> line (M3.tr2 m pt) p
      | Qcurve (c, pt) -> qcurve (M3.tr2 m c) (M3.tr2 m pt) p
      | Ccurve (c, c', pt) -> 
	  ccurve (M3.tr2 m c) (M3.tr2 m c') (M3.tr2 m pt) p
      | Earc (large, cw, r, a, pt) -> 
	    (* TODO something about these v3, review Gg,
	       Something faster ??? Try to factorize. *)
	  let sina = sin a in
	  let cosa = cos a in
	  let rx = V2.x r in
	  let ry = V2.y r in
	  let ax = v2 (cosa *. rx) (sina *. rx) in 
	  let ay = v2 (-. sina *. ry) (cosa *. ry) in 
	  let ax' = (V3.to_v2 (M3.tr m (V3.of_v2 ax ~z:0.))) in
	  let ay' = (V3.to_v2 (M3.tr m (V3.of_v2 ay ~z:0.)) )in
	  let a' = atan2 (V2.y ax') (V2.x ax') in 
	  let rx' = V2.norm ax' in
	  let ry' = V2.norm ay' in
	  earc ~large ~cw (v2 rx' ry') a' (M3.tr2 m pt) p
      | Close -> close p
    in
    if tr == M3.id then fold add p' p else fold (add_tr tr) p' p

  let casteljau pt c c' pt' t =
    let b00 = V2.lerp pt c t in
    let b01 = V2.lerp c c' t in
    let b02 = V2.lerp c' pt' t in
    let b10 = V2.lerp b00 b01 t in
    let b11 = V2.lerp b01 b02 t in
    let b = V2.lerp b10 b11 t in
    Format.printf "t:%F %a%!\n" t V2.print b;
    b
    		       
  let bounds ?(control = false) p = match p with
  | [] -> Rect.empty 
  | p -> 
      let xmin = ref max_float in
      let ymin = ref max_float in
      let xmax = ref ~-.max_float in
      let ymax = ref ~-.max_float in
      let update pt = 
	let x = Pt.x pt in
	let y = Pt.y pt in
	if x < !xmin then xmin := x;
	if x > !xmax then xmax := x;
	if y < !ymin then ymin := y;
	if y > !ymax then ymax := y
      in
      let rec csub = function 
        | Start pt :: l -> update pt; csub l
	| Line pt :: l -> update pt; csub l
	| Qcurve (c, pt) :: l -> update c; update pt; csub l
	| Ccurve (c, c', pt) :: l -> update c; update c'; update pt; csub l
	| Earc (large, cw, radii, angle, pt') :: l ->
	    let pt = last_pt [l] in
	    begin match ellipse_param pt large cw radii angle pt' with
	    | None -> update pt'; csub l
	    | Some (c, m, a1, a2) ->
		(* TODO wrong in general. *)
		let t = (a1 +. a2) /. 2. in
		let b = V2.add c (M2.tr m (v2 (cos t) (sin t))) in
		update b;  update pt'; csub l
	    end
	| Close :: l -> csub l
	| [] -> ()
      in
      let sub = function 
        | Start pt :: l -> update pt; csub l
	| Line pt :: l -> update pt; csub l
	| Qcurve (c, pt) :: l -> 
	    (* TODO *)
	    update c; update pt; csub l
	| Ccurve (c, c', pt') :: l -> 
	    let pt = last_pt [ l ] in
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
	      let a = dim pt in 
	      let b = dim c in
	      let cc = dim c' in
	      let d = dim pt' in
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
		  update (casteljau pt c c' pt' (1.0 /. (1.0 +. s))) in
		solve a b cc upd;		
	      else
		let upd s = update (casteljau pt c c' pt' (s /. (1.0 +. s))) in
		solve cc b a upd
	    in
	    update_z V2.x; update_z V2.y; update pt'; csub l
	| Earc (large, cw, radii, angle, pt') :: l ->
	    let pt = last_pt [l] in
	    begin match ellipse_param pt large cw radii angle pt' with
	    | None -> update pt'; csub l
	    | Some (c, m, a1, a2) ->
		(* TODO wrong in general. *)
		let t = (a1 +. a2) /. 2. in
		let b = V2.add c (M2.tr m (v2 (cos t) (sin t))) in
		update b;  update pt'; csub l
	    end
	| Close :: l -> csub l
	| [] -> ()
      in
      if control then List.iter csub p else List.iter sub p;
      rect (pt !xmin !ymin) (size (!xmax -. !xmin) (!ymax -. !ymin))
	
  let circle ?rel c r p =
    let cx = Pt.x c in
    let cy = Pt.y c in
    let half = pt (cx -. r) cy in 
    let half' = pt (cx +. r) cy in
    let radii = v2 r r in
    p >> start ?rel half >> earc radii 0. half' >> earc radii 0. half >> close
      
  let ellipse ?rel c radii p = 
    let cx = Pt.x c in
    let cy = Pt.y c in
    let rx = V2.x radii in
    let h = pt (cx -. rx) cy in
    let h' = pt (cx +. rx) cy in
    p >> start ?rel h >> earc radii 0. h' >> earc radii 0. h >> close
      
  let rect ?rel r p = 
    let o = Rect.o r in
    let size = Rect.size r in
    let w = Size.w size in 
    let h = Size.h size in
    let x = Pt.x o in
    let y = Pt.y o in 
    let rx = w +. x in
    let ty = h +. y in
    p >> start ?rel o >> 
    line (pt rx y) >> line (pt rx ty) >> line (pt x ty) >> close
      
  let rrect ?rel r radii p = 
    let o = Rect.o r in
    let size = Rect.size r in
    let w = Size.w size in 
    let h = Size.h size in 
    let rx = V2.x radii in
    let ry = V2.y radii in
    let xmin = Pt.x o in
    let xmax = xmin +. w in
    let x1 = xmin +. rx in
    let x2 = xmax -. rx in
    let ymin = Pt.y o in 
    let ymax = ymin +. h in
    let y1 = ymin +. ry in
    let y2 = ymax -. ry  in
    p >> 
    start ?rel (pt xmin y1) >>
    earc radii 0. (pt x1 ymin) >>
    line (pt x2 ymin) >>
    earc radii 0. (pt xmax y1) >>
    line (pt xmax y2) >>
    earc radii 0. (pt x2 ymax) >>
    line (pt x1 ymax) >>
    earc radii 0. (pt xmin y2) >>
    close

  let qlinear tol line acc p0 p1 p2 = 
    let tol = 16. *. tol *. tol in 
    let rec aux tol line acc p0 p1 p2 = 
      let is_flat =                           (* adapted from the cubic case. *)
	let ux = 2. *. Pt.x p1 -. Pt.x p0 -. Pt.x p2 in 
	let uy = 2. *. Pt.y p1 -. Pt.y p0 -. Pt.y p2 in
	let ux = ux *. ux in
	let uy = uy *. uy in
	ux +. uy <= tol
      in
      if is_flat then line p2 acc else
      let p01 = Pt.mid p0 p1 in
      let p12 = Pt.mid p1 p2 in
      let p012 = Pt.mid p01 p12 in
      aux tol line (aux tol line acc p0 p01 p012) p012 p12 p2
    in
    aux tol line acc p0 p1 p2

  let rec clinear tol line acc p0 p1 p2 p3 =
    let tol = 16. *. tol *. tol in 
    let rec aux tol line acc p0 p1 p2 p3 = 
      let is_flat = (* cf. Kaspar Fischer according to R. Willocks. *)
	let ux = 3. *. Pt.x p1 -. 2. *. Pt.x p0 -. Pt.x p3 in
	let uy = 3. *. Pt.y p1 -. 2. *. Pt.y p0 -. Pt.y p3 in 
	let ux = ux *. ux in
	let uy = uy *. uy in
	let vx = 3. *. Pt.x p2 -. 2. *. Pt.x p3 -. Pt.x p0 in
	let vy = 3. *. Pt.y p2 -. 2. *. Pt.y p3 -. Pt.y p0 in
	let vx = vx *. vx in
	let vy = vy *. vy in
	let mx = if ux > vx then ux else vx in
	let my = if uy > vy then uy else vy in
	mx +. my <= tol
      in
      if is_flat then line p3 acc else    
      let p01 = Pt.mid p0 p1 in 
      let p12 = Pt.mid p1 p2 in 
      let p23 = Pt.mid p2 p3 in
      let p012 = Pt.mid p01 p12 in
      let p123 = Pt.mid p12 p23 in
      let p0123 = Pt.mid p012 p123 in    
      aux tol line (aux tol line acc p0 p01 p012 p0123) p0123 p123 p23 p3
    in
    aux tol line acc p0 p1 p2 p3
  

  let elinear tol line acc p0 large cw r a p1 = 
    match ellipse_param p0 large cw r a p1 with
    | None -> line p1 acc
    | Some (c, m, t0, t1) -> 
	let tol2 = tol *. tol in
	let rec aux tol line acc p0 t0 p1 t1 = 
	  let t = (t0 +. t1) /. 2. in
	  let b = V2.add c (M2.tr m (v2 (cos t) (sin t))) in
	  let is_flat =                (* cf. Drawing elliptic... L. Maisonbe *)
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
	  if is_flat then line p1 acc else
	  aux tol line (aux tol line acc p0 t0 b t) b t p1 t1 
	in
	aux tol2 line acc p0 t0 p1 t1
	  
  let linearize ?(tol = 1e-3) ~start ~line acc p = (* use refs for context ? *)
    let lin (acc, ss, last) = function 
      | Start pt -> (start pt acc), pt, pt
      | Line pt -> (line pt acc), ss, pt
      | Qcurve (c, pt) ->  (qlinear tol line acc last c pt), ss, pt
      | Ccurve (c, c', pt) -> (clinear tol line acc last c c' pt), ss, pt
      | Earc (large, cw, r, a, pt) -> 
	  (elinear tol line acc last large cw r a pt), ss, pt 
      | Close -> line ss acc, ss, ss
    in
    let acc, _, _ = fold lin (acc, V2.o, V2.o) p in
    acc

  let sample ?tol ~start ~sample period acc p =
    let foi = float_of_int in 
    let s pt (last, residual, acc) = (pt, 0., (start pt acc)) in
    let l pt (last, residual, acc) = 
      let seg_len = V2.norm (V2.sub pt last) in
      let first_pt = period -. residual in
      let to_walk = seg_len -. first_pt in
      let pt_count = int_of_float (to_walk /. period) in
      let residual' = to_walk -. (foi pt_count) *. period in
      let accr = ref acc in
      for i = 0 to pt_count do 
	let t = (first_pt +. (foi i) *. period) /. seg_len in
	accr := sample (V2.lerp last pt t) !accr 
      done;
      (pt, residual', !accr)
    in
    let _, _, acc = linearize ?tol ~start:s ~line:l (V2.o, 0., acc) p in
    acc


  let one_div_3 = 1. /. 3. 
  let two_div_3 = 2. /. 3. 
  let ecubic tol cubic acc p0 large cw r a p1 = (* TODO tailrec *)
    match ellipse_param p0 large cw r a p1 with
    | None -> (* line with a cubic *)
	let c = V2.add (V2.smul two_div_3 p0) (V2.smul one_div_3 p1) in
	let c' = V2.add (V2.smul one_div_3 p0) (V2.smul two_div_3 p1) in
	cubic c c' p1 acc
    | Some (c, m, t0, t1) -> 
	let mt = (* TODO something better *)
	  m2 (-. (M2.el m 0 0)) (M2.el m 1 0) (* gives the tangent to a point *)
	    (-. (M2.el m 0 1)) (M2.el m 1 1)
	in
	let tol = tol /. max (V2.x r) (V2.y r) in
	let rec aux tol cubic acc p0 t0 p1 t1 = 
	  let dt = t1 -. t0 in
	  let a = 0.25 *. dt in
	  let is_flat = (2.*. (sin a) ** 6.) /. (27.*. (cos a) ** 2.) <= tol in
	  if is_flat then 
	    let l = (4. *. tan a) /. 3. in
	    let c = V2.add p0 (V2.smul l (M2.tr mt (v2 (sin t0) (cos t0)))) in
	    let c' = V2.sub p1 (V2.smul l (M2.tr mt (v2 (sin t1) (cos t1)))) in
	    cubic c c' p1 acc
	  else
	    let t = (t0 +. t1) /. 2. in
	    let b = V2.add c (M2.tr m (v2 (cos t) (sin t))) in
	    aux tol cubic (aux tol cubic acc p0 t0 b t) b t p1 t1
      in
      aux tol cubic acc p0 t0 p1 t1

  let print fmt p =                              (* TODO rewrite with iter *)
    let pr = Format.fprintf in
    let pr_seg fmt = function
      | Start pt -> 
	  pr fmt "@ @[<1>S@ %a@]" Pt.print pt
      | Line pt -> 
	  pr fmt "@ @[<1>L@ %a@]" Pt.print pt
      | Qcurve (c, pt) ->
	  pr fmt "@ @[<1>Qc@ %a@ %a@]" Pt.print c Pt.print pt
      | Ccurve (c, c', pt) -> 
	  pr fmt "@ @[<1>Cc@ %a@ %a@ %a@]" Pt.print c Pt.print c' Pt.print pt
      | Earc (large, cw, radii, angle, pt) ->
	  pr fmt "@ @[<1>Ea@ %B@ %B@ %a@ %F %a@]" large cw Pt.print radii angle
	    Pt.print pt
      | Close -> pr fmt "@ Close"
    in
    let pr_segs fmt segs = List.iter (pr_seg fmt) (List.rev segs) in
    let pr_sub fmt segs = 
      pr fmt "@ @[<1>(sub@ %a)@]" pr_segs segs
    in
    let pr_subs fmt p = List.iter (pr_sub fmt) (List.rev p) in
    pr fmt "@[<1>(path%a)@]" pr_subs p 
    
end
    
module I = struct
  type antialias = bool
  type area_rule = [ `Even_odd | `Non_zero ]
  type cap = [ `Butt | `Round | `Square ]
  type cut_part = Area | Outline 
  type dashes = float * float list
  type join = [ `Miter | `Round | `Bevel ]
  type stops = (float * color) list   
  type cut = [ 
    | `Area of area_rule
    | `Cap of cap 
    | `Dashes of dashes 
    | `Join of join 
    | `Miter_limit of float
    | `Width of float ]

  type blender = [ 
    | `Color
    | `Color_burn
    | `Color_dodge
    | `Darken
    | `Difference
    | `Exclusion
    | `Hard_light
    | `Hue
    | `Lighten
    | `Luminosity
    | `Multiply
    | `Normal
    | `Overlay
    | `Saturation
    | `Screen
    | `Soft_light ] 

  type tr = 
    | Transl of v2 | Rot of float | Scale of v2 | Shear of v2 | Affine of m3

  let m3_of_tr = function 
    | Transl v -> M3.transl2 v
    | Rot a -> M3.rot2 a
    | Scale s -> M3.scale2 s
    | Shear s -> M3.shear2 s
    | Affine m -> m

  type param = [ 
    | cut 
    | `Antialias of antialias 
    | `Cloth of t 
    | `Blender of blender
    | `Flatness of float ] 
  and state = param list * tr list 
  and base = 
    | Mono of color 
    | Axial of pt * pt * stops 
    | Radial of pt * pt * float * stops 
    | Pattern of rect * t (* TODO *)
    | Ri of rect * Ri.t
  and t = 
    | State of state * t
    | Base of base
    | Cut of cut_part * P.t
    | Blend of t list


  let push_param p = function
  | State ((pl, tl), i) -> State ((p :: pl, tl), i)                 (* merge *)
  | i -> State (([ p ], []), i)

  let push_tr tr = function
  | State ((pl, tl), i) -> State ((pl, tr :: tl), i)                (* merge *)
  | i -> State (([], [ tr ]), i)

  let default_cloth = Base (Mono (v4 0. 0. 0. 1.))
  let default i = 
    let d = [ `Antialias true; `Flatness 1.0; `Blender `Normal;
	      `Width 1.; `Cap `Butt; `Dashes (0., []); `Join `Miter; 
	      `Miter_limit 10.; `Area `Non_zero; `Cloth default_cloth ]
    in
    List.fold_right push_param d i


  let mono c = Base (Mono c)
  let blank = mono (v4 0. 0. 0. 0.)
    
  let pattern v i = Base (Pattern (v,i))
  let of_ri rect img = Base (Ri (rect,img))
  let axial p p' stops = Base (Axial (p, p', stops))
  let radial ?f c radius stop = 
    let f = match f with Some f -> f | None -> c in
    Base (Radial (f, c, radius, stop))

  let cutter_l cl = function
  | State ((pl, tl), i) -> 
      let pl' = List.rev_append (List.rev (cl :> param list)) pl in
      State ((pl', tl), i)
  | i -> State (((cl :> param list), []), i)

  let cutter c = push_param (c :> param)
  let cloth c = push_param (`Cloth c)
  let acut p = Cut (Area, p) 
  let ocut p = Cut (Outline, p) 

  let blender b = push_param (`Blender b)
  let blend i i' = match i with
  | Blend il -> Blend (i' :: il)
  | _ -> Blend (i' :: i :: [])

  let transl pt = push_tr (Transl pt)
  let rot a = push_tr (Rot a)
  let scale s = push_tr (Scale s)
  let shear v = push_tr (Shear v)
  let affine m = push_tr (Affine m)

  let antialias a = push_param (`Antialias a) 
  let flatness f = push_param (`Flatness f)
      
  let strip_params pl =                     (* Removes useless state params. *)
    let strip acc v = 
      let add param = if List.exists param acc then acc else v :: acc in
      match v with
      | `Antialias _ -> add (function `Antialias _ -> true | _ -> false)
      | `Area _ -> add (function `Area _ -> true | _ -> false)
      | `Blender _ -> add (function `Blender _ -> true | _ -> false)
      | `Cap _ -> add (function `Cap _ -> true | _ -> false)
      | `Cloth _ -> add (function `Cloth _ -> true | _ -> false)
      | `Dashes _ -> add (function `Dashes _ -> true | _ -> false)
      | `Flatness _ -> add (function `Flatness _ -> true | _ -> false)
      | `Join _ -> add (function `Join _ -> true | _ -> false)
      | `Miter_limit _ -> add (function `Miter_limit _ -> true | _ -> false)
      | `Width _ -> add (function `Width _ -> true | _ -> false)
    in
    List.fold_left strip [] (List.rev pl)

  let print fmt i =                            (* TODO not tail-recursive. *) 
    let pr = Format.fprintf in 
    let pr_tr fmt = function
    | Transl v -> pr fmt "@[<1>(transl@ %a)@]" V2.print v
    | Rot r -> pr fmt "@[<1>(rot@ %F)@]" r
    | Scale s -> pr fmt "@[<1>(scale@ %a)@]" V2.print s
    | Shear s -> pr fmt "@[<1>(shear@ %a)@]" V2.print s
    | Affine m -> pr fmt  "@[<1>(affine@ %a)@]" M3.print m
    in
    let pr_stops fmt stops = pr fmt "TODO" in (* TODO *)
    let rec pr_param fmt = function
    | `Antialias a -> pr fmt "antialias@ =@ %B" a
    | `Flatness t -> pr fmt "flatness@ =@ %F" t
    | `Width w -> pr fmt "width@ =@ %F" w
    | `Cap c -> 
	let pr_cap fmt = function
          | `Butt -> pr fmt "butt" 
	  | `Round -> pr fmt "round" 
	    | `Square -> pr fmt "square"
	in
	pr fmt "cap@ =@ %a" pr_cap c
    | `Dashes (phase,dl) -> 
	let pr_dashes fmt dl = List.iter (pr fmt "@ %F") dl in
	pr fmt "phase@ =@ %F dashes = @[<1>[%a]@]" phase pr_dashes dl 
    | `Join j ->
	let pr_join fmt = function
	  | `Miter -> pr fmt "miter"
	  | `Round -> pr fmt "round"
	  | `Bevel -> pr fmt "bevel"
	in
	pr fmt "join@ =@ %a" pr_join j
    | `Miter_limit l -> 
	pr fmt "miter-limit@ =@ %F" l
    | `Area a -> 
	let pr_area fmt = function
	  | `Even_odd -> pr fmt "even-odd"
	  | `Non_zero -> pr fmt "non-zero"
	in
	pr fmt "area@ =@ %a" pr_area a
    | `Cloth i -> pr fmt "cloth@ =@ %a" pr_img i
    | `Blender b -> 
	let pr_blender fmt = function 
	| `Color -> pr fmt "color"
	| `Color_burn -> pr fmt "color-burn"
	| `Color_dodge -> pr fmt "color-dodge"
	| `Darken -> pr fmt "darken"
	| `Difference -> pr fmt "difference"
	| `Exclusion -> pr fmt "exclusion"
	| `Hard_light -> pr fmt "hard-light"
	| `Hue -> pr fmt "hue"
	| `Lighten -> pr fmt "ligthen"
	| `Luminosity -> pr fmt "luminosity"
	| `Multiply -> pr fmt "multiply"
	| `Normal -> pr fmt "normal"
	| `Overlay -> pr fmt "overlay"
	| `Saturation -> pr fmt "saturation"
	| `Screen -> pr fmt "screen"
	| `Soft_light -> pr fmt "soft-light"
	in
	pr fmt "blender@ =@ %a" pr_blender b
    and pr_base fmt = function
      | Mono c -> 
	  pr fmt "mono@ %a" V4.print c
      | Axial (pt, pt', stops) -> 
	  pr fmt "axial@ %a@ %a@ %a" V2.print pt V2.print pt' pr_stops stops
      | Radial (pt, pt', r, stops) -> 
	  pr fmt "radial@ %a@ %a@ %F@ %a" 
	    V2.print pt V2.print pt' r pr_stops stops
      | Pattern (v,i) -> 
	  pr fmt "pattern@ %a@ %a" Rect.print v pr_img i
      | Ri (r,ri) -> pr fmt "TODO" (* TODO *)
    and pr_img fmt = function
      | State ((pl, tl), i) ->
	  let pr_params fmt pl = List.iter (pr_param fmt) pl in	  
	  let pr_trs fmt trl = List.iter (pr_tr fmt) trl in 
	  pr fmt "@[<1>(state@ %a@ %a@ %a)@]"
	    pr_params pl pr_trs tl pr_img i
      | Base b ->
	  pr fmt "@[<1>(base@ %a)@]" pr_base b
      | Cut (part, path) -> 
	  let part = match part with Area -> "area" | Outline -> "outline" in
	  pr fmt "@[<1>(cut@ %s@ %a)@]" part P.print path
      | Blend il -> 
	  let pr_imgs fmt il = List.iter (pr_img fmt) il in
	  pr fmt "@[<1>(blend@ %a)@]" pr_imgs il
    in
    pr fmt "%a" pr_img i 
end

let ( ++ ) i i' = I.blend i i'

exception Unsupported of string
let unsupported s = raise (Unsupported s)

(* output abstraction *)

type output = Buffer.t -> unit
	
let output_of_channel oc = Buffer.output_buffer oc
let output_of_buffer b = Buffer.add_buffer b
let output_of_fun f = f

(* 
let outs f s =                     (* TODO remove *)
  let s = Buffer
  o s 0 (String.length s)
*)
