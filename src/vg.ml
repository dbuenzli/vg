(*---------------------------------------------------------------------------
   Copyright (c) %%COPYRIGHT%%. All rights reserved.
   Distributed under a BSD3 license, see license at the end of the file.
   %%PROJECTNAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

open Gg;;

let ( >> ) v f = f v
let ( & ) f v = f v 

module P = struct
  let err_close = "Close can only appear at the end of a subpath"
  let err_empty = "empty path"

  type segment = 
    | Start of p2
    | Line of p2
    | Qcurve of p2 * p2 
    | Ccurve of p2 * p2 * p2 
    | Earc of bool * bool * size2 * float * p2
    | Close
	
  type subpath = segment list
	(* Invariants: Start always the last element and doesn't appear
	   somewhere else. Closure can only appear as the first element. *)
         
  type t = subpath list 
	
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
      if dt > 0. && cw then -. V1._2pi else
      if dt < 0. && not cw then V1._2pi else
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
	  let ax = V2.v (cosa *. rx) (sina *. rx) in 
	  let ay = V2.v (-. sina *. ry) (cosa *. ry) in 
	  let ax' = (V2.of_v3 (M3.tr m (V3.of_v2 ax ~z:0.))) in
	  let ay' = (V2.of_v3 (M3.tr m (V3.of_v2 ay ~z:0.)) )in
	  let a' = atan2 (V2.y ax') (V2.x ax') in 
	  let rx' = V2.norm ax' in
	  let ry' = V2.norm ay' in
	  earc ~large ~cw (V2.v rx' ry') a' (M3.tr2 m pt) p
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
		let b = V2.add c (M2.tr m (V2.v (cos t) (sin t))) in
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
		let b = V2.add c (M2.tr m (V2.v (cos t) (sin t))) in
		update b;  update pt'; csub l
	    end
	| Close :: l -> csub l
	| [] -> ()
      in
      if control then List.iter csub p else List.iter sub p;
      Box2.v (P2.v !xmin !ymin) (Size2.v (!xmax -. !xmin) (!ymax -. !ymin))
	
  let circle ?rel c r p =
    let cx = P2.x c in
    let cy = P2.y c in
    let half = P2.v (cx -. r) cy in 
    let half' = P2.v (cx +. r) cy in
    let radii = V2.v r r in
    p >> start ?rel half >> earc radii 0. half' >> earc radii 0. half >> close
      
  let ellipse ?rel c radii p = 
    let cx = P2.x c in
    let cy = P2.y c in
    let rx = V2.x radii in
    let h = P2.v (cx -. rx) cy in
    let h' = P2.v (cx +. rx) cy in
    p >> start ?rel h >> earc radii 0. h' >> earc radii 0. h >> close
      
  let rect ?rel r p = 
    let o = Box2.o r in
    let size = Box2.size r in
    let w = Size2.w size in 
    let h = Size2.h size in
    let x = P2.x o in
    let y = P2.y o in 
    let rx = w +. x in
    let ty = h +. y in
    p >> start ?rel o >> 
    line (P2.v rx y) >> line (P2.v rx ty) >> line (P2.v x ty) >> close
      
  let rrect ?rel r radii p = 
    let o = Box2.o r in
    let size = Box2.size r in
    let w = Size2.w size in 
    let h = Size2.h size in 
    let rx = V2.x radii in
    let ry = V2.y radii in
    let xmin = P2.x o in
    let xmax = xmin +. w in
    let x1 = xmin +. rx in
    let x2 = xmax -. rx in
    let ymin = P2.y o in 
    let ymax = ymin +. h in
    let y1 = ymin +. ry in
    let y2 = ymax -. ry  in
    p >> 
    start ?rel (P2.v xmin y1) >>
    earc radii 0. (P2.v x1 ymin) >>
    line (P2.v x2 ymin) >>
    earc radii 0. (P2.v xmax y1) >>
    line (P2.v xmax y2) >>
    earc radii 0. (P2.v x2 ymax) >>
    line (P2.v x1 ymax) >>
    earc radii 0. (P2.v xmin y2) >>
    close

  let qlinear tol line acc p0 p1 p2 = 
    let tol = 16. *. tol *. tol in 
    let rec aux tol line acc p0 p1 p2 = 
      let is_flat =                           (* adapted from the cubic case. *)
	let ux = 2. *. P2.x p1 -. P2.x p0 -. P2.x p2 in 
	let uy = 2. *. P2.y p1 -. P2.y p0 -. P2.y p2 in
	let ux = ux *. ux in
	let uy = uy *. uy in
	ux +. uy <= tol
      in
      if is_flat then line p2 acc else
      let p01 = V2.mid p0 p1 in
      let p12 = V2.mid p1 p2 in
      let p012 = V2.mid p01 p12 in
      aux tol line (aux tol line acc p0 p01 p012) p012 p12 p2
    in
    aux tol line acc p0 p1 p2

  let rec clinear tol line acc p0 p1 p2 p3 =
    let tol = 16. *. tol *. tol in 
    let rec aux tol line acc p0 p1 p2 p3 = 
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
      if is_flat then line p3 acc else    
      let p01 = V2.mid p0 p1 in 
      let p12 = V2.mid p1 p2 in 
      let p23 = V2.mid p2 p3 in
      let p012 = V2.mid p01 p12 in
      let p123 = V2.mid p12 p23 in
      let p0123 = V2.mid p012 p123 in    
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
	  let b = V2.add c (M2.tr m (V2.v (cos t) (sin t))) in
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
    let acc, _, _ = fold lin (acc, P2.o, P2.o) p in
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
    let _, _, acc = linearize ?tol ~start:s ~line:l (P2.o, 0., acc) p in
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
	  M2.v (-. (M2.el m 0 0)) (M2.el m 1 0)(* gives the tngt to a point *)
	       (-. (M2.el m 0 1)) (M2.el m 1 1)
	in
	let tol = tol /. max (V2.x r) (V2.y r) in
	let rec aux tol cubic acc p0 t0 p1 t1 = 
	  let dt = t1 -. t0 in
	  let a = 0.25 *. dt in
	  let is_flat = (2.*. (sin a) ** 6.) /. (27.*. (cos a) ** 2.) <= tol in
	  if is_flat then 
	    let l = (4. *. tan a) /. 3. in
	    let c = V2.add p0 (V2.smul l (M2.tr mt (V2.v (sin t0) (cos t0)))) in
	    let c' = V2.sub p1 (V2.smul l (M2.tr mt (V2.v (sin t1) (cos t1)))) 
	    in
	    cubic c c' p1 acc
	  else
	    let t = (t0 +. t1) /. 2. in
	    let b = V2.add c (M2.tr m (V2.v (cos t) (sin t))) in
	    aux tol cubic (aux tol cubic acc p0 t0 b t) b t p1 t1
      in
      aux tol cubic acc p0 t0 p1 t1

  let print fmt p =                              (* TODO rewrite with iter *)
    let pr = Format.fprintf in
    let pr_seg fmt = function
      | Start pt -> 
	  pr fmt "@ @[<1>S@ %a@]" V2.print pt
      | Line pt -> 
	  pr fmt "@ @[<1>L@ %a@]" V2.print pt
      | Qcurve (c, pt) ->
	  pr fmt "@ @[<1>Qc@ %a@ %a@]" V2.print c V2.print pt
      | Ccurve (c, c', pt) -> 
	  pr fmt "@ @[<1>Cc@ %a@ %a@ %a@]" V2.print c V2.print c' V2.print pt
      | Earc (large, cw, radii, angle, pt) ->
	  pr fmt "@ @[<1>Ea@ %B@ %B@ %a@ %F %a@]" large cw V2.print radii angle
	    V2.print pt
      | Close -> pr fmt "@ Close"
    in
    let pr_segs fmt segs = List.iter (pr_seg fmt) (List.rev segs) in
    let pr_sub fmt segs = 
      pr fmt "@ @[<1>(sub@ %a)@]" pr_segs segs
    in
    let pr_subs fmt p = List.iter (pr_sub fmt) (List.rev p) in
    pr fmt "@[<1>(path%a)@]" pr_subs p 
    
end

type path = P.t


module I = struct
  type tr = Move of v2 | Rot of float | Scale of v2 | Shear of v2 | Matrix of m3
  type cap = [ `Butt | `Round | `Square ]
  type join = [ `Miter | `Round | `Bevel ]
  type dashes = float * float list
  type outline = 
      { width : float;
	cap : cap;
	join : join;
	miter_angle : float;
	dashes : dashes option }

  type area_rule = [ `Aeo | `Anz | `Ol of outline ]
  type blender = Over 

  type prim = 
    | Mono of color
    | Axial of Color.stops * p2 * p2
    | Radial of Color.stops * p2 * p2 * float
    | Raster of box2 * Raster.t
    
  type t = 
    | Prim of prim
    | Cut of area_rule * t * path
    | Blend of blender * float option * t * t
    | Tr of tr list * t

  let mono c = Prim (Mono c) 
  let void = mono Color.void 
  let axial stops p p' = Prim (Axial (stops, p, p'))
  let radial stops ~f c r = Prim (Radial (stops, f, c, r))
  let raster b r = Prim (Raster (b, r))

  let ol = 
    { width = 1.; cap = `Butt; join = `Miter; miter_angle = 0.; dashes = None }

  let cut ar i pa = Cut (ar, i, pa)  
  type glyph = int * v2
  type text = string * (int * int) list * bool (* reverse *)
  let cut_glyphs ?text ar gl i = failwith "unimplemented"

  let blend ?a i i' = Blend (Over, a, i, i')

  let push_tr tr = function (* collects sucessive transforms in a single Tr. *)
    | Tr (tl, i) -> Tr (tr :: tl, i)
    | i -> Tr ([tr], i) 

  let move v i = push_tr (Move v) i 
  let rot a i = push_tr (Rot a) i
  let scale s i = push_tr (Scale s) i 
  let shear s i = push_tr (Shear s) i
  let tr m i = push_tr (Matrix m) i

  let compare i i' = Pervasives.compare i i' 
  let equal i i' = i = i' 
  let print fmt i = failwith "unimplemented"
  let to_string i = failwith "unimplemented"
end

type image = I.t


module Vgo = struct
  type surface = size2 * box2  * image
  type meta_data
  type dest = [ 
    | `Buffer of Buffer.t | `Channel of Pervasives.out_channel 
    | `Fun of int -> unit ]
  type format = [ `PDF | `SVG_1_2 ]
  type output       
  let make_output d = failwith "unimplemented"
  module Render = struct
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
