(*---------------------------------------------------------------------------
   Copyright (c) 2013 The vg programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(* This is used to be in Vg.P, removing it for now. The removal shows
   that this could be provided by a third party library. The only
   Private function is used is the elliptical arc parametrisation
   Vg.Vgr.Private.p.earc_params. *)

open Gg
open Vg

module P : sig

  type linear_fold = [ `Sub of p2 | `Line of p2 | `Close ]
  (** The type for linear folds. *)

  val linear_fold : ?tol:float -> ('a -> linear_fold -> 'a) -> 'a -> path -> 'a
  (** [linear_fold tol f acc p] approximates the subpaths of [p] by a
      sequence of line segments and applies [f] to those with an
      accumulator. Subpaths are traversed in the order they were
      specified, always start with a [`Sub], but may not be
      [`Close]d. The maximal distance between the original path and
      the linear approximation does not exceed [tol] (defaults to
      [1e-3]). *)

  type sampler = [ `Sub of p2 | `Sample of p2 | `Close ]
  (** The type for path samplers. *)

  val sample : ?tol:float -> float -> ('a -> sampler -> 'a) -> 'a -> path -> 'a
  (** [sample tol dt f acc p], samples the subpaths of [p] at every
      distance [dt] on the curve and applies [f] to those with an
      accumulator. Subpaths are traversed in the order they were
      specified, always start with a [`Sub], followed by
      [`Sample]s at every distance [dt] along the curve. If the subpath
      is closed [`Close] is called aswell. [tol] has the same meaning
      as in {!linear_fold}. *)

  val bounds : ?ctrl:bool -> path -> box2
  (** [bounds ctrl p] is an axis-aligned rectangle containing [p]. If
      [ctrl] is [true] (defaults to [false]) control points are also
      included in the rectangle. Returns {!Gg.Box2.empty} if the path
      is [empty].

      {b Warning.} This function computes the bounds of the ideal
      path (without width). Path {!outline}s areas will exceed these
      bounds. *)
end = struct

  (* linear_{qcurve,ccurve,earc} functions are not t.r. but the recursion
     should converge stop rapidly. *)

  type linear_fold = [ `Sub of p2 | `Line of p2 | `Close ]
  type sampler = [ `Sub of p2 | `Sample of p2 | `Close ]

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

  let linear_earc tol line acc p0 large cw a r p1 =
    match Vgr.Private.P.earc_params p0 large cw a r p1 with
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
    | `Earc (l, cw, a, r, pt) -> linear_earc tol line acc last l cw a r pt, pt
    | `Close -> f acc `Close, (* ignored, `Sub or end follows *) last
    in
    fst (P.fold linear (acc, P2.o) p)

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

  (* Needs fixing in certain cases, see comments. *)
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
      | `Earc (large, cw, angle, radii, pt) :: l ->
          let last = last_pt l in
          begin match earc_params last large cw angle radii pt with
          | None -> update pt; seg_ctrl l
          | Some (c, m, a1, a2) ->
              (* Wrong in general. There are many cases to consider.
                 Depending on a1 - a2 < pi or crosses the ellipses axes.
                 Do proper development on paper. *)
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
      | `Qcurve (c, pt) :: l ->
          (* Wrong, compute bound *) update c; update pt; seg l
      | `Ccurve (c, c', pt) :: l ->
          let last = last_pt l in
          let update_z dim = (* Kallay, computing tight bounds *)
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
      | `Earc (large, cw, angle, radii, pt) :: l ->
          let last = last_pt l in
          begin match earc_params last large cw angle radii pt with
          | None -> update pt; seg l
          | Some (c, m, a1, a2) ->
              (* Wrong in general, see above. *)
              let t = (a1 +. a2) /. 2. in
              let b = V2.add c (V2.ltr m (V2.v (cos t) (sin t))) in
              update b;  update pt; seg l
          end
      | `Close :: l -> seg l
      | [] -> ()
      in
      if ctrl then seg_ctrl p else seg p;
      Box2.v (P2.v !xmin !ymin) (Size2.v (!xmax -. !xmin) (!ymax -. !ymin))

end

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
