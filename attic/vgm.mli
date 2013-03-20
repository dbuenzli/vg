(*----------------------------------------------------------------------------
   Copyright (c) %%COPYRIGHTYEAR%%, Daniel C. Bünzli. All rights reserved.     
   Distributed under a BSD license, see ../LICENSE.                            
  ----------------------------------------------------------------------------*)

(** Applicative 2D vector graphics.

    Vgm is a module for applicative 2D vector graphics with output to
    multiple rendering {{:html.Vgm_out}backends}.

    Consut the {{:#semantics}semantics}, the {{:#basics}basics} and
    {{#examples}examples}. 

    Open the module to use it. It defines only a few toplevel types,
    constructors and composition functions. 

    {e Version %%VERSION%% - %%EMAIL%%}

    {1:top  } *)

open Cgm;;

type pt = v2
type size = v2
type rect = v4
module Ri : sig type t = unit end

(** {1 Vector graphics} *)

type path 
(** The type for paths. *)

type image 
(** The type for images. *)

(** 2D paths.

    A path is a set of subpaths. Subpaths can be disconnected. Each
    subpath starts at a point and is made of a list of directed
    straight or curved segments. A path implicitly defines an area
    which can be concave, convex, and self-intersecting. The way the
    area is defined depends on an {{:Vg.I.html#TYPEarea_rule} area
    determination rule}.

    Path contructors always take the path as the last argument, it allows
    to use the operator {!Vg.(>>)} to build paths from the empty path.

    In the functions below, if the optional argument [rel] is [true]
    point coordinates given to the function are expressed relative to
    the {{:#VALlast_pt}last point} of the path or [(0,0)] if there is no
    such point. If a segment is added on an empty path without
    a previous {!start}, an implicit start of [(0,0)] is issued.

*) 
module P : sig

  (** {1 Paths} *)

  type t = path
  (** The type for paths. *)

  val empty : path
  (** The empty path. *)

  val is_empty : path -> bool
  (** [true] if the path is empty. *)

  val last_pt : path -> pt
  (** The last point. Raises [Not_found]
      on empty paths. *)

  val append : ?tr:m3 -> path -> path -> path
    (** [append ~tr p p'] appends [p] transformed by [tr] to [p']. [tr] defaults
     to the identity.*)

  val start : ?rel:bool -> pt -> path -> path
  (** [start pt p], starts a new subpath at [pt]. *)
 
  val line : ?rel:bool -> pt -> path -> path
  (** [line pt p], straight line from the last point to [pt]. *)

  val qcurve : ?rel:bool -> pt -> pt -> path -> path
  (** [qcurve c pt p], quadratic bézier curve from the last point
      to [pt] with control point [c]. *)

  val ccurve : ?rel:bool -> pt -> pt -> pt -> path -> path
  (** [ccurve c c' pt], quadratic bézier curve from the last point
      to [pt] with control points [c] and [c']. *)

  val earc : ?rel:bool -> ?large:bool -> ?cw:bool -> size -> float -> pt -> 
    path -> path
  (** [earc radii angle pt p], elliptical arc from the last point to
      [pt].  The ellipse is defined by the given horizontal and
      vertical [radii] which are rotated by the given [angle] with
      respect to the current coordinate system. In the general case
      this defines four possible arcs, thus [large] indicates if more
      than pi radians of the arc is to be traversed and [cw] if the
      arc is to be traversed in the clockwise direction (both default
      to [false]). See
      {{:http://www.w3.org/TR/2000/CR-SVG-20001102/images/paths/arcs02.png}this
      figure}. If parameters do not define a valid ellipse (coincident or 
      too far apart points, zero radius) the arc collapses to a line. *)

  val close : path -> path
  (** Straight line from the last point to the subpath's starting
      point and ends the subpath.  *)

  val bounds : ?control:bool -> path -> rect
  (** Axis-aligned rectangle containing the path. If [control]
      is true control points are also included in the rectangle.
      Beware
      that the outline of a path, depending on its {{:Vg.I.html#TYPEcut}width} 
      may exceed these bounds. Raises [Invalid_arg] on empty paths.  *)
 
  val print : Format.formatter -> path -> unit
      (** Prints the path. This is for debugging purposes, do not 
	  use as a serialization format. *)

  (** {1 Subpaths} 

      The following functions add and close a new subpath to the given path. *)

  val circle : ?rel:bool -> pt -> float -> path -> path
  (** [circle c r p], circle with center [c] and radius [r]. *)

  val ellipse : ?rel:bool -> pt -> size -> path -> path
  (** [ellipse c radii p], axis-aligned ellipse with center [c] and
      given [radii].*)

  val rect : ?rel:bool -> rect -> path -> path
  (** [rect r], axis-aligned rectangle. *)

  val rrect :?rel:bool -> rect -> size -> path -> path
  (** [rrect r radii], axis-aligned rectangle 
      [r] with round corner with given [radii]. *)

  (** {1 Accessing path data} *)

  type segment = 
    | Start of pt
    (** Starting point of a subpath (empty segment). *)
    | Line of pt               
    (** Line to point. *)
    | Qcurve of pt * pt  
    (** Quadratic curve to point, a control point and the point *)
    | Ccurve of pt * pt * pt   
    (** Cubic curve to point, two control points and the point *)
    | Earc of bool * bool * size * float * v2
    (** Elliptic arc to point, large, cw, raddii, angle and the point *)
    | Close
    (** Line to [Start] and ends the subpath. *)

  val fold : ('a -> segment -> 'a) -> 'a -> path -> 'a
  (** [fold seg p v]. Applies [seg] to each path segment. 
      Each [Start] denotes the beginning of a new subpath. Note that
      subpaths are not necessarily [Close]d. *)

  val iter : (segment -> unit) -> path -> unit
  (** [iter seg p]. Applies [seg] to each segment.
      Each [Start] denotes the beginning of a new subpath. Note that
      subpaths are not necessarily [Close]d. *)

  val linearize : ?tol:float -> start:(pt -> 'a -> 'a) -> 
    line:(pt -> 'a -> 'a) -> 'a -> path -> 'a
  (** Accesses path data as a sequence of line segments. [start] is
      called at each new subpath and [line] denotes a line segment
      from the last point to the given point.  The maximal distance
      between the original path and the approximation does not exceed
      [tol] (defaults to [1e-3]). *) 

  val sample : ?tol:float -> start:(pt -> 'a -> 'a) -> 
    sample:(pt -> 'a -> 'a) -> float -> 'a -> path -> 'a
  (** [sample start sample interval acc p], invokes [start] at each subpath 
      start and then [sample] at every distance [interval] on the curve. TODO
   better doc.*)
end



(** Scalable images. 

    {b Note.} All length, width, coordinates are always expressed w.r.t. to 
       the environment's transform when the parameter is used.
    {b TODO} Give an example (width scale). *)
module I : sig
  
  (** {1 Basic types} *)
  
  type cap = [ `Butt | `Round | `Square ]
  (** The type for path outline line caps. *)
  
  type join = [ `Miter | `Round | `Bevel ]
  (** The type for path outline line joins. *)
  
  type dashes = float * float list
  (** The type for path outline dashes. Specifies, in user space,
      the phase (starting offset) and the lengths of alternative 'on' and
      'off' dash segments (starting with 'on').

      {b Note.} The maximal size for the list of dashes is highly dependent
      on the chosen backend. 
*)

  type area_rule = [ `Even_odd | `Non_zero ]
  (** The type for path area determination rules. *)

  type cut = [ 
    | `Width of float
    | `Cap of cap 
    | `Dashes of dashes 
    | `Join of join 
    | `Miter_limit of float
    | `Area of area_rule ]
  (** Cutter properties. All properties except [`Area] affect
      the cut of the outline. [`Area] affects the cut
      of the area. *)
	  
  type blender = [ `Color
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
  (** The type for blend modes (correspond to PDF 
      {{:http://www.adobe.com/devnet/pdf/pdfs/blend_modes.pdf} blend modes}). *)

  type stops = (float * color) list 
  (** The type for gradient color stops. A list that pairs an offset in [0..1]
      with a color. Each pair specifies a location and its color 
      on the segment defining the gradient range. *)

  (** {1 Images} *)

  type t = image
  (** The type for scalable images. *)
    
  val default : image -> image 
  (** Sets the default rendering environment for the image. You don't need
      to call this function except if you want to reset the rendering
      state. *)
      

  val print : Format.formatter -> image -> unit
  (** Prints the stream of commands of 
      the image. This is for debugging purposes, do not 
      use as a serialization format. *)

  (** {2 Base images} *)

  val blank : image
  (** Infinite transparent image. *)

  val mono : color -> image
  (** [mono c] is an infinite monochrome image of color [c]. *)

  val axial : pt -> pt -> stops -> image
  (** [axial p p' stops] is an infinite image with a color
      gradient varying between points [p] and [p'] according to the [stops]. 
      Colors extend to the infinite perpendicular to the axis [pp'].
      Colors at [p] and [p'] are used to fill the plane beyond the segment
      region. *)

  val radial : ?f:pt -> pt -> float -> stops -> image
  (** [radial ?f c r stops] is an infinite image with a color gradient
      made of concentric circles centered at [c] up to radius [r]
      according to the stops.  Color at the last stop is used 
      to fill the plane beyond the maximal circle. [f] is the focal
      point and defaults to [c]. *)

  val pattern : rect -> image -> image
  (** [pattern r i] is an infinite image in which
      the region [r] or image [i] is repeated infinitely 
      to tile the plane. *)

  val of_ri : rect -> Ri.t -> image
  (** An image from the given raster image framed in the specified
      rectangle. (need alpha ?, filtering, overlaps the mask notion ?)  *)

  (** {2 Cutting images} *)

  val cutter : cut -> image -> image
  (** Sets a cutting property for the image. *)

  val cutter_l : cut list -> image -> image
  (** Sets a list of cutting property for the image. 
      [cutter cl i] is equivalent to [List.fold_right cutter cl i]. *)

  val cloth : image -> image -> image
  (** Sets the image to cut for the given image. Note paper 
      is an image in itself. {b TODO} global alpha here ?,
      matrix property think about it. think snip ! *)
  
  val acut : path -> image
  (** Cuts out the area defined by the path in the current {!cloth}, 
      according to the current cutter property [`Area]. *)

  val ocut : path -> image
  (** Cuts out the outline defined by the path in the current {!cloth},
      according to the current cutter properties. *)

(** {2 Blending images} *)

  val blender : blender -> image -> image
  (** Sets the blender for the image (defaults to [`Normal]). *)

  val blend : image -> image -> image 
  (** Image resulting from 
      covering the first image with the second one using the current 
      blender. See also {!Vg.(<+>)}. *)

(** {2 Transforming images} *) 

  val move : v2 -> image -> image
  (** Translates the image. *)

  val rot : float -> image -> image
  (** Rotates the image. *)

  val scale : v2 -> image -> image
  (** Scales the image. *)

  val shear : v2 -> image -> image
  (** Shears the image. *)

  val affine : m3 -> image -> image
  (** Transforms the image. *)

(** {2 Rendering parameters} 

    The following parameters can be provided as a hint to renderers. *)

  type antialias = bool
  val antialias : antialias -> image -> image
  val flatness : float -> image -> image
end

(** Output backends. *)
module Vgo : sig

   exception Unsupported of string
  (** May be raised by backends if they do not support a feature. 
      Backends list explictely the features they do not support. *)

   type page = size * rect * I.t
   (** The type for pages. The physical size in meters, the rectangular
       region of the image to render and the image itself. *)

  (** {1 Non interactive output} 

      {b Important note.} Non interactive backends support {!Vg}'s
      drawing model which is mainly dedicated to picture and gui
      drawing and hence restricted in scope. If you want to author documents
      that are able to fully support the features of a file format
      (e.g. pdf outline hierarchy or cross reference links) you should 
      use a dedicated
      library.

      Backends that output files output to the following abstraction. *)

  type output
  (** The type for output abstractions. *)

  val output_of_channel : out_channel -> output
  (** Output abstraction from the given channel. *)

  val output_of_buffer : Buffer.t -> output
  (** Output abstraction from the given buffer. *)
      
  val output_of_fun : (Buffer.t -> unit) -> output
  (** Output abstraction from the given function. The function is
      called repeatedly with a buffer containing bytes to output. *)

  (** {2 PDF backend}
      
      This backend produces 
      {{:http://www.adobe.com/devnet/pdf/pdf_reference.html}PDF} 1.4
      documents. *)

  val output_pdf : ?compress:(string -> string) -> ?title:string -> 
    ?author:string -> ?subject:string ->
    ?keywords:string -> ?creator:string -> ?date:string -> 
      output -> page list -> unit
  (**
     Outputs a rectangular region of the image on a document of
     the given size in meters. Meta-data strings must be [Latin-1] encoded.
     todo UTF-8.
      {ul 
      {- [compress], a callback to compress data using
         the zlib/deflate compression method.}
      {- [title], document's title.}
      {- [author], document's author.}
      {- [subject], document's subject.}
      {- [keywords], keywords describing the document.}
      {- [creator], application creating the document.}
      {- [date] YYYYMMDDHHmmSSOHH'mm}} 
   *)

  (** {2 SVG backend} 

      This backend produces {{:http://www.w3.org/Graphics/SVG/} SVG} 1.1 
      images. 
	
      To avoid dependencies on external libraries, the backend
      doesn't currently support raster images.  However 
      a hook is provided to allow you to write to the image. 

      For now only [`Normal] blend mode. 
      Ignored [`Antialias | `Tolerance | `Blend ]. *)

  val output_svg : ?title:string -> output -> page -> unit
 (** [output oc size view i] outputs the part of the image [i]
    specified by [view] (may be larger than [i]'s extents) to an svg
    image with dimension [size] in meters. The origin of the [view]
    rectangle is mapped to the lower left corner of the svg image and
    its maximal point to the top right one. The [title] argument is
    written in the title element of the svg image. *)
      
  (** {1 Writing backends} *)

  (** Module to write backends. 

      Invariants guaranteed by the constructors.  No two consecutive
      Param however the same rendering parameter may appear twice in
      the list of a param and according to semantics the second one
      should be set. No two consecutive Tr. *) 
module Backend : sig end end

val ( >> ) : 'b -> ('b -> 'a) -> 'a
(** Operator to build paths. Left associative. *)

val ( ++ ) : I.t -> I.t -> I.t
(** Operator to blend images same as {!I.blend}. Left associative. *)

val ( & ) : ('a -> 'b) -> 'a -> 'b
(** Operator to avoid parentheses when specifying rendering environments.
    Left associative.
 *) 

(** {1:basics Basics}

    In the following basic knowledge about vector graphics is assumed.

    {ul
    {- Initially the coordinate system has its origin 
       at the bottom left the y-axis points up and the x-axis on the right.}
    {- Angles are always given in radians.}
    {- This is a rendering library there is no provision to query 
       the current rendering state. Your higher level structures
       should handle that.}}  

  {2:model Rendering model} 

  Basic knowledge about vector graphics is assumed. 

  Most 2D vector libraries are designed around the basic notions of
  path strokes and fills. Vgm uses the notion of cuts introduced by
  {{:http://haskell.org/haven}Haven} which unifies fills and
  strokes. These different notions are as expressive however cutting
  is clearer from a specification point of view (e.g. for stroking
  self-intersecting paths).

  Conceptually an image [i] is seen as a function [i : pt -> color]
  from 2D points to colors. Vg allows you to define and compose images 
  into a resulting infinite image of which a finite part is eventually
  rendered. The simplest base image is an infinite monochrome image
  that associates to any point in the plane
  a given color. For example this is an infinite red image (the figure
  obviously shows only a finite part of it):
[TODO image] 
{[let red = I.mono (color 1. 0. 0. 1.)]}
  An image with a red circle is an infinite image which associates
  the color red to the points located in the interior of the circle and
  a transparent white color outside. Vg allows you to create such 
  an image by taking the infinite red image and cut out the interior 
  of the circle as if you were cutting in large piece of red cloth.
  This is performed as follows :
[TODO image]
{[let circle r = P.empty >> P.circle (pt 0. 0.) r
let red_c = I.cloth red & I.acut (circle 2.)]}
  To create an image in which a circle is outlined with a black stroke of
  width of 0.1, we (conceptually) create the area that corresponding to 
  the outline and cut it out from a black infinite image.
  [TODO image]
{[let black_oc = 
    I.cloth black & I.cutter (`Width 0.1) & I.ocut (circle 1.)]}
As you see, instead of having a function that transforms a path to the path of 
its outline and then cut the area defined by this path we introduce a new 
cut primitive {!I.ocut} to cut out outlines. The reason is that this allows 
us to match the fill and stroke model of backend renderers and would be 
inefficient to ignore.

Introduce cutter properties. TODO what about multiple cuts ?

TODO talk about environment. TODO talk about user space.

Images can be composed toghether by pasting one of top of the other with
the function {!I.blend} of which the operator {!(<+>)} is a synonym. To paste
the black circle on the red one... special effects can be made by specifying
of colors of the back interact with color of the front.

Images are defined with respect to an implicit environment which sets
    default values for some operations. The environment avoids to have
    to specify the same arguments over and over. For example if we
    would like to draw many paths in red we can define the current
    image to crop be [red]. The following code is be equivalent to the
    preceding one.  {[let img = Si.env [ `Crop red ] begin Si.stroke
    (circle 4.) <+> Si.stroke (circle 2.)  end]}

   When no image is explictly given to Si.crop, the one from the
   environment is taken.

   Environments properties can be overriden for example the following
   code draws three concentric circles. The first one is red and
   has the default stroke_width. The two others are red 
   code is equivalent to the preceding code except that the circles
   are black and the stroke width is [0.5].
{[let black = Si.mono (color 0. 0. 0. 1.) 
let img = Si.env [ `Crop red ] begin
            Si.stroke (circle 6.) <+>
            Si.env [ `Crop black; `S_width 0.5 ] begin
              Si.stroke (circle 4.) <+> 
              Si.stroke (circle 2.)
            end
          end]} 

 When an image is constructed it cannot access the current environment
 properties and those will only be known at output. However it may
 be usefull, for example to set the stroke width to half the current
 stroke width. For that the alternate function [Si.env_f] allows to 
 change the environment depending on the current environment.
 {[let half_width e = [ `S_width (0.5 *. Si.Env.s_width e) ] 
let img' = Si.env_f half_width img]}

 All base images have the default environment.

*)


(** {1:col Color model} 

    Vg assumes a linear RGB color space with an alpha
    component. Components range from [0..1]. Make sure all the color data
    you pass (e.g. raster images) is in that space. 
    
    Specify non-premultiplied ? Any way compositing must be 
    done with premultiplied alpha. 
*)
