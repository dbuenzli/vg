(*---------------------------------------------------------------------------
   Copyright (c) %%COPYRIGHT%%. All rights reserved.
   Distributed under a BSD3 license, see license at the end of the file.
   %%PROJECTNAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Declarative 2D vector graphics.

    [Vg] is a module for declarative 2D vector graphics. Renderers for
    {{!Vgr_pdf}PDF}, {{!Vgr_svg}SVG} and the HTML {{!Vgr_htmlc}canvas}
    element are bundled with the library and an API allows to
    implement new renderers.

    Consult the {{!basics}basics} and the {{!semantics}semantics}. 
    Open the module to use it, this defines only modules and types in 
    your scope and a single {{!(>>)}sequencing operator}.

    {e Release %%VERSION%% - %%AUTHORS%% } *)

open Gg;;

(** {1 Paths and images} *)

type path 
(** The type for paths. *)

type image 
(** The type for images. *)

val ( >> ) : 'a -> ('a -> 'b) -> 'b
(** [x >> f] is [f x]. Associates to left. 
    Used as the path and image composition operator. *)

(** Paths.
    
    Consult their {{!sempaths}semantics}. 

    Path combinators always take the path to act upon as the last
    argument. Use {!Vg.(>>)} to build paths from the empty path.  *)
module P : sig

  (** {1 Path areas} *)

  type cap = [ `Butt | `Round | `Square ]
  (** The type for path caps. {{!semcaps}Semantics}.*)

  type join = [ `Bevel | `Miter | `Round ]
  (** The type for segment jointures. {{!semjoins}Semantics}.*)

  type dashes = float * float list
  (** The type for dashes. {{!semdashes}Semantics}. *)

  type outline = 
      { width : float;          (** Outline width. *)
	cap : cap;              (** Shape at the end points of open subpaths
				    and dashes. *)
	join : join;            (** Shape at segment jointures. *)
	miter_angle : float;    (** Limit {e angle} for miter joins.*)
	dashes : dashes option; (** Outline dashes. *) }
  (** The type for path outline area specifications. 
      {{!semoutlines}Semantics}.*)

  val o : outline 
  (** [o] holds a default set of values. [width] is [1.],
      [cap] is [`Butt], [join] is [`Miter], [miter_angle] is 
      [0.] and [dashes] is [None]. *)
      
  val pp_outline : Format.formatter -> outline -> unit 
  (** [pp_outline ppf o] prints a textual representation of [o] on [ppf]. *)

  type area = [ `Aeo | `Anz | `O of outline ]
  (** The type for path area specifications. 
      {{!sempaths}Semantics}.*)

  val pp_area : Format.formatter -> area -> unit
  (** [pp_area ppf a] print a textual representation of [a] on [ppf] *)

  (** {1 Paths} *)

  type t = path
  (** The type for paths. *)

  val empty : path
  (** [empty] is the empty path. *)

  (** {1 Subpaths and segments} 

      If a path segment is directly added to a path [p] which is
      {{!empty}[empty]} or whose last subpath is {{!close}closed}, a
      new subpath is {e automatically} started with {!sub}[ P2.o p].

      In the functions below the default value of the optional
      argument [rel] is [false]. If [true], the points given to the
      function are expressed {e relative} to the {{!last_pt}last
      point} of the path or {!Gg.P2.o} if the path is empty. *)

  val sub : ?rel:bool -> p2 -> path -> path
  (** [sub pt p] is [p] with a new subpath starting at [pt]. If [p]'s last 
      subpath had no segment it is automatically {!close}d. *)
 
  val line : ?rel:bool -> p2 -> path -> path
  (** [line pt p] is [p] with a straight line from [p]'s last point to [pt]. *)

  val qcurve : ?rel:bool -> p2 -> p2 -> path -> path
  (** [qcurve c pt p] is [p] with a quadratic bézier curve from 
      [p]'s last point to [pt] with control point [c]. *)

  val ccurve : ?rel:bool -> p2 -> p2 -> p2 -> path -> path
  (** [ccurve c c' pt p] is [p] with a cubic bézier curve from [p]'s 
      last point to [pt] with control points [c] and [c']. *)

  val earc : ?rel:bool -> ?large:bool -> ?ccw:bool -> size2 -> float -> p2 -> 
    path -> path
  (** [earc r angle pt p] is [p] with an elliptical arc from [p]'s
      last point to [pt].  The ellipse is defined by the horizontal
      and vertical radii [r] which are rotated by [angle] with respect
      to the current coordinate system. In the general case this
      defines four possible arcs, thus [large] indicates if more than
      pi radians of the arc is to be traversed and [ccw] if the arc is
      to be traversed in the counter clockwise direction (both default
      to [false]). See
      {{:http://www.w3.org/TR/2000/CR-SVG-20001102/images/paths/arcs02.png}this
      figure}. If parameters do not define a valid ellipse (coincident
      or too far apart points, zero radius) the arc collapses to a
      line. *)

  val close : path -> path
  (** [close p] is [p] with a straight line from [p]'s last point to
      [p]'s current subpath starting point, this ends the subpath. *)

  (** {2 Derived subpaths} 

      The following convenience functions start and close a new subpath
      to the given path. *)

  val circle : ?rel:bool -> p2 -> float -> path -> path
  (** [circle c r p] is [p] with a circle subpath of center [c] 
      and radius [r]. *)

  val ellipse : ?rel:bool -> p2 -> size2 -> path -> path
  (** [ellipse c r p] is [p] with an axis-aligned ellipse subpath of
      center [c] and radii [r].*)

  val rect : ?rel:bool -> box2 -> path -> path
  (** [rect r p] is [p] with an axis-aligned rectangle subpath 
      [r]. If [r] is empty, [p] is returned. *)

  val rrect :?rel:bool -> box2 -> size2 -> path -> path
  (** [rrect r cr p] is [p] with an axis-aligned rectangle subpath
      [r] with round corners of radii [cr]. If [r] is empty, [p]
      is returned. *)

  (** {1 Functions} *)

  val last_pt : path -> p2
  (** [last_pt p] is the last point of [p]'s last subpath.
      @raise Invalid_argument if [p] is [empty]. *)

  val append : path -> path -> path
  (** [append p' p] appends [p'] to [p]. If [p]'s last subpath had no
      segment it is closed.

      {b Warning.} To accomodate {!(>>)} the argument order is the opposite of
      {!List.append}. *)

  val bounds : ?ctrl:bool -> path -> box2
  (** [bounds ctrl p] is an axis-aligned rectangle containing [p]. If
      [ctrl] is [true] (defaults to [false]) control points are also
      included in the rectangle. Returns {!Box2.empty} if the path 
      is [empty].

      {b Warning.} This function computes the bounds of the ideal
      path (without width). Path {!outline}s areas will exceed these 
      bounds. *)

  val tr : Gg.m3 -> path -> path 
  (** [tr m p] is the affine transform in homogenous 2D space of the path
      [p] by [m]. *)

  (** {1 Traversal} *)

  type fold = 
    [ `Sub of p2
    (** New subpath starting at point, the point *)
    | `Line of p2               
    (** Line to point, the point *)
    | `Qcurve of p2 * p2  
    (** Quadratic curve to point, a control point and the point *)
    | `Ccurve of p2 * p2 * p2   
    (** Cubic curve to point, two control points and the point *)
    | `Earc of bool * bool * size2 * float * p2
    (** Elliptic arc to point, [large], [cw], [raddii], [angle] and the point *)
    | `Close 
    (** Line to point of the last [`Sub], ends the subpath. *)
    ]
  (** The type for path folds. *)

  val fold : ?rev:bool -> ('a -> fold -> 'a) -> 'a -> path -> 'a
  (** [fold ~rev f acc p], applies [f] to each subpath and subpath segments
      with an accumulator. Subpaths are traversed in the order they
      were specified, always start with a [`Sub], but may not be
      [`Close]d. If [rev] is [true] (defaults to [false]) the segments
      and subpaths are traversed in reverse order. *)

  (** {b TODO} the following two folds are strictly speaking not needed
      but they are nice for effects. They do however add ~100 lines to 
      vg. Do we keep them here ? *)

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

  (** {1 Predicates and comparisons} *) 

  val is_empty : path -> bool
  (** [is_empty p] is [true] iff [p] is [empty]. *)

  val equal : path -> path -> bool 
  (** [equal p p'] is [p = p']. *)

  val equal_f : (float -> float -> bool) -> path -> path -> bool 
  (** [equal_f eq p p'] is like {!equal} but uses [eq] to test floating
      point values. *)

  val compare : path -> path -> int 
  (** [compare p p'] is {!Pervasives.compare}[ p p']. *)

  val compare_f : (float -> float -> int) -> path -> path -> int 
  (** [compare_f cmp p p'] is like {!compare} but uses [cmp] to compare 
      floating point values. *)

  (** {1 Printers} *)

  val to_string : path -> string
  (** [to_string p] is a textual representation of [p]. *) 

  val pp : Format.formatter -> path -> unit
  (** [pp ppf p] prints a textual representation of [p] on [ppf]. *)
  
  val pp_f : (Format.formatter -> float -> unit) -> Format.formatter -> 
    path -> unit
  (** [pp_f pp_float ppf p] prints [p] like {!pp} but uses [pp_float] to 
      print floating point values. *)
end

(** Images.
  
    Consult their {{!semimages}semantics} aswell as 
    the semantics of {{!semcolors}colors} and {{!semstops}color stops}. 

    Image combinators always take the image as the last
    argument, to use {!Vg.(>>)} compose them.  *)
module I : sig

  (** {1:prims Primitive images} *)

  type t = image
  (** The type for images. *)

  val mono : color -> image
  (** [mono c] is a monochrome image of color [c].
      {ul {- \[[mono c]\]{_[pt]} [= c] for any [pt].}} *)

  val void : image
  (** [void] is [mono ]{!Gg.Color.void}, the invisible black image. *)

  val axial : Color.stops -> p2 -> p2 -> image
  (** [axial stops pt pt'] is an image with an axial color gradient 
      varying between [pt] and [pt'] according to {{!semstops} color stops} 
      [stops].

      {ul {- \[[axial stops pt pt']\]{_[q]} [=] \[[stops]\]{_[t]} if [q] is 
      on the line perpendicular to the line [pt] and [pt'] at
      the point [pt + t * (pt' - pt)].}} *) 

  val radial : Color.stops -> ?f:p2 -> p2 -> float -> image 
  (** [radial stops ~f c r] is an image with a color gradient varying
      according to {{!semstops} color stops} [stops] on circles whose
      center are on the segment from [f] to [c] and radius vary,
      respectively, from [0] to [r]. The focus [f] defaults to [c], it
      must be inside the circle [(c, r)]. 

      {b TODO} Semantics is wrong.

      {ul {- \[[radial stops ~f c r]\]{_[p]} [=] \[[stops]\]{_[t]} if [p] is
      on the circle defined by radius [t * r] and center [f + t * (c - f)].}} 
  *)

  val raster : box2 -> Raster.t -> image 
  (** [raster r ri] is an image with [ri] framed in the rectangle
      [r] 

      {b TODO} Semantics and filtering.

      {ul 
      {- \[[raster r ri]\]{_[p]} [=] TODO if [p] is in {{!aboxes}S([r])}.}
      {- \[[raster r ri]\]{_[p]} [= Gg.color.void] otherwise.}} *)

  (** {1:cut Cutting images} *)

  val cut : ?area:P.area -> path -> image -> image
  (** [cut area p i] is [i] with the {{!sempaths}area} outside 
      \[[a], [p]\] cut out, i.e. mapped to {!Gg.Color.void}. [area]
      defaults to {{!P.area}[`Anz]}.
      {ul 
      {- \[[cut area p i]\]{_[pt]} [=] \[[i]\]{_[pt]} if \[[a], [p]\]{_[pt]}}
      {- \[[cut area p i]\]{_[pt]} [=] {!Gg.Color.void} otherwise.}} *)

(* TODO

  type glyph = int * v2

  type text = string * (int * int) list * bool (* reverse *)
  (* cluster is an indivisble mapping of M character to N glyphs. *)

  val cut_glyphs : ?text:text -> P.area -> glyph list -> image ->
  image
*)

  (** {1:blend Blending images} *)
  
  type blender = [ `Atop | `In | `Out | `Over | `Plus | `Copy | `Xor ]

  val blend : ?a:float -> ?blender:blender -> image -> image -> image 
  (** [blend i i'] blends the colors of [i] over those of [i']. If [a] is 
      specified this value is used as the alpha value for each color of the 
      resulting image.

      {b TODO.} Semantics. Blender, support just `Over ? 
      {ul 
      {- \[[blend i i']\]{_[p]} [=] (\[[i]\]{_[p]}) \[\] (\[[i']\]{_[p]}) 
         for any [p] if [a] is unspecified.}
      {- \[[blend i i']\]{_[p]} [= (c]{_r}[,c]{_g}[,c]{_b}[,a)] 
         for any [p] with
         c = \[[i]\]{_[p]} \[\] \[[i']\]{_[p]} otherwise.}} 
   *)

  (** {1:transf Transforming images} *)

  val move : v2 -> image -> image
  (** [move v i] is [i] translated by [v].
      {ul {- \[[move v i]\]{_[pt]} [=] \[[i]\]{_[pt-v]} for any [pt]}} *)

  val rot : float -> image -> image
  (** [rot a i] is [i] rotated by [a]. 
      {ul {- \[[rot a i]\]{_[pt]} [=] \[[i]\]{_[m⋅pt]} for any [pt] with
       [m = M2.rot -a].}} *)

  val scale : v2 -> image -> image
  (** [scale s i] is [i] scaled by [s].
      {ul {- \[[scale s i]\]{_[(x,y)]} [=] \[[i]\]{_[(x/s]{_x}[,y/s]{_y}[)]} 
      for any [(x,y)]}} *)

  val tr : m3 -> image -> image
  (** [tr m i] is the affine transform in homogenous 2D space of 
      each point of [i] by [m] (see {!P2.tr}). 
      {ul {- \[[tr m i]\]{_[p]} [=] \[[i]\]{_[m]{^-1}⋅[p]} for any [p]}} *)

  (** {1:predicates Predicates and comparisons} *)

  val is_void : image -> bool 
  (** [is_void i] is [i == void]. *)

  val equal : image -> image -> bool
  (** [equal i i'] is [i = i']. *)

  val equal_f : (float -> float -> bool) -> image -> image -> bool
  (** [equal eq i i'] is [i = i'] is like {!equal} but uses [eq] 
      to test floating point values. 

      {b Note.} Raster images are tested with {!Raster.equal}. *)

  val compare : image -> image -> int   
  (** [compare i i'] is [Pervasives.compare i i']. *) 

  val compare_f : (float -> float -> int) -> image -> image -> int
  (** [compare_f cmp i i'] is like {!compare} but uses [cmp] to
      compare floating point values. 

      {b Note.} Raster images are tested with {!Raster.compare}. *)

  (** {1:printers Printers} *)

  val to_string : image -> string
  (** [to_string i] is a textual representation of [i]. *)

  val pp : Format.formatter -> image -> unit
  (** [pp ppf i] prints a textual representation of [i] on [ppf]. *)

  val pp_f : (Format.formatter -> float -> unit) -> Format.formatter -> 
    image -> unit
  (** [pp_f pp_float ppf i] prints [i] like {!pp} but uses [pp_float]
      to print floating point values. *)
end

(** {1 Renderers} *)

type renderer
(** The type for image renderers. *)

(** Image renderers. 

    Renderer render finite rectangular regions of images on
    rectangular surfaces. The following renderers are distributed with
    the library:
    {ul 
    {- {!Vgr_pdf}, renders sequence of images as a multi-page PDF/A document.}
    {- {!Vgr_svg}, renders a single image as an 
       {{:http://www.w3.org/TR/SVGTiny12/}SVG Tiny 1.2} document.}
    {- {!Vgr_htmlc}, renders sequence of images on an 
       {{:http://www.w3.org/TR/2dcontext/}HTML canvas} 
       element via {{:http://ocsigen.org/js_of_ocaml/}js_of_ocaml}.}}
*) 
module Vgr : sig

  (** {1:renderable Renderable} 

      A renderable specifies the physical size of a render surface and
      a view rectangle for an image. It implicetly defines the mapping
      between [Vg]'s coordinate space and the surface, see
      {{!coordinates} this section} for more information. *)

  type renderable = size2 * box2 * image
  (** The type for renderables. The physical size of the rendering
      surface in millimeters, the view rectangle and the image to
      render. *)

  (** {1:meta Render metadata} 

      Render metadata specifies additionnal data (e.g. quality hints)
      for the renderer aswell as image metadata (date, title, etc.)
      for the image.  Consult the documentation of renderers to see
      which keys they support. *)

  (** Render metadata. 

      Render metadata is a set of {{!keys}keys} mapping to typed values. 
      Renderers can define their own keys but should favor {{!stdkeys}standard}
      ones if appropriate. *)
  module Meta : sig

    (** {1:keys Keys} *)
    
    type 'a key 
    (** The type for metadata keys whose lookup value is ['a]. *)

    val key : unit -> 'a key 
    (** [key ()] is a new metadata key. *)
    
    (** {1:metadata Metadata} *)    

    type t 
    (** The type for metadata. *)

    val empty : t 
    (** [empty] is the empty metadata. *)

    val is_empty : t -> bool 
    (** [is_empty m] is [true] iff [m] is empty. *)

    val mem : t -> 'a key -> bool 
    (** [mem m k]  is [true] iff [k] has a mapping in [m]. *)

    val add : t -> 'a key -> 'a -> t
    (** [add m k v] is [m] with [k] mapping to [v]. *)

    val rem : t -> 'a key -> t
    (** [rem m k] is [m] with [k] unbound. *)

    val find : t -> 'a key -> 'a option
    (** [find m k] is [k]'s mapping in [m], if any. *)

    val get : t -> 'a key -> 'a
    (** [get m k] is [k]'s mapping in [m].
        @raise Invalid_argument if [k] is not bound in [m]. *)

    (** {1:stdkeys Standard keys} 

      {b Note.} All string values must be UTF-8 encoded. *)

    (** {2:renderingmeta Rendering metadata} *)

    val res : v2 key
    (** [res] specifies the rendering resolution in samples per meters. *)

    val quality : [`Best | `Speed] key
    (** [quality] specifies the rendering quality. *)

    (** {2:imagemeta Image metadata} *)

    val author : string key 
    (** [author] is the author of the image. *)

    val creator : string key 
    (** [creator] is the name of the application creating the image. *)

    val date : ((int * int * int) * (int * int * int)) key
    (** [date] is the date of creation of the image. 

        The first triple is the year (0-9999), the month (1-12) and
	the day (1-31). The second triple is the time in UTC, the hour
	(0-23), minutes (0-59) and seconds (0-59). 

        {b Warning.} Numerical constraints are not checked by
        the renderers. *)

    val title : string key 
    (** [title] is the title of the image. *)

    val subject : string key 
    (** [subject] is the subject of the image. *)

    val keywords : string list key 
    (** [keywords] is a list of keywords for the image. *)
  end

  (** {1:warnings Render warnings} 

      TODO redo.

      Renderers do their best to support [Vg]'s rendering model and
      semantics. However they may sometimes lack capabilities provided
      by [Vg]'s api. If a renderer encounters a capabilities it
      doesn't support, it must ignore the unsupported rendering
      instruction and add a warning to a list returned by the
      {!render} function.

      Consult the documentation of renderers to see which capabilities are
      supported. *)

  (** {1:render Rendering} *)

  type dst_stored = 
    [ `Buffer of Buffer.t | `Channel of Pervasives.out_channel | `Manual ]
  (** The type for stored renderer destination. With a [`Manual]
      destination the client must provide output storage see
      {!Manual.dst}. *)

  type dst = [ dst_stored | `Immediate ]
  (** The type for renderer destinations. Either a stored destination 
      or an [`Immediate] surface destination, usually denoting some
      kind of interactive renderer. *)

  type t = renderer
  (** The type for renderers. Values of this type are created by the renderer
      modules. The type parameter specifies the type for warnings. *)
    
  val render : t -> [< `Image of renderable | `Await | `End ] -> 
    [ `Ok | `Partial ]
  (** [render r v] is:
        {ul
        {- [`Partial] iff [r] has a [`Manual] destination and needs more 
        output storage. The client must use {!Manual.dst} to provide a new
        buffer can then call {!render} with [`Await] until [`Ok] is returned.}
        {- [`Ok] when the encoder is ready to encode a new [`Image]
        (if the renderer supports it) or [`End].}}
        For [`Manual] destinations, encoding [`End] always returns [`Partial]
        the client should as usual use {!Manual.dst} and continue with [`Await]
        until [`Ok] is returned at which point {!Manual.dst_rem}[ r] is 
        guaranteed to be the size of the last provided buffer (i.e. nothing
        was written). 

        {b Semantics of multiple images render.} If multiple images are 
        rendered the previous image is cleared from the render surface. 
        This can take the form of defining a new page like in {!Vgr_pdf} or
        simply clearing the render surface like in {!Vgr_htmlc}.

        @raise Invalid_argument if [`Image] or [`End] is encoded after
        a [`Partial] encode. Or if multiple [`Image]s are encoded in 
        a renderer that doesn't support them. *)
       
  val renderer_dst : renderer -> dst
  (** [render_dst r] is [r]'s output destination. *)

  val renderer_meta : renderer -> Meta.t 
  (** [renderer_meta r] is [r]'s render metadata. *)

  (** {1 Manual render destinations} *)

  (** Manual render destinations. 

      {b Warning.} Only use with [`Manual] renderers. *)
  module Manual : sig
    val dst : renderer -> string -> int -> int -> unit 
    (** [dst r s j l] provides [r] with [l] bytes to write, starting
          at [j] in [s]. This byte range is written by calls to {!render}
          until [`Partial] is returned. Use {!dst_rem} to know the remaining
          number of non-written free byte in [s]. *)
      
    val dst_rem : renderer -> int 
      (** [dst_rem r] is the remaining number of non-written, free bytes
          in the last buffer provided with {!dst}. *)
  end

  (** {1:renderer  Implementing renderers} *)

  (** Private functions for implementing renderers.

      [Vg] users should not use these definitions, they exposes [Vg]'s
      internals for implementing renderers.  This functionality is
      subject to change {e even between minor versions of the
      library}.

      In order to provide a consistant interface for [Vg] users,
      renderer writers should follow the guidelines below.  You may
      want to drop an email to %%AUTHORS%% for help and discussion.
      {ul
      {- If you render to "Bla", define you renderer in a module
         called [Vgr_bla] (lowercase).}
      {- The renderer creation function must be named [Vgr_bla.renderer], 
         have an optional argument [?meta:Vgr.Meta.t] and 
         return a [Vgr.t] value.}
      {- Images must be rendered via the {!render} function. If you 
         are writing a batch renderer provide support for each of the 
         {!dst} types and especially the non-blocking interface.}
      {- If your renderer supports render metadata or needs user-defined
         parameters use the {!Meta.t} type to define it. Whenever possible
         reuse the the existing {{!Meta.stdkeys}standard} keys.}
      {- Follow [Vg]'s 
         {{!coordinates}coordinate system conventions} to 
         specify the relationship between a surface and the view 
         rectangle to render.}
      {- If the renderer doesn't support [Vg]'s full rendering model or
         diverges from its semantics it must ignore unsupported features
         and warn the client by implementing the 
         {!Incomplete_renderer} interface.}}
*)
  module Private : sig
    
    (** {1 Incomplete renderer} 
        
        Interface to implement if your renderer doesn't fully support
        [Vg]'s rendering model. *)

    (** The type for incomplete renderers. *)
    module type Incomplete_renderer = sig

      (** {1 Rendering warnings} *)

      type warning
      (** The type for rendering warnings. Must be a polymorphic
          variant with one case for each missing feature. *)

      val pp_warning : Format.formatter -> warning -> unit
      (** [pp_warning ppf w] prints a textual representation of [w] on [ppf]. *)

      val warn : (warning -> unit) Meta.key
      (** [warn] is a function called whenever missing features are 
          encountered during rendering. *)
    end

    (** {1 Path representation} *)

    type segment = 
      [ `Sub of p2
      | `Line of p2
      | `Qcurve of p2 * p2 
      | `Ccurve of p2 * p2 * p2 
      | `Earc of bool * bool * size2 * float * p2
      | `Close ]
    (** The type for path segments. *)
      
    type path = segment list
    (** The type for paths. The segment list is reversed. A few invariants
        apply. TODO See Vg's source. *)

(* 
    val path : P.t -> path 
    (** [path p] is [p]'s internal representation. *)
*)

    (** {1 Image representation} *)

    (** The type for transforms. We don't uniformely express as a
        matrix since renderers may have shorter syntaxes for some
        transforms. *)
    type tr = Move of v2 | Rot of float | Scale of v2 | Matrix of m3
    
    (** The type for image primitives. *)    
    type primitive = 
      | Mono of color
      | Axial of Color.stops * p2 * p2
      | Radial of Color.stops * p2 * p2 * float
      | Raster of box2 * raster
    
            
    (** The type for images. *)
    type image = 
      | Primitive of primitive
      | Cut of P.area * path * image
      | Blend of I.blender * float option * image * image
      | Tr of tr * image

(*
    val image : I.t -> image
    (** [image i] is [i]'s internal representation. *)
*)

   (** {1 Renderers } *)
   
   type renderer
   (** The type for renderers. *)
    
   val renderer : t -> renderer
   (** [renderer r] is [r]'s internal representation. *)

   type k = renderer -> [ `Ok | `Partial ] 
   (** The type for renderer continuations. *)

   type 'a render_fun = 'a -> [`End | `Image of size2 * box2 * image ] -> k -> k
   (** The type for rendering functions. TODO *)

   val create_renderer : ?meta:Meta.t -> ?once:bool -> [< dst] -> 
     'a -> 'a render_fun -> t
   (** [create_renderer meta multi dst state rfun] is a renderer
       [r] such that: 
       {ul
       {- [render r (`Image i)] invokes [rfun state (`Image i)]}
       {- [render r `End] invokes [rfun state `End]}}
       If [once] is [true] (defaults to [false]) calling {!render}
       with more than one [`Image] will raise an [Invalid_argument] exception.
   *)
       
    val partial : k -> renderer -> [> `Partial]
    (** [partial k r] suspends the renderer [r] and returns [`Partial]. 
        Rendering will continue with [k r], on [`Await] {!render}. *)

    (** {1 Writing {!dst_storage} destinations} *)

    val flush : k -> renderer -> [ `Ok | `Partial ]
    (** [flush k r] flushes the renderer [r]. This must be called
        by the rendering function on [`End]. *)

    val writeb : int -> k -> renderer -> [ `Ok | `Partial ]
    (** [writeb b k r] writes the byte [b] and [k]ontinues. *)
        
    val writes : string -> int -> int -> k -> renderer -> [ `Ok | `Partial ]
    (** [writes s j l k r]  writes [l] bytes from [s] starting at [j]
        and [k]ontinues. *)

    val writebuf : Buffer.t -> int -> int -> k -> renderer -> [`Ok | `Partial ]
    (** [writebuf buf j l k r] write [l] bytes from [buf] starting at [j]
        and [k]ontinues. *)
      
  end
end

(** {1:basics Basics} 

    [Vg] is designed to be opened in your module. This defines only
    types and modules in your scope and a {e single} value, the
    sequencing operator {!(>>)}. Thus to use [Vg] start with :
{[open Gg;;
open Vg;;]}
    {!Gg} gives us types for points ({!Gg.p2}), vectors ({!Gg.v2}), 2D
    extents ({!Gg.size2}), rectangles ({!Gg.box2}) and colors
    ({!Gg.color}). Later you may want to read {!Gg}'s documentation
    {{!Gg.basics}basics} but for now it is sufficient to know that each of these
    types has a constructor [v] in a module named after the
    capitalized type name ({!P2.v}, {!V2.v}, etc.).

    {2 Infinite images}

    In [Vg], images are {e conceptually} infinite. They are seen as
    functions mapping points of the plane to colors.  Images are
    immutable values of type {!image}.

    The simplest image is a monochrome image: an image that associates
    the same color to every point in the plane.  This expression
    defines an infinite red image:
{[let red = I.mono Color.red]}
    The module {!I} contains all the combinators to define and compose
    infinite images, we'll explore some of them later.

    {2 Rendering}

    Manipulating infinite images with combinators is blissful but
    seeing them is more interesting. Images defined with [Vg] can be
    rendered to multiple renderers. The module {!Vgr} defines a few
    functions common to certain renderers and allows to specify image
    metadata for non-interactive renderers.

    The following code outputs the unit square of [red] on a
    4x4 centimeters PDF surface in the file [/tmp/vg-tutorial.pdf]: 
{[let usquare_to_pdf i = 
  let size = Size2.v 40. 40. in
  let view = Box2.v P2.o (Size2.v 1. 1.) in
  try
    let oc = open_out "/tmp/vg-tutorial.pdf" in
    let r = Vgr_pdf.renderer (`Channel oc) in
    ignore (Vgr.render r (`Image (size, view, i));
    ignore (Vgr.render r `End);
    close_out oc
  with Sys_error e -> prerr_endline e

let () = usquare_to_pdf red]}
    The result should be a red square like {{:TODObasics-red-usquare.png} 
    this}.

    {2:coordinates Coordinate spaces} 

    [Vg]'s cartesian coordinate space has the y-axis pointing up and
    x-axis right. It has no units, you define what they mean to you.

    On rendering the corners of the specified view rectangle are
    mapped one to one to the surface's corners. This is done
    regardless of the (renderer dependent) surface coordinate's system,
    appropriate transforms are applied so that the bottom-left corner
    of the view rectangle maps to the bottom-left corner of the
    surface and so forth for each corner.

    If a surface has a physical size, the view rectangle implicitely
    defines a physical unit for [Vg]'s coordinate space when the image
    is rendered on the surface.

    {2 Scissors and glue}

    Monochrome images can be boring. To make things more interesting
    [Vg] gives you scissors. Suppose you have a path (more on paths
    later) defining a circle, the following code cuts out that circle
    in the monochrome image [red] and returns it as an infinite image.
{[let circle = P.empty >> P.circle (P2.v 0.5 0.5) 0.4 
let red_circle = I.cut `Aeo red circle]}
    Look at the {{:TODObasics-red-circle.png}result} rendered by
    [usquare_to_pdf].  

    The value [red_circle] is still an infinite image, it associates
    the color {!Gg.Color.red} to the points located in the circle and
    an invisible transparent black color ({!Gg.Color.void}) at every
    other point of the plane. The white color surrounding the circle
    in the rendered image does not belong to the image itself, it is
    the color of the background against which the image is composited
    (your eyes require a wavelength there and {!Gg.Color.void} cannot
    provide it).

    The first argument to {!I.cut} is a value of type {!I.area_rule}.
    An area rule determines how a path should be interpreted as an
    area of the plane. Here [`Aeo] is the even-odd rule and for
    [circle] this defines its interior. But [circle] can also be seen
    as defining a thin outline area of a given width with an
    area rule [`Ol ol]. The argument [ol] of type {!type:I.outline} defines
    parameters for the outline area; for example the width of the
    outline. The following code defines an image with a black [circle]
    outline area of width [0.01].
{[let circle_outline =
  let ol = { I.ol with I.width = 0.01 } in
  I.cut (`Ol ol) (I.mono Color.black) circle]}
    {{:TODObasics-circle-outline.png} Result}. Here the image
    [circle_outline] is an infinite image that associates the color
    {!Gg.Color.black} in the outline area and the invisible color
    {!Gg.Color.void} at every other point of the plane. Remember that
    the white color you see is in fact {!Color.void}.

    With {!I.cut}, area rules and paths can cut out interesting,
    finite portions of infinite images (e.g. areas with holes). This
    gives us scissors, but to combine the results we need some
    glue. Glue is provided by the function {!I.blend} which pastes
    (alpha blends to be precise) an image on top of the other. Here's
    how you glue [circle_outline] on top of [red_circle] :
{[let dot = I.blend circle_outline red_circle]} 
    {{:TODObasics-dot.png}Result}. 
    Again, the image [dot] is still an infinite image, the color at a
    point [p] is the color of [red_circle] at [p] alpha blended with
    the color of [circle_outline] at [p].

    If you need to blend many images, use the sequencing operator, 
    it makes code more readable : 
{[red_circle >> I.blend circle_outline >> I.blend other]} 
  
    {2 Transforming images}
    
    It can be useful to transform an image, for example to rescale or
    rotate it. The combinators {!I.move}, {!I.rot}, {!I.scale},
    {!I.shear} and {!I.tr} allow to perform arbitrary affine
    transformations on an image.

    The following example moves [dot]'s center to the origin.
{[let odot = I.move (V2.v (-0.5) (-0.5)) dot]}
    {{:TODObasics-odot.png}Result}. Here [odot] is an infinite image 
    whose color at [p] is given by the color at [p + (0.5, 0.5)]
    in [dot]

    {2 Paths} 

    As we already said path are used to define areas of the
    plane. Paths are immutable value of type {!path}. A path is made
    of disconnected subpaths.  A subpath is a sequence of points
    defining segments that define an interpolation
    scheme. Interpolation between two points can be.

    {2 Gradients}




    {2 Raster images}

    

{[let i = 
  let circle r = P.empty & P.circle (V2.t 0.5 0.5) r in
  let o = `Ol { I.ol with width = 5. } in
  let black = I.mono Color.black in 
  let red = I.mono Color.red in 
  I.cut o black (circle 0.15) +++
  I.cut o black circle
  I.blend (I.cut o black (circle 0.3)) &
  I.blend (I.cut o red (circle))]}

    
    {2:remarkstips Remarks and tips}
    {ul
    {- Everything is tail-recursive unless otherwise mentionned.}
    {- [to_string] functions are not thread-safe. Thread-safety
       can be achieved with [print] functions.}
    {- Angles follow [Gg]'s {{!Gg.mathconv}conventions}.}
    {- Do not rely on the output of printer functions, they
       are subject to change.}
    {- Rendering results are undefined if path
       or image data contains NaNs or infinite floats.}
    {- The modules assumes any string value to be UTF-8 encoded.}
    {- Sharing (sub)images, paths and outlines
       values in the definition of an image results in more
       efficient rendering in space and time.}
    {- Sometimes favourite feature not present. Just a matter
       of transforming. E.g. elliptic gradient. Others 
       are not possible e.g. canvas radial gradients. 
       Design choices are a tension between supporting 
       rendering to many renderers in a least surprising way.}
    {- Images are said to be immutable. This is only true if you 
       don't change the samples of raster images given o {!I.raster}.}
    {- The renderers support [Vg]'s imaging model which is
       oriented towards diagram and gui rendering. If you need to access
       features of a file format that are beyond this scope use a dedicated 
       library. }
    {- TODO The essence of [Gg] is the {e collage} model : an image is a
       superposition of image cuts. This contrasts with the classical 
      {e painter}'s model where an image is a superposition of path fills and
      strokes. The models are equivalent but the collage model make some
      operations clearer. An example of an unclear operation in the
      painter's model is stroking a self-intersecting path with a
      translucent color.}}


    {2:refs References}

    [Vg]'s collage model draws from the following works of Conal Elliott 
    and Antony Courtney.

    Conal Elliott. 
    {e {{:http://conal.net/papers/bridges2001/}Functional Image
    Synthesis}}, Proceedings of Bridges, 2001.

    Antony Courtney. {e Haven : Functional Vector Graphics}, chapter 6
    in
    {{:http://yufind.library.yale.edu/yufind/Record/9844025/Description}Modeling
    User Interfaces in a Functional Language}, Ph.D. Thesis, Yale
    University, 2004. *)

(** {1:semantics Semantics} 

    The following notations and definitions are used to give precise
    meaning to the images and [Vg]'s combinators.

    {2:semcolors Colors}

    The semantics of colors is the one ascribed to {{!Gg.Color.t}[Gg.color]}. 

    The (semantic) blending function \[\][ : color -> color -> color]
    mixes two colors [c = (r,g,b,a)] and [c' = (r',g',b',a')] into a
    new color value written [c] \[\] [c'] defined as follows :
{[c \[\] c' = ((a'r' + (1 - a')ar) / a'',
           (a'g' + (1 - a')ag) / a'',
           (a'b' + (1 - a')ab) / a'',
            a'') 
          with a'' = a' + (1 - a')a]}

      In the equation above division by zero results in zero.  The
      function \[\] corresponds to the Porter-Duff {e over} operator.

      More information about the alpha component and image
      compositing can be found here :

      Alvy Ray Smith. {e
      {{:ftp://ftp.alvyray.com/Acrobat/4_Comp.pdf}Image compositing
      fundamentals}}. Microsoft technical Memo 4. 1995. 

      {2:semstops Color stops} 

      A value of type {!Gg.Color.stops} specifies a color at each point of the
      1D {e unit} space. It is defined by a list of pairs
      [(t]{_i}[, c]{_i}[)] where [t]{_i} is a value from [0] to [1] and
      [c]{_i} the corresponding color at that value. Colors at points
      between [t]{_i} and [t]{_i+1} are linearly interpolated between
      [c]{_i} and [c]{_i+1}. Pairs whose [t]{_i} lies outside [0] to
      [1] or such that [t]{_i-1} >= [t]{_i} are discarded.

      Given a stops value [stops = \[][(t]{_0}[, c]{_0}[);]
      [(t]{_1}[, c]{_1}[);] ... [(t]{_n}[, c]{_n}[)][\]] and any point
      [t] of 1D space, the semantic function \[\] [: Color.stops ->
      float -> color] maps them to a color value written
      \[[stops]\]{_t} as follows.
      {ul
      {- \[[]\]{_t} = [(0, 0, 0, 0)] for any [t]}
      {- \[[stops]\]{_t} [= c]{_0} if [t < t]{_0}.}
      {- \[[stops]\]{_t} [= c]{_n} if [t >= t]{_n}.}
      {- \[[stops]\]{_t} [= (1-u)c]{_i}[ + uc]{_i+1} 
      with [u = (t - t]{_i}[)/(t]{_i+1}[-t]{_i}[)]
      if [t]{_i} [<= t <] [t]{_i+1}}}

    {2:semimages Images}    

    An image is a mapping from the infinite 2D euclidian space to
    {{!semcolors}colors}. Values of type {!image} represent infinite
    images. Given an image [i] and a point [pt] of the plane the
    semantic function \[\][: image -> p2 -> Gg.color] maps them to a
    color value written \[[i]\]{_[pt]} representing the image's color
    at this point.

    {2:sempaths Paths and areas}
    
    A value of type {!path} is a list of subpaths. A subpath is a list
    of {e directed} and connected curved {e segments}. Subpaths can be
    disconnected from each other and may (self-)intersect.

    A path and a value of type {!P.area} defines a finite area of
    the 2D euclidian space. Given a rule [a], a path [pa] and a point
    [p], the semantic function \[\]: [P.area -> path -> p2 ->
    bool] maps them to a boolean value written \[[a], [p]\]{_[pt]}
    that indicates whether [p] belongs to the area or not. The
    semantics of area rules is as follows :
    {ul
    {- \[[`Aeo], [p]\]{_[pt]} is determined by the even-odd rule.
       Cast a ray from [pt] to infinity in any direction (just make 
       sure it doesn't intersect [p] tangently or at a singularity).
       Count the number of [p]'s segments the ray crosses. [pt] belongs to
       the area iff this number is odd.
       {{:http://www.w3.org/TR/SVG/images/painting/fillrule-evenodd.png}
       Illustration}.}
    {- \[[`Anz], [p]\]{_[pt]} is determined by the non-zero winding number 
       rule. Cast a ray from [pt] to infinity in any direction (just make 
       sure it doesn't intersect [pa] tangently or at a singularity). 
       Starting with a count of zero examine each intersection of the 
       ray with [p]'s segments. If the segment crosses the ray from left to
       right add one to the count, otherwise, from right to left, subtract
       one. [pt] belongs to the area iff the final count is not zero.
       {{:http://www.w3.org/TR/SVG/images/painting/fillrule-nonzero.png}
       Illustration}.}
    {- \[[`O o], [p]\]{_[pt]} is determined by the outline
        area of [p] as defined by the value [o] of type 
        {!type:P.outline}. See below.}}

    {3:semoutlines Outlines}

    The outline area of a path is the union of its subpaths outline
    areas.  A subpath outline area is defined by the parallel
    curves of its segments connected according to the
    {{:http://www.w3.org/TR/SVG11/images/painting/linejoin.png}joins}
    specified [o.join] and closed at the subpath end points (if
    any) with a
    {{:http://www.w3.org/TR/SVG11/images/painting/linecap.png}cap}
    specified by [o.cap]. The parallel curves of a segment are its
    parallels on each side at a distance [o.width / 2]. The area of
    a subpath may also can also be chopped at regular interval 
    according to the [o.dashes] parameter.
    
    {4:semcaps Path caps} 

    
    For path outlines, specifies the
    {{:http://www.w3.org/TR/SVG11/images/painting/linecap.png}shape}
    at the end points of open subpaths and dashes.
    {ul 
    {- [`Butt]. A line with a squared-off end. draws the line to
       extend only to the exact endpoint of the path `Round.}
    {- [`Round]. A line with a rounded end. Quartz draws the line to
       extend beyond the endpoint of the path. The line ends with a
       semicircular arc with a radius of 1/2 the line’s width,
       centered on the endpoint.}  
    {- [`Square]. A line with a squared-off end. Quartz extends the
       line beyond the endpoint of the path for a distance equal to
       half the line width}}


    {4:semjoins Segment jointures}
    
    For path outlines, specifies the
    {{:http://www.w3.org/TR/SVG11/images/painting/linejoin.png}shape}
    at segment jointures.
    {ul
    {- [`Miter]. Outer parallel curves of the joining segments are
       extended until they meet, unless the joining angle is smaller
       than [miter_angle] in which case, the join is converted to a
       bevel.}  
    {- [`Bevel]. A join with a squared-off end. Quartz draws the line
       to extend beyond the endpoint of the path, for a distance of
       1/2 the line’s width.}
    {- [`Round]. Quartz draws the line to extend beyond the endpoint
       of the path. The line ends with a semicircular arc with a
       radius of 1/2 the line’s width, centered on the endpoint.}}

      The [miter_angle] parameter is used when [join] is [`Miter] : if
      the join angle of two segments is below [miter_angle] the jointure
      shape is converted to a [`Bevel]. 

    {4:semdashes Path dashes}

      The {e dash pattern} is
      a list of lengths that specify the length of alternating dashes
      and gaps (starting with dashes). The {e dash offset} indicates
      where to start in the dash pattern at the beginning of a
      subpath. 
*)

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
