(*---------------------------------------------------------------------------
   Copyright 2013 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Declarative 2D vector graphics.
    
    [Vg] is a declarative 2D vector graphics library. In [Vg], images
    are {{!Vg.image}values} that denote functions mapping points of
    the cartesian plane to colors. The library provides
    {{!Vg.I}combinators} to define and compose them. Renderers for
    {{!Vgr_pdf}PDF}, {{!Vgr_svg}SVG} and the HTML {{!Vgr_htmlc}canvas}
    are distributed with the library. An API allows to implement
    new renderers.

    Consult the {{!basics}basics}, the {{!semantics}semantics} and
     {{!examples}examples}.
    
    Open the module to use it, this defines only modules and types in 
    your scope and a single {{!(>>)}composition operator}.

    {e Release %%VERSION%% — %%MAINTAINER%% } *)

open Gg
  
(** {1 Fonts} *)
  
(** Fonts. 
    
    Font handling in [Vg] happens in renderers and text layout and
    text to glyph translations are expected to be carried out by an
    external library. Values of type {!Vg.font} just represent a font 
    specification to be resolved by the concrete renderer. *)
module Font : sig
  
  (** {1 Fonts} *)
  
  type weight =
    [ `W100 | `W200 | `W300 | `W400 | `W500 | `W600 | `W700 | `W800 | `W900 ]
  (** The type for font weights. Usually [`W400] denotes a normal 
      weight and [`W700], a bold weight. *)
    
  type slant = [ `Normal | `Italic | `Oblique ]
  (** The type for font slants. *)
               
  type t = 
    { name : string;
      slant : slant;
      weight : weight; 
      size : float; }
  (** The type for fonts. The size is expressed in 
      [Vg]'s {{!coordinates}coordinate space}, the em unit of the font
      is scaled to that size. *)
 
  (** {1 Predicates and comparisons} *)

  val equal : t -> t -> bool 
  (** [equal font font'] is [font = font']. *)

  val equal_f : (float -> float -> bool) -> t -> t -> bool 
  (** [equal_f eq font font'] is like {!equal} but uses [eq] to test floating
      point values. *)

  val compare : t -> t -> int
  (** [compare font font'] is [Pervasives.compare font font'] *)

  val compare_f : (float -> float -> int) -> t -> t -> int 
  (** [compare_f cmp font font'] is like {!compare} but uses [cmp] to compare 
      floating point values. *)

  (** {1 Printers} *)

  val to_string : t -> string
  (** [to_string font] is a textual representation of [font]. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf font] is a textual representation of [font] on [ppf]. *)
end

type font = Font.t
(** The type for fonts. *)

type glyph = int
(** The type for glyphs. The integer represents a glyph identifier in a 
    backend dependent font format. *)

(** {1 Paths and images} *)

type path 
(** The type for paths. *)

type image 
(** The type for images. *)

val ( >> ) : 'a -> ('a -> 'b) -> 'b
(** [x >> f] is [f x], associates to left.
    Used to build paths and compose images. *)

(** Paths.
    
    Consult their {{!sempaths}semantics}. 
    
    The composition operator {!Vg.(>>)} is used to build paths from
    the empty path. For this reason path combinators always take the
    path to use as the last argument. *)
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
      miter_angle : float;    (** Limit {e angle} for miter joins 
                                    (in \[0;pi\]).*)
      dashes : dashes option; (** Outline dashes. *) }
  (** The type for path outline area specifications. 
      {{!semoutlines}Semantics}.*)
    
  val o : outline 
  (** [o] holds a default set of values. [width] is [1.],
      [cap] is [`Butt], [join] is [`Miter], [miter_angle] is 
      [11.5] degrees in radians and [dashes] is [None]. *)
    
  val pp_outline : Format.formatter -> outline -> unit 
  (** [pp_outline ppf o] prints a textual representation of [o] on [ppf]. *)
    
  type area = [ `Aeo | `Anz | `O of outline ]
  (** The type for path area specifications. 
      {{!sempaths}Semantics}.*)

  val pp_area : Format.formatter -> area -> unit
  (** [pp_area ppf a] prints a textual representation of [a] on [ppf] *)
    
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
      point} of the path or {!Gg.P2.o} if the path is {{!empty}[empty]}. *)
    
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
    
  val earc : ?rel:bool -> ?large:bool -> ?cw:bool -> ?angle:float -> size2 -> 
    p2 -> path -> path
  (** [earc large cw a r pt p] is [p] with an elliptical arc from
      [p]'s last point to [pt].  The ellipse is defined by the
      horizontal and vertical radii [r] which are rotated by [a] with
      respect to the current coordinate system. If the parameters do not
      define a valid ellipse (points coincident or too far apart, zero
      radius) the arc collapses to a line.
      
      In general the parameters define four possible arcs, thus
      [large] indicates if more than pi radians of the arc is to be
      traversed and [cw] if the arc is to be traversed in the
      clockwise direction (both default to [false]). In the following
      image, in red, the elliptical arc from the left point to the
      right one.  The top row is [~large:false] and the left column
      is [~cw:false]:
      {%html: <img src="doc-earcs.png" style="width:75mm; height:45mm;"/> %}
  *)
    
  val close : path -> path
  (** [close p] is [p] with a straight line from [p]'s last point to
      [p]'s current subpath starting point, this ends the subpath. *)
    
  (** {2 Derived subpaths} 
      
      The following convenience functions start and close a new subpath
      to the given path. *)
    
  val circle : ?rel:bool -> p2 -> float -> path -> path
  (** [circle c r p] is [p] with a circle subpath of center [c] 
      and radius [r]. *)
    
  val ellipse : ?rel:bool -> ?angle:float -> p2 -> size2 -> path -> path
  (** [ellipse c r p] is [p] with an axis-aligned (unless [angle] is specified) 
      ellipse subpath of center [c] and radii [r].*)
    
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
  
  val tr : Gg.m3 -> path -> path 
  (** [tr m p] is the affine transform in homogenous 2D space of the path
      [p] by [m]. 
      
      {b Bug.} Elliptical arcs transformation is currently broken if
      [m] doesn't scale uniformely or shears. *)

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
    | `Earc of bool * bool * float * size2 * p2
    (** Elliptic arc to point, [large], [cw], [angle], [raddii] and the point *)
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
    
    Consult their {{!semimages}semantics}. 

    The composition operator {!Vg.(>>)} is used to compose images. 
    For this reason image combinators always take the
    image to use as the last argument. *)
module I : sig


  (** {1 Images} *)

  type t = image
  (** The type for images. *)

  val void : image
  (** [void] is [const ]{!Gg.Color.void}, an invisible black image. 
      [void] is an identity element for {!blend}. *)

  (** {1:prims Primitive images} *)

  val const : color -> image
  (** [const c] is an image of color [c].
      {ul {- \[[const c]\]{_[pt]} [= c] for any [pt].}} *)

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

      {ul {- \[[radial stops ~f c r]\]{_[p]} [=] \[[stops]\]{_[t]} if 
      [t] is the smallest value such that [p] is
      on the circle defined by radius [t * r] and center [f + t * (c - f)].}} *)

(*
  val raster : box2 -> Raster.t -> image 
  (** [raster r ri] is an image with [ri] framed in the rectangle
      [r] 

      To define: proper semantics and filtering.
      {ul 
      {- \[[raster r ri]\]{_[p]} [=] if [p] is in {{!Gg.aboxes}S([r])}.}
      {- \[[raster r ri]\]{_[p]} [= Gg.color.void] otherwise.}} *)
*)

  (** {1:cut Cutting images} *)

  val cut : ?area:P.area -> path -> image -> image
  (** [cut area p i] is [i] with the {{!sempaths}area} outside 
      \[[a], [p]\] cut out, i.e. mapped to {!Gg.Color.void}. [area]
      defaults to {{!P.area}[`Anz]}.
      {ul 
      {- \[[cut area p i]\]{_[pt]} [=] \[[i]\]{_[pt]} if \[[a], [p]\]{_[pt]}}
      {- \[[cut area p i]\]{_[pt]} [=] {!Gg.Color.void} otherwise.}} 

      {b Warning.} For {e outline} cuts most renderers support only
      cutting into {!const} {!axial} and {!radial} images. Consult
      the individual renderer documentation. *)

  val cut_glyphs : ?area:[ `O of P.outline ] -> ?text:string -> 
    ?blocks:bool * (int * int) list ->
    ?advances:v2 list -> (* ?o:p2 -> *)
    font -> glyph list -> image -> image
  (** {b WARNING.} The interface and specifics of glyph rendering are 
      still subject to change in the future.
 
      [cut_glyphs area text blocks advances font glyphs i] is like 
      {!cut} except the path cut is the union of all the paths of the 
      glyphs [glyphs] of the font [font].

      The origin of the first glyph is set to [P2.o], the origin of
      each subsequent glyph in [glyphs] is offset by the advance
      vector of the previous glyph as provided by [font]. Advance
      vectors for each glyph can be overriden by providing their value
      in [advances], if the length of [advances] is smaller than 
      [glyphs] the rendering results are undefined. 

      If provided the [text] parameter indicates the UTF-8 text
      corresponding to the sequence of glyphs. This may be used by
      certain renderer to allow text search in the result or to draw
      the text if it lacks control over glyph rendering (in which case
      an empty list of glyphs may be passed). 

      If provided [blocks] is used to specify a sequential map between
      [glyphs] and the characters of [text]. The number of elements in
      the list defines the number of blocks. Starting at the head of
      each sequence, each block [(char_adv, glyph_adv)] indicates the
      number of characters and glyphs that make the next block (one or
      the other may be 0). If the boolean is [true] the glyph sequence
      is reversed for peforming the map. If [blocks] is unspecified a
      one to one map between glyphs and characters is assumed with
      undefined results if the number of glyphs and characters differ.

      If [area] is provided, the outline area of the glyphs are cut as
      specified, otherwise the area of the glyphs is determined as 
      mandated by the font. {b Warning.} Backend support is poor 
      this may be removed in the future. *)

  (** {1:blend Blending images} *)
  
  val blend : image -> image -> image 
  (** [blend src dst] is [src] blended over [dst] using source over
      destination alpha blending.
      {ul 
      {- \[[blend src dst]\]{_[p]} [=] [Color.blend]  \[[src]\]{_[p]}  
      \[[dst]\]{_[p]}}} *)

  (** {1:transf Transforming images} *)

  val move : v2 -> image -> image
  (** [move v i] is [i] translated by [v].
      {ul {- \[[move v i]\]{_[pt]} [=] \[[i]\]{_[pt-v]} for any [pt]}} *)

  val rot : float -> image -> image
  (** [rot a i] is [i] rotated by [a]. 
      {ul {- \[[rot a i]\]{_[pt]} [=] \[[i]\]{_[m⋅pt]} for any [pt] and with
       [m = M2.rot -a].}} *)

  val scale : v2 -> image -> image
  (** [scale s i] is [i] scaled by [s].
      {ul {- \[[scale s i]\]{_[(x,y)]} [=] \[[i]\]{_[(x/s]{_x}[,y/s]{_y}[)]} 
      for any [(x,y)]}} *)

  val tr : m3 -> image -> image
  (** [tr m i] is the affine transform in homogenous 2D space of 
      each point of [i] by [m] (see {!Gg.P2.tr}). 
      {ul {- \[[tr m i]\]{_[pt]} [=] \[[i]\]{_[m]{^-1}⋅[pt]} for any [pt]}} *)

  (** {1:predicates Predicates and comparisons} 

      {b Note.} These predicates consider the structure of image
      values not their denotational interpretation; a single
      denotational interpretation can have many structural
      representations. *)

  val is_void : image -> bool 
  (** [is_void i] is [i == void]. *)

  val equal : image -> image -> bool
  (** [equal i i'] is [i = i']. *)

  val equal_f : (float -> float -> bool) -> image -> image -> bool
  (** [equal eq i i'] is like {!equal} but uses [eq] to test floating
      point values.

      {b Note.} Raster images are tested with {!Gg.Raster.equal}. *)

  val compare : image -> image -> int   
  (** [compare i i'] is [Pervasives.compare i i']. *) 

  val compare_f : (float -> float -> int) -> image -> image -> int
  (** [compare_f cmp i i'] is like {!compare} but uses [cmp] to
      compare floating point values. 

      {b Note.} Raster images are tested with {!Gg.Raster.compare}. *)

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

(** {1:renderers Image renderers} *)

type renderer
(** The type for image renderers. *)

(** Image renderers. 

    Renderers renders finite rectangular regions of images on
    rectangular targets. The following renderers are distributed with
    the library:
    {ul 
    {- {!Vgr_pdf}, renders sequence of images as a multi-page PDF 1.7 document.}
    {- {!Vgr_svg}, renders a single image as an 
       {{:http://www.w3.org/TR/SVG11/}SVG 1.1} document.}
    {- {!Vgr_htmlc}, renders sequence of images on an 
       {{:http://www.w3.org/TR/2dcontext/}HTML canvas} 
       via {{:http://ocsigen.org/js_of_ocaml/}js_of_ocaml}.}} *) 
module Vgr : sig

  (** {1:warnings Render warnings} 

      Renderers do their best to support [Vg]'s rendering model and
      semantics. However they may sometimes lack capabilities provided
      by [Vg]'s rendering model. 

      Whenever a renderer encounters an unsupported capability it
      ignores it and calls the [warn] callback specified at renderer
      {{!Vgr.create}creation} time. The documentation of renderers
      indicate which warnings they report. *)

  type warning = 
    [ `Unsupported_cut of P.area * I.t
    | `Unsupported_glyph_cut of P.area * I.t
    | `Textless_glyph_cut of I.t
    | `Other of string ]
  (** The type for render warnings. *)

  val pp_warning : Format.formatter -> warning -> unit
  (** [pp_warning ppf w] prints a textual representation of [w] on [ppf]. *)

  type warn = warning -> unit 
  (** The type for warning callbacks. *)

  (** {1:metadata Render metadata} 

      Some renderers support the specification of metadata as an XML
      {{:http://www.adobe.com/devnet/xmp.html}XMP metadata} packet 
      (ISO 16684-1).
      The following convenience function returns a well-formed, correctly 
      escaped, metadata packet according to the information you provide. *)

  val xmp : ?title:string -> ?authors:string list -> 
    ?subjects:string list -> ?description:string -> ?rights:string -> 
    ?creator_tool:string -> ?create_date:float -> unit -> string
  (** [xmp title authors creator_tool subject description create_date]
      is an XML XMP metadata packet. 
      {ul 
      {- [title] is mapped to dc:title.}
      {- [authors] is mapped to dc:creator.} 
      {- [subjects] is mapped to dc:subject.} 
      {- [description] is mapped to dc:description.}
      {- [rights] is mapped to dc:rights.}
      {- [creator_tool] is mapped to xmp:CreatorTool.}
      {- [create_date] (a POSIX timestamp in seconds) is mapped to 
         xmp:CreateDate.}} 

      {b Note.} All strings must be UTF-8 encoded. Unicode characters
      that are not legal XML
      {{:http://www.w3.org/TR/REC-xml/#NT-Char}characters} are
      replaced by the Unicode
      {{:http://unicode.org/glossary/#replacement_character}replacement
      character} *)

  (** {1:renderable Renderable} 

      A renderable specifies a finite view rectangle and a physical
      size for an image render. It implicitly defines the mapping
      between [Vg]'s coordinate space and the render target, see
      {{!coordinates} this section} for more information. *)

  type renderable = size2 * box2 * image
  (** The type for renderables. The physical size on the render target
      in millimeters, the view rectangle and the image to render. *)

  (** {1:render Rendering} *)

  type dst_stored = 
    [ `Buffer of Buffer.t | `Channel of Pervasives.out_channel | `Manual ]
  (** The type for stored renderer destination. With a [`Manual]
      destination the client must provide output storage see
      {!Manual.dst}. *)

  type dst = [ dst_stored | `Other ]
  (** The type for renderer destinations. Either a stored destination
      or an [`Other] destination, usually denoting some kind of
      interactive renderer. *)

  type 'a target constraint 'a = [< dst ]
  (** The type for render targets. The type parameter specifies the supported
      type of destinations. Values of this type are provided by concrete 
      renderer implementation. *)

  type t = renderer
  (** The type for renderers. *)

  val create : ?limit:int -> ?warn:warn ->
    ([< dst] as 'dst) target -> 'dst -> renderer
  (** [create limit warn target dst] is a renderer using [target] to render 
      images to [dst]. [warn] is called whenever the renderer lacks a 
      capability, see {{!warnings}warnings}.

      [limit] limits the time spent in the {!render} function (defaults to
      [max_int], unlimited).  The cost model may change in a future
      version of the library. For now each image combinator costs one
      unit, when the limit is reached {!render} returns with
      [`Partial].  *)

  val render : renderer -> [< `Image of renderable | `Await | `End ] -> 
    [ `Ok | `Partial ]
  (** [render r v] is:
      {ul
      {- [`Partial] iff [r] has a [`Manual] destination and needs more
         output storage or if [r] has a limit. In the first
         case the client must use {!Manual.dst} to provide
         a new buffer. In both cases the client should then 
         call {!render} with [`Await] until
         [`Ok] is returned.}
      {- [`Ok] when the encoder is ready to encode a new [`Image] (if
         the renderer supports it) or [`End].}}  For [`Manual]
         destinations, encoding [`End] always returns [`Partial] the
         client should as usual use {!Manual.dst} and continue with
         [`Await] until [`Ok] is returned at which point
         {!Manual.dst_rem}[ r] is guaranteed to be the size of the
         last provided buffer (i.e. nothing was written).

         {b Semantics of multiple images render.} The semantics
         of multiple image renders are left to the backend.
       
         @raise Invalid_argument if [`Image] or [`End] is encoded after
         a [`Partial] encode. Or if multiple [`Image]s are encoded in 
         a renderer that doesn't support them. *)
       
  val renderer_dst : renderer -> dst
  (** [render_dst r] is [r]'s destination. *)

  val renderer_limit : renderer -> int 
  (** [renderer_limit r] is [r]'s limit. *)

  (** {1 Manual render destinations} *)

  (** Manual render destinations. 

      {b Warning.} Only use with renderers with [`Manual] destinations. *)
  module Manual : sig
    val dst : renderer -> string -> int -> int -> unit 
    (** [dst r s j l] provides [r] with [l] bytes to write, starting
        at [j] in [s]. This byte range is written by calls to {!render}
        until [`Partial] is returned. Use {!dst_rem} to know the remaining
        number of non-written free bytes in [s]. *)
      
    val dst_rem : renderer -> int 
    (** [dst_rem r] is the remaining number of non-written, free bytes
        in the last buffer provided with {!dst}. *)
  end

  (** {1:renderer  Implementing renderers} *)

  (** Private functions for implementing renderers.

      {b Warning.}
      [Vg] users should not use these definitions, they exposes [Vg]'s
      internals for implementing renderers.  This functionality is
      subject to change even between minor versions of the
      library.

      In order to provide a consistant interface for [Vg] users,
      renderer writers should follow the guidelines below.  You may
      want to drop an email to %%MAINTAINER%% for help and discussion.
      {ul
      {- If you render to "Bla", define you renderer in a module
         called [Vgr_bla] (lowercase).}
      {- The renderer target creation function must be named
         [Vgr_bla.target].}
      {- Images must be rendered via the {!render} function. If you 
         are writing a batch renderer provide support for each of the 
         {!dst} types and especially the non-blocking interface.}
      {- Respect Vg's linear sRGB color model.}
      {- Whenever possible use an XMP metadata packet for metadata, 
         see {!Vgr.xmp}.}
      {- The renderer should implement the rendering cost model, 
         see the [limit] parameter of {!render}.} 
      {- Follow [Vg]'s 
         {{!coordinates}coordinate system conventions} to 
         specify the relationship between a target and the view 
         rectangle to render.}
      {- If the renderer doesn't support [Vg]'s full rendering model or
         diverges from its semantics it must ignore unsupported features
         and warn the client via the {!warn} function.}} *)
  module Private : sig
    
    (** {1 Internal data} *)

    (** Internal data. *)
    module Data : sig
      
      (** {1 Path representation} *)

      type segment = 
        [ `Sub of p2
        | `Line of p2
        | `Qcurve of p2 * p2 
        | `Ccurve of p2 * p2 * p2 
        | `Earc of bool * bool * float * size2 * p2
        | `Close ]
      (** The type for path segments. *)
      
      type path = segment list
      (** The type for paths. The segment list is reversed. A few invariants
          apply. See the comment in [Vg]'s source. *)

      val of_path : P.t -> path
      (** [of_path p] is the internal representation of [p]. *)

      (** {1 Image representation} *)

      (** The type for transforms. Not uniformely expressed as a
          matrix since renderers may have shorter syntaxes for some
          transforms. *)
      type tr = Move of v2 | Rot of float | Scale of v2 | Matrix of m3
    
      val tr_to_m3 : tr -> M3.t
      (** [tr_to_m3 tr] is the matrix of [tr]. *)

      val inv_tr_to_m3 : tr -> M3.t 
      (** [inv_tr_to_m3 tr] is the matrix inverse of [tr]. *)

      type blender = [ `Atop | `In | `Out | `Over | `Plus | `Copy | `Xor ] 

      (** The type for image primitives. *)    
      type primitive = 
        | Const of color
        | Axial of Color.stops * p2 * p2
        | Radial of Color.stops * p2 * p2 * float
        | Raster of box2 * raster
                  
      (** The type for glyph runs. *)
      type glyph_run =
        { font : font;
          text : string option; 
          o : p2;                          (** Unused for now, always P2.o *)
          blocks : bool * (int * int) list;
          advances : v2 list; 
          glyphs : glyph list; }

      (** The type for images. *)
      type image =
        | Primitive of primitive
        | Cut of P.area * path * image
        | Cut_glyphs of P.area * glyph_run * image 
        | Blend of blender * float option * image * image
        | Tr of tr * image
                    
      val of_image : I.t -> image 
      (** [of_image i] is the internal representation of [i]. *)
    end

    (** Font helpers. *)
    module Font : sig
  
      val css_font : unit:string -> font -> string 
      (** [css_font unit font] is a CSS
          {{:http://www.w3.org/TR/CSS2/fonts.html#font-shorthand}font property}
          for the font with size expressed in [unit]. *)
      
      val css_weight : font -> string 
      (** [css_weight font] is [font]'s weigth as CSS [font-weight] value. *)

      val css_slant : font -> string 
      (** [css_slant font] is [font]'s slant as a CSS [font-style] value. *)
    end

    (** Paths helpers. *)
    module P : sig
      val of_data : Data.path -> P.t 
      (** [of_data d] is the path from the internal representation [d]. *)

      val earc_params : p2 -> large:bool -> cw:bool -> float -> v2 -> p2 -> 
        (p2 * m2 * float * float) option 
        (** [earc_params p large cw angle r p'] is [Some (c, m, a, a')] 
            with [c] the center of the ellipse, [m] a transform matrix 
            mapping the unit circle to the ellipse, [a] and [a'] the 
            angle on the unit circle corresponding to the first and last
            point of the arc. [None] is returned if the parameters do not
            define a valid arc. *)

      val miter_limit : P.outline -> float 
      (** [miter_limit o] is the miter limit corresponding to 
          [o.miter_angle]. *)
    end

    (** Image helpers *)
    module I : sig
      val of_data : Data.image -> I.t
      (** [of_data d] is the image from the internal representation [d]. *)
    end

    (** {1 Renderers } *)
    
    type renderer
    (** The type for renderers. *)
            
    type k = renderer -> [ `Ok | `Partial ] 
    (** The type for renderer continuations. *)
                         
    type render_fun = [`End | `Image of size2 * box2 * Data.image ] -> k -> k 
    (** The type for rendering functions. *)
      
    type 'a render_target = renderer -> 
      'a -> bool * render_fun constraint 'a = [< dst]
    (** The type for render targets. The function takes a created
        renderer and its destination. It should return a boolean
        indicating whether multiple images can be rendered on the
        target, and a function that is invoked by the renderer to
        render a new image or end the rendering sequence. *)

    val create_target : 'a render_target -> 'a target
    (** [create_target t] makes an end-user render target from [t]. *)

    val renderer : t -> renderer
    (** [renderer r] is [r]'s internal representation. *)
            
    val limit : renderer -> int
    (** [limit r] is [r]'s render limit. *)
      
    val warn : renderer -> warning -> unit
    (** [warn r w] reports the warning [w] on [r]. *)       
      
    val partial : k -> renderer -> [> `Partial]
    (** [partial k r] suspends the renderer [r] and returns [`Partial]. 
        Rendering will continue with [k r], on {!render} [`Await]. *)
                                   
    (** {1 Writing {!dst_stored} destinations} *)
                                   
    val flush : k -> renderer -> [ `Ok | `Partial ]
    (** [flush k r] flushes the renderer [r]. If [r] writes 
        on a stored destination this function {b must} be called
        by the rendering function on [`End]. *)
                                 
    val writeb : int -> k -> renderer -> [ `Ok | `Partial ]
    (** [writeb b k r] writes the byte [b] and [k]ontinues. *)
                                         
    val writes : string -> int -> int -> k -> renderer -> [ `Ok | `Partial ]
    (** [writes s j l k r]  writes [l] bytes from [s] starting at [j]
        and [k]ontinues. *)
                                                          
    val writebuf : Buffer.t -> int -> int -> k -> renderer -> [`Ok | `Partial ]
    (** [writebuf buf j l k r] write [l] bytes from [buf] starting at [j]
        and [k]ontinues. *)

    (** {1 Miscellaneous} *) 

    val add_xml_data : Buffer.t -> string -> unit 
    (** [add_xml_data b s] adds [s] to [b], escapes 
        ['<', '>', '&'] and ['"'] (but {b not} single quotes) and 
        maps illegal XML unicode characters to the replacement character
        U+FFFD. *)
  end
end

(** {1:basics Basics} 

    [Vg] is designed to be opened in your module. This defines only
    types and modules in your scope and a {e single} value, the
    composition operator {!(>>)}. Thus to use [Vg] start with :
{[
open Gg
open Vg
]}
    {!Gg} gives us types for points ({!Gg.p2}), vectors ({!Gg.v2}), 2D
    extents ({!Gg.size2}), rectangles ({!Gg.box2}) and colors
    ({!Gg.color}). Later you may want to read {!Gg}'s documentation
    {{!Gg.basics}basics} but for now it is sufficient to know that each of these
    types has a constructor [v] in a module named after the
    capitalized type name ({!Gg.P2.v}, {!Gg.V2.v}, etc.).

    {2 A collage model}

    Usual vector graphics libraries follow a {e painter model} in
    which paths are filled, stroked and blended on top of each other
    to produce a final image. [Vg] departs from that, it has a {e
    collage model} in which paths define 2D areas in infinite images
    that are {e cut} to define new infinite images to be blended on
    top of each other.

    The collage model maps very well to a declarative imaging model.
    It is also very clear from a specification point of view, both
    mathematically and metaphorically. This cannot be said from the
    painter model where the semantics of an operation like stroking a
    self-intersecting translucent path —  which usually applies the
    paint only once —  doesn't directly map to the underlying paint
    stroke metaphor. The collage model is also more economical from a
    conceptual point view since image cuts and blends naturally unify
    the distinct concepts of clipping paths, path strokes, path fills
    and compositing groups (unsupported for now in [Vg]) of the
    painter model.

    The collage model introduced in the following sections was stolen
    and adapted from the following works.
    {ul 
    {- Conal Elliott. 
    {e {{:http://conal.net/papers/bridges2001/}Functional Image
    Synthesis}}, Proceedings of Bridges, 2001.}
    {- Antony Courtney. {e Haven : Functional Vector Graphics}, chapter 6
    in
    {{:http://web.archive.org/web/20060207195702/http://www.apocalypse.org/pub/u/antony/work/pubs/ac-thesis.pdf}Modeling
    User Interfaces in a Functional Language}, Ph.D. Thesis, Yale
    University, 2004.}}

    {2 Infinite images}

    Images in [Vg] are immutable and abstract value of type
    {!image}. {e Conceptually}, images are seen as functions mapping
    points of the infinite 2D plane to colors:

    [type Vg.image ] ≈  [Gg.p2 -> Gg.color]

    The simplest image is a constant image: an image that associates
    the same color to every point in the plane. For a constant 
    gray of intensity 0.5 this would be expressed by the function: 
{[
fun _ -> Color.gray 0.5
]}
    In [Vg] the combinator {!I.const} represents constant infinite images 
    and the above function is written:
{[
let gray = I.const (Color.gray 0.5)
]}
    The module {!I} contains all the combinators to define and compose
    infinite images, we will explore some of them later. But for now 
    let's just render that fascinating image. 

    {2 Rendering}

    An infinite image alone cannot be rendered. We need a {e finite}
    view rectangle and a specification of that view's physical size on
    the render target. These informations are coupled together with an
    image to form a {!Vgr.renderable}.

    Renderables can be given to a renderer for display via the
    function {!Vgr.render}.  Renderers are created with {!Vgr.create}
    and need a {{!Vgr.target}render target} value that defines the
    concrete renderer implementation used (PDF, SVG, HTML canvas etc.).

    The following function outputs the unit square of [gray] on a
    30x30 millimeters SVG target in the file [/tmp/vg-basics.svg]:
{[let svg_of_usquare i = 
  let size = Size2.v 30. 30. in
  let view = Box2.unit in
  try
    let oc = open_out "/tmp/vg-basics.svg" in
    let r = Vgr.create (Vgr_svg.target ()) (`Channel oc) in
    try 
      ignore (Vgr.render r (`Image (size, view, i)));
      ignore (Vgr.render r `End);
      close_out oc
    with e -> close_out oc; raise e
  with Sys_error e -> prerr_endline e

let () = svg_of_usquare gray]}
    The result should be an SVG image with a gray square 
    like this:
{%html: <img src="doc-gray-square.png" style="width:30mm; height:30mm;"/> %}

    {2:coordinates Coordinate space} 

    [Vg]'s cartesian coordinate space has its origin at the bottom
    left with the x-axis pointing right, the y-axis pointing up. It
    has no units, you define what they mean to you. However a
    {{!Vgr.renderable}renderable} implicitely defines a physical unit
    for [Vg]'s coordinate space: the corners of the specified view
    rectangle are mapped on a rectangular area of the given physical
    size on the target.

    {2 Scissors and glue}

    Constant images can be boring. To make things more interesting
    [Vg] gives you scissors: the {!I.cut} combinator. 

    This combinator takes a finite area of the plane defined by a path
    [path] (more on paths later) and a source image [img] to define the
    image [I.cut path img] that has the color of the source image in the
    area defined by the path and the invisible transparent black color
    ({!Gg.Color.void}) everywhere else. In other words [I.cut path img]
    represents this function: 
{[
fun pt -> if inside path pt then img pt else Color.void
]}
    The following code cuts a circle of radius [0.4] centered in the 
    unit square in the [gray] image defined before.
{[
let circle = P.empty >> P.circle (P2.v 0.5 0.5) 0.4 
let gray_circle = I.cut circle gray 
]}
    Rendered by [svg_of_usquare] the result is:

{%html: <img src="doc-gray-circle.png" style="width:30mm; height:30mm;"/> %}

    Note that the background white color surrounding the circle does
    not belong to the image itself, it is the color of the webpage
    background against which the image is composited. Your eyes
    require a wavelength there and {!Gg.Color.void} cannot provide it.

    {!I.cut} has an optional [area] argument of type {!P.area} that
    determines how a path should be interpreted as an area of the
    plane. The default value is [`Anz], which means that it uses the
    non-zero winding number rule and for [circle] that defines its
    interior.

    But the [circle] path can also be seen as defining a thin outline
    area around the ideal mathematical circle of [circle]. This can be
    specified by using an outline area `O o. The value [o] of type
    {!P.outline} defines various parameters that define the outline
    area; for example its width. The following code cuts the [circle]
    outline area of width [0.04] in an infinite black image.

{[
let circle_outline = 
  let area = `O { P.o with P.width = 0.04 } in 
  let black = I.const Color.black in 
  I.cut ~area circle black
]}

    Below is the result and again, the white you see here is in 
    fact {!Gg.Color.void}. 

{%html: <img src="doc-circle-outline.png" style="width:30mm; height:30mm;"/> %}

    {!I.cut} gives us scissors but to combine the results of cuts we
    need some glue: the {!I.blend} combinator. This combinator takes
    two infinite images [front] and [back] and defines an image
    [I.blend front back] that has the colors of [front] alpha blended
    on top of those of [back]. [I.blend front back] represents
    this function: 
{[
let i' = fun pt -> Color.blend (front pt) (back pt) 
]}
    If we blend [circle_outline] on top of [gray_circle]:
{[
let dot = I.blend circle_outline gray_circle
]}
    We get:

{%html: <img src="doc-dot.png" style="width:30mm; height:30mm;"/> %}

    The order of arguments in {!I.blend} is defined so that images can
    be blended using the left-associative composition operator
    {!Vg.(>>)}. That is [dot] can also be written as follows:
{[
let dot = gray_circle >> I.blend circle_outline
]}  

    This means that with {!Vg.(>>)} and {!I.blend} left to right order in 
    code maps to back to front image blending.

    {2 Transforming images} 

    The combinators {!I.move}, {!I.rot}, {!I.scale}, and {!I.tr} allow
    to perform arbitrary
    {{:http://mathworld.wolfram.com/AffineTransformation.html}affine
    transformations} on an image. For example the image [I.move v i]
    is [i] but translated by the vector [v], that is the following
    function:
{[
fun pt -> img (V2.(pt - v)) 
]}
    The following example uses [I.move]. The function [scatter_plot]
    takes a list of points and returns a scatter plot of the
    points. First we define a [dot] around the origin, just a black
    circle of diameter [pt_width].  Second we define the function [mark]
    that given a point returns an image with [dot] at that
    point and [blend_mark] that blends a [mark] at a point on an image. 
    Finally we blend all the marks toghether.
{[
let scatter_plot pts pt_width = 
  let dot = 
    let circle = P.empty >> P.circle P2.o (0.5 *. pt_width) in
    I.const Color.black >> I.cut circle
  in
  let mark pt = dot >> I.move pt in 
  let blend_mark acc pt = acc >> I.blend (mark pt) in
  List.fold_left blend_mark I.void pts
]}
    Note that [dot] is defined outside [mark], this means that all [mark]s
    share the same [dot], doing so allows renderers to perform space 
    and time optimizations. For example the SVG renderer will output a single
    [circle] path shared by all marks. 
   
    Here's the result of [scatter_point] on 800 points with coordinates
    on independent normal distributions. 
{%html: <img src="doc-scatter-plot.png" style="width:40mm; height:40mm;"/> %}

    {2 Paths} 

    Paths are used to define areas of the plane. A path is an
    immutable value of type {!path} which is a list of disconnected
    subpaths. A {e subpath} is a list of directed and connected curved
    segments.

    To build a path you start with the empty path {!P.empty}, give it
    to {!P.sub} to start a new subpath and give the result to
    {!P.line}, {!P.qcurve}, {!P.ccurve}, {!P.earc} or {!P.close} to
    add a new segment and so forth.

    Path combinators take the path they act upon as the last argument
    so that the left-associative operator {!Vg.(>>)} can be used to
    construct paths. 

    The image below is made by cutting the outline of the single path [p]
    defined hereafter.
{%html: <img src="doc-subpaths.png" style="width:30mm; height:30mm;"/> %}
{[
let p =
  let rel = true in
  P.empty >> 
  P.sub (P2.v 0.1 0.5) >> 
    P.line (P2.v 0.3 0.5) >> 
    P.qcurve ~rel (P2.v 0.2 0.5) (P2.v 0.2 0.0) >> 
    P.ccurve ~rel (P2.v 0.0 (-. 0.5)) (P2.v 0.1 (-. 0.5)) (P2.v 0.3 0.0) >> 
    P.earc ~rel (Size2.v 0.1 0.2) (P2.v 0.15 0.0) >> 
  P.sub (P2.v 0.18 0.26) >> 
    P.qcurve ~rel (P2.v (0.01) (-0.1)) (P2.v 0.1 (-. 0.05)) >> 
    P.close >> 
  P.sub (P2.v 0.65 0.8) >> 
    P.line ~rel (P2.v 0.1 (-. 0.05))
in
let area = `O { P.o with P.width = 0.01 } in
I.const Color.black >> I.cut ~area p
]}

    Except for {!P.close} which has no other argument but a path, the
    last point argument before the path argument is always the concrete end
    point of the segment. When [true] the optional [rel] argument
    indicates that the coordinates given to the constructor are
    expressed relative to end point of the last segment (or [P2.o] if
    there is no such segment).

    Note that after a [P.close] or on the [P.empty] path, the 
    call to {!P.sub} can be omitted. In that case an implicit 
    [P.sub P2.o] is introduced.

    For more information about how paths are intepreted as 
    areas, consult their {{!sempaths}semantics}. 

    {2:remarkstips Remarks and tips}
    {ul
    {- Angles follow [Gg]'s {{!Gg.mathconv}conventions}.}
    {- Matrices given to {!P.tr} and {!I.tr} are supposed to 
       be affine and as such ignore the last row of the matrix.} 
    {- [to_string] functions are not thread-safe. Thread-safety
       can be achieved with [pp] functions.}
    {- Do not rely on the output of printer functions, they
       are subject to change.}
    {- Rendering results are undefined if path
       or image data contains NaNs or infinite floats.}
    {- Any string is assumed to be UTF-8 encoded.}
    {- Sharing (sub)image, path and outline
       values in the definition of an image may result in more
       efficient rendering in space and time.}}
*)

(** {1:semantics Semantics}

    The following notations and definitions are used to give precise
    meaning to the images and the combinators.

    {2:semcolors Colors}

    The semantics of colors is the one ascribed to
    {{!Gg.Color.t}[Gg.color]}: colors are in a {e linearized} sRGBA space.

    {3:semstops Color stops} 

    A value of type {!Gg.Color.stops} specifies a color at each point
    of the 1D {e unit} space. It is defined by a list of pairs
    [(t]{_i}[, c]{_i}[)] where [t]{_i} is a value from [0] to [1] and
    [c]{_i} the corresponding color at that value. Colors at points
    between [t]{_i} and [t]{_i+1} are linearly interpolated between
    [c]{_i} and [c]{_i+1}. If [t]{_i} lies outside [0] to [1]
    or if [t]{_i-1} >= [t]{_i} the semantics is undefined.

    Given a stops value [stops = \[][(t]{_0}[, c]{_0}[);]
    [(t]{_1}[,c]{_1}[);] ... [(t]{_n}[, c]{_n}[)][\]] and any point
    [t] of 1D space, the semantic function: 
    
    \[\] [: Gg.Color.stops -> float -> Gg.color] 
    
    maps them to a color value written \[[stops]\]{_t}
    as follows.

    {ul
      {- \[[]\]{_t} = [(0, 0, 0, 0)] for any [t]}
      {- \[[stops]\]{_t} [= c]{_0} if [t < t]{_0}.}
      {- \[[stops]\]{_t} [= c]{_n} if [t >= t]{_n}.}
      {- \[[stops]\]{_t} [= (1-u)c]{_i}[ + uc]{_i+1} 
      with [u = (t - t]{_i}[)/(t]{_i+1}[-t]{_i}[)]
      if [t]{_i} [<= t <] [t]{_i+1}}}

    {2:semimages Images}    

    Values of type {!image} represent maps from the infinite
    2D euclidian space to {{!semcolors}colors}. Given an image [i] and 
    a point [pt] of the plane the semantic function

    \[\][: image -> Gg.p2 -> Gg.color] 

    maps them to a color value written \[[i]\]{_[pt]} representing the
    image's color at this point.

    {2:sempaths Paths and areas}
    
    A value of type {!path} is a list of subpaths. A subpath is a list
    of {e directed} and connected curved {e segments}. Subpaths are
    disconnected from each other and may (self-)intersect.

    A path and a value of type {!P.area} defines a finite area of the
    2D euclidian space. Given an area specification [a], a path [p]
    and a point [pt], the semantic function:

    \[\]: [P.area -> path -> Gg.p2 -> bool] 

    maps them to a boolean value written \[[a], [p]\]{_[pt]}
    that indicates whether [pt] belongs to the area or not. 

    The semantics of area rules is as follows:
    {ul

    {- \[[`Anz], [p]\]{_[pt]} is [true] iff the winding number of [p]
        around [pt] is non zero. To determine the winding number cast
        a ray from [pt] to infinity in any direction (just make sure
        the ray doesn't intersect [p] tangently or at a
        singularity). Starting with zero add one for each intersection
        with a counter-clockwise oriented segment of [p] and substract
        one for each clockwise ones. The resulting sum is the
        winding number. This is usually refered to as the {e non-zero winding 
        rule} and is the default for {!I.cut}.
{%html: <img src="doc-anz.png" style="width:90mm; height:30mm;"/> %}}
    {- \[[`Aeo], [p]\]{_[pt]} is [true] iff the number of
        intersections of [p] with a ray cast from [pt] to infinity in
        any direction is odd (just make sure the ray doesn't intersect
        [p] tangently or at a singularity). This is usually refered
        to as the {e even-odd rule}.
{%html: <img src="doc-aeo.png" style="width:90mm; height:30mm;"/> %}}
    {- \[[`O o], [p]\]{_[pt]} is [true] iff [pt] is in the outline 
        area of [p] as defined by the value [o] of type {!type:P.outline}. 

        {4:semoutlines Outline areas}

        The outline area of a path is the union of its subpaths
        outline areas. A subpath outline area is inside the parallel
        curves at a distance [o.width / 2] of its path segments that
        are joined accoring to the join style [o.join] (see below) and
        closed at the subpath end points with a cap style [o.cap] (see
        below). The outline area of a subpath can also be chopped at
        regular intervals according to the [o.dashes] parameter (see
        below).

        {4:semjoins Segment jointures}

        The shape of subpath segment jointures is specified in 
        [o.join] by a value of type {!P.join}. From left to right:
{%html: <img src="doc-joins.png" style="width:90mm; height:30mm;"/> %}
        {ul
        {- [`Miter], the outer parallel curves are extended until they
           meet unless the joining angle is smaller than
           [o.miter_angle] in which case the join is converted to a
           bevel.}
        {- [`Round], joins the outer parallel curves by a semicircle 
           centered at the end point with a diameter equal to [o.width].}
        {- [`Bevel], joins the outer parallel curves by a segment.}}

        {4:semcaps Subpath caps} 

        The shape of subpath (or dashes) end points is specified in
        [o.cap] by a value of type {!P.cap}. From left to right:
{%html: <img src="doc-caps.png" style="width:90mm; height:20mm;"/> %}
        {ul 
        {- [`Butt], end points are square and extend only to the 
           exact end point of the path.}
        {- [`Round], end points are rounded by a semicircle at 
           the end point with a diameter equal to [o.width].}
        {- [`Square], end points are square and extend by a distance 
           equal to half [o.width].}}

        {4:semdashes Outline dashes} 

        The path outline area can be chopped at regular intervals by
        spefiying a value [(off, pat)] of type {!P.dashes} in [o.dashes].

        The {e dash pattern} [pat] is a list of lengths that specify
        the length of alternating dashes and gaps (starting with
        dashes). The {e dash offset} [off] is a {e positive} offset
        that indicates where to start in the dash pattern at the
        beginning of a subpath.}}  *)

(** {1:examples Examples}

Many examples of images and their source can be found in the
{{:http://erratique.ch/software/vg/demos/rhtmlc.html}online version}
of [Vg]'s test image database. Clicking on the title of an image brings
you to its definition.

The following examples show for each renderer the minimal code
needed to output an image. This code can also be found in the [test]
directory of the distribution.

{2:minpdf Minimal PDF output} 

The file [min_pdf.ml] contains the following mostly self-explanatory
code. We first define an image and then render it. For the latter
step we define some meta-data for the image, a function to print
rendering warnings and then render the image on stdout.  

{[
open Gg
open Vg

(* 1. Define your image *)

let aspect = 1.618
let size = Size2.v (aspect *. 100.) 100. (* mm *)
let view = Box2.v P2.o (Size2.v aspect 1.)
let image = I.const (Color.v_srgb 0.314 0.784 0.471)

(* 2. Render *)

let () =
  let title = "Vgr_pdf minimal example" in
  let description = "Emerald Color" in
  let xmp = Vgr.xmp ~title ~description () in
  let warn w = Vgr.pp_warning Format.err_formatter w in
  let r = Vgr.create ~warn (Vgr_pdf.target ~xmp ()) (`Channel stdout) in
  ignore (Vgr.render r (`Image (size, view, image)));
  ignore (Vgr.render r `End)
]}

This can be compiled with:
{[
> ocamlfind ocamlopt -package gg,vg,vg.pdf \
                     -linkpkg -o min_pdf.native min_pdf.ml
]}

{2:minsvg Minimal code for SVG output}

The file [min_svg.ml] contains the following mostly self-explanatory
code. We first define an image and then render it. For the latter
step we define some meta-data for the image, a function to print
rendering warnings and then render the image on stdout. 

{[open Gg
open Vg

(* 1. Define your image *)

let aspect = 1.618  
let size = Size2.v (aspect *. 100.) 100. (* mm *)
let view = Box2.v P2.o (Size2.v aspect 1.)
let image = I.const (Color.v_srgb 0.314 0.784 0.471)

(* 2. Render *)

let () = 
  let title = "Vgr_svg minimal example" in 
  let description = "Emerald Color" in 
  let xmp = Vgr.xmp ~title ~description () in
  let warn w = Vgr.pp_warning Format.err_formatter w in
  let r = Vgr.create ~warn (Vgr_svg.target ~xmp ()) (`Channel stdout) in
  ignore (Vgr.render r (`Image (size, view, image))); 
  ignore (Vgr.render r `End)
]}

This can be compiled with:
{[
> ocamlfind ocamlopt -package gg,vg,vg.svg \
                     -linkpkg -o min_svg.native min_svg.ml
]}

{2:minhtmlc Minimal code for HTML canvas output}

The file [min_htmlc.ml] contains the following code. Step by step we have:
{ol
{- Define an image.}
{- Create and add to the DOM an anchor [a] that will parent the canvas.
   This will allow to download a (usually PNG) file of the image.}
{- Create a canvas element [c] and add it as a child of [a].}
{- Create a renderer [r] targeting the canvas [c].}
{- Render the image.}
{- Ask the canvas for an image data URL and set it as the the link of the 
   anchor.}}
{[
open Gg
open Vg

(* 1. Define your image *)

let aspect = 1.618  
let size = Size2.v (aspect *. 100.) 100. (* mm *)
let view = Box2.v P2.o (Size2.v aspect 1.)
let image = I.const (Color.v_srgb 0.314 0.784 0.471)

(* Browser bureaucracy. *)

let main _ = 
  let d = Dom_html.window ## document in 
  let a = (* 2 *)
    let a = Dom_html.createA d in 
    a ## title <- Js.string "Download PNG file";
    a ## href <- Js.string "#"; 
    a ## setAttribute (Js.string "download", Js.string "min_htmlc.png");
    Dom.appendChild (d ## body) a; a
  in 
  let c = (* 3 *)
    let c = Dom_html.createCanvas d in 
    Dom.appendChild a c; c
  in 
  let r = Vgr.create (Vgr_htmlc.target c) `Other in   (* 4 *)
  ignore (Vgr.render r (`Image (size, view, image))); (* 5 *)
  ignore (Vgr.render r `End);
  a ## href <- (c ## toDataURL ()); (* 6 *)
  Js._false

let () = Dom_html.window ## onload <- Dom_html.handler main
]}

This file needs to be compiled to byte code and then [js_of_ocaml]
must be applied. This can be achieved with:
{[> ocamlfind ocamlc \
  -package js_of_ocaml,js_of_ocaml.syntax \
  -package gg,vg,vg.htmlc \
  -syntax camlp4o -linkpkg -o min_htmlc.byte min_htmlc.ml \
  && js_of_ocaml min_htmlc.byte]}

Finally we need a minimal HTML file that references our final
javascript [min_htmlc.js]. The following one will do:
{v
<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width,
                                 initial-scale=1.0">
  <script type="text/javascript" defer="defer" src="min_htmlc.js"></script>
  <style type="text/css"> body \{ background-color: black; margin: 3em; \}</style>
  <title>Vgr_htmlc minimal example</title>
</head>
<body>
  <noscript>Sorry, you need to enable JavaScript to see this page.</noscript>
</body>
</html>
v}
*)

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
