(*---------------------------------------------------------------------------
   Copyright (c) 2013 The vg programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Declarative 2D vector graphics.

    Consult the {{!page-tutorial}tutorial}, the {{!page-semantics}semantics}
    and the {{!page-image_howto}image howto}.

    Open the module to use it, this defines only modules and types in
    your scope. *)

open Gg

(** {1:fonts Fonts} *)

(** Fonts.

    Font handling in [Vg] happens in renderers and text layout and
    text to glyph translations are expected to be carried out by an
    external library. Values of type {!Vg.font} just represent a font
    specification to be resolved by the concrete renderer. *)
module Font : sig

  (** {1:fonts Fonts} *)

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
  (** [compare font font'] is [Stdlib.compare font font'] *)

  val compare_f : (float -> float -> int) -> t -> t -> int
  (** [compare_f cmp font font'] is like {!compare} but uses [cmp] to compare
      floating point values. *)

  (** {1:fmt Formatters} *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf font] is a textual representation of [font] on [ppf]. *)

  val to_string : t -> string
  [@@ocaml.deprecated "Use Font.pp instead."]
  (** @deprecated use {!pp} instead. *)
end

type font = Font.t
(** The type for fonts. *)

type glyph = int
(** The type for glyphs. The integer represents a glyph identifier in a
    backend dependent font format. *)

(** {1:base Paths and images} *)

type path
(** The type for paths. *)

type image
(** The type for images. *)

(**/**)
val ( >> ) : 'a -> ('a -> 'b) -> 'b
[@@ocaml.deprecated "Use |> instead."]
(** [x >> f] is [f x], associates to left.
    Used to build paths and compose images. *)
(**/**)

(** Paths.

    Consult their {{!page-semantics.sempaths}semantics}.

    The [|>] operator is used to build paths from the empty path. For
    this reason path combinators always take the path to use as the
    last argument. *)
module P : sig

  (** {1 Path areas} *)

  type cap = [ `Butt | `Round | `Square ]
  (** The type for path caps. {{!page-semantics.semcaps}Semantics}.*)

  type join = [ `Bevel | `Miter | `Round ]
  (** The type for segment jointures. {{!page-semantics.semjoins}Semantics}.*)

  type dashes = float * float list
  (** The type for dashes. {{!page-semantics.semdashes}Semantics}. *)

  type outline =
    { width : float;          (** Outline width. *)
      cap : cap;              (** Shape at the end points of open subpaths
                                    and dashes. *)
      join : join;            (** Shape at segment jointures. *)
      miter_angle : float;    (** Limit {e angle} for miter joins
                                    (in \[0;pi\]).*)
      dashes : dashes option; (** Outline dashes. *) }
  (** The type for path outline area specifications.
      {{!page-semantics.semoutlines}Semantics}.*)

  val o : outline
  (** [o] holds a default set of values. [width] is [1.],
      [cap] is [`Butt], [join] is [`Miter], [miter_angle] is
      [11.5] degrees in radians and [dashes] is [None]. *)

  val pp_outline : Format.formatter -> outline -> unit
  (** [pp_outline ppf o] prints a textual representation of [o] on [ppf]. *)

  type area = [ `Aeo | `Anz | `O of outline ]
  (** The type for path area specifications.
      {{!page-semantics.sempaths}Semantics}.*)

  val pp_area : Format.formatter -> area -> unit
  (** [pp_area ppf a] prints a textual representation of [a] on [ppf] *)

  (** {1:paths Paths} *)

  type t = path
  (** The type for paths. *)

  val empty : path
  (** [empty] is the empty path. *)

  (** {1:subs Subpaths and segments}

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
      {%html: <img src="../../_assets/doc-earcs.png"
                   style="width:75mm; height:45mm;"/> %}
  *)

  val close : path -> path
  (** [close p] is [p] with a straight line from [p]'s last point to
      [p]'s current subpath starting point, this ends the subpath. *)

  (** {2:derived Derived subpaths}

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

  val smooth_qcurve : ?rel:bool -> p2 -> path -> path
  (** [smooth_qcurve pt p] is [qcurve c pt p] with control point [c]
      defined as the reflexion of the control point of the previous
      quadratic curve relative to the last point of [p]. If the
      previous segment of [p] is not a quadratic curve, [c] is
      the last point of [p] or the origin if the path is empty. *)

  val smooth_ccurve : ?rel:bool -> p2 -> p2 -> path -> path
  (** [smooth_ccurve c' pt p] is [ccurve c c' pt p] with control point
      [c] defined as the reflexion of the second control point of the
      previous cubic curve relative to the last point of [p]. If the
      previous segment of [p] is not a cubic curve, [c] is the last point
      of [p] or the origin if the path is empty. *)

  (** {1:funs Functions} *)

  val last_pt : path -> p2
  (** [last_pt p] is the last point of [p]'s last subpath.
      Raises [Invalid_argument] if [p] is [empty]. *)

  val append : path -> path -> path
  (** [append p' p] appends [p'] to [p]. If [p]'s last subpath had no
      segment it is closed.

      {b Warning.} To accomodate [|>] the argument order is the opposite of
      {!List.append}. *)

  val tr : Gg.m3 -> path -> path
  (** [tr m p] is the affine transform in homogenous 2D space of the path
      [p] by [m].

      {b Bug.} Elliptical arcs transformation is currently broken if
      [m] doesn't scale uniformely or shears. *)

  (** {1:traversal Traversal} *)

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
  (** [compare p p'] is {!Stdlib.compare}[ p p']. *)

  val compare_f : (float -> float -> int) -> path -> path -> int
  (** [compare_f cmp p p'] is like {!compare} but uses [cmp] to compare
      floating point values. *)

  (** {1:fmt Formatters} *)

  val pp : Format.formatter -> path -> unit
  (** [pp ppf p] prints a textual representation of [p] on [ppf]. *)

  val pp_f : (Format.formatter -> float -> unit) -> Format.formatter ->
    path -> unit
  (** [pp_f pp_float ppf p] prints [p] like {!pp} but uses [pp_float] to
      print floating point values. *)

  val to_string : path -> string
  [@@ocaml.deprecated "Use P.pp instead."]
  (** @deprecated use {!pp} instead. *)
end

(** Images.

    Consult their {{!page-semantics.semimages}semantics}.

    The [|>] operator is used to compose images. For this reason
    image combinators always take the image to use as the last
    argument. *)
module I : sig

  (** {1:images Images} *)

  type t = image
  (** The type for images. *)

  val void : image
  (** [void] is [const ]{!Gg.Color.void}, an invisible black image.
      [void] is an identity element for {!val-blend}. *)

  (** {1:prims Primitive images} *)

  val const : color -> image
  (** [const c] is an image of color [c].
      {ul {- \[[const c]\]{_[pt]} [= c] for any [pt].}} *)

  val axial : Color.stops -> p2 -> p2 -> image
  (** [axial stops pt pt'] is an image with an axial color gradient
      varying between [pt] and [pt'] according to {{!page-semantics.semstops}
      color stops} [stops].

      {ul {- \[[axial stops pt pt']\]{_[q]} [=] \[[stops]\]{_[t]} if [q] is
      on the line perpendicular to the line [pt] and [pt'] at
      the point [pt + t * (pt' - pt)].}} *)

  val radial : Color.stops -> ?f:p2 -> p2 -> float -> image
  (** [radial stops ~f c r] is an image with a color gradient varying
      according to {{!page-semantics.semstops} color stops} [stops] on
      circles whose center are on the segment from [f] to [c] and radius vary,
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
  (** [cut area p i] is [i] with the {{!page-semantics.sempaths}area} outside
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
      {!val-cut} except the path cut is the union of all the paths of the
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
  (** [compare i i'] is [Stdlib.compare i i']. *)

  val compare_f : (float -> float -> int) -> image -> image -> int
  (** [compare_f cmp i i'] is like {!compare} but uses [cmp] to
      compare floating point values.

      {b Note.} Raster images are tested with {!Gg.Raster.compare}. *)

  (** {1:fmt Formatters} *)

  val pp : Format.formatter -> image -> unit
  (** [pp ppf i] prints a textual representation of [i] on [ppf]. *)

  val pp_f : (Format.formatter -> float -> unit) -> Format.formatter ->
    image -> unit
  (** [pp_f pp_float ppf i] prints [i] like {!pp} but uses [pp_float]
      to print floating point values. *)

  val to_string : image -> string
  [@@ocaml.deprecated "Use I.pp instead."]
  (** @deprecated use {!pp} instead. *)
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
       via {{:http://erratique.ch/software/brr}[brr]}.}
    {- {!Vgr_cairo}, renders images using the
       {{:http://cairographics.org/}Cairo} library.}} *)
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
    [ `Buffer of Buffer.t | `Channel of out_channel | `Manual ]
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

      [limit] limits the time spent in the {!val-render} function (defaults to
      [max_int], unlimited).  The cost model may change in a future
      version of the library. For now each image combinator costs one
      unit, when the limit is reached {!val-render} returns with
      [`Partial].  *)

  val render : renderer -> [< `Image of renderable | `Await | `End ] ->
    [ `Ok | `Partial ]
  (** [render r v] is:
      {ul
      {- [`Partial] iff [r] has a [`Manual] destination and needs more
         output storage or if [r] has a limit. In the first
         case the client must use {!Manual.dst} to provide
         a new buffer. In both cases the client should then
         call {!val-render} with [`Await] until
         [`Ok] is returned.}
      {- [`Ok] when the encoder is ready to encode a new [`Image] (if
         the renderer supports it) or [`End].}}

      For [`Manual] destinations, encoding [`End] always returns
      [`Partial] the client should as usual use {!Manual.dst} and
      continue with [`Await] until [`Ok] is returned at which point
      {!Manual.dst_rem}[ r] is guaranteed to be the size of the last
      provided buffer (i.e. nothing was written).

      {b Semantics of multiple images render.} The semantics
      of multiple image renders are left to the backend.

      Raises [Invalid_argument] if [`Image] or [`End] is encoded after
      a [`Partial] encode. Or if multiple [`Image]s are encoded in
      a renderer that doesn't support them. *)

  val renderer_dst : renderer -> dst
  (** [render_dst r] is [r]'s destination. *)

  val renderer_limit : renderer -> int
  (** [renderer_limit r] is [r]'s limit. *)

  (** {1:manual Manual render destinations} *)

  (** Manual render destinations.

      {b Warning.} Only use with renderers with [`Manual] destinations. *)
  module Manual : sig
    val dst : renderer -> bytes -> int -> int -> unit
    (** [dst r s j l] provides [r] with [l] bytes to write, starting
        at [j] in [s]. This byte range is written by calls to {!val-render}
        until [`Partial] is returned. Use {!dst_rem} to know the remaining
        number of non-written free bytes in [s]. *)

    val dst_rem : renderer -> int
    (** [dst_rem r] is the remaining number of non-written, free bytes
        in the last buffer provided with {!val-dst}. *)
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
      want to drop an email to the maintainer for help and discussion.
      {ul
      {- If you render to "Bla", define you renderer in a module
         called [Vgr_bla] (lowercase).}
      {- The renderer target creation function must be named
         [Vgr_bla.target].}
      {- Images must be rendered via the {!val-render} function. If you
         are writing a batch renderer provide support for each of the
         {!dst} types and especially the non-blocking interface.}
      {- Respect Vg's linear sRGB color model.}
      {- Whenever possible use an XMP metadata packet for metadata,
         see {!Vgr.xmp}.}
      {- The renderer should implement the rendering cost model,
         see the [limit] parameter of {!val-render}.}
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
        Rendering will continue with [k r], on {!val-render} [`Await]. *)

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
