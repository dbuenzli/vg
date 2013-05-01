(*---------------------------------------------------------------------------
   Copyright 2013 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Vg HTML canvas renderer.

    Renders on an HTML canvas element via 
    the {{:http://www.w3.org/TR/2dcontext/}HTML canvas} element.
    
    {b Unsupported capabilities.} Outlines cuts with dashes are
    unsupported, they are rendered as if [None] was specified for
    dashes and the [`Dashes] warning is reported.  The [`Aeo] cut
    rule is unsupported, it falls back on the [`Anz] rules and the
    c[`Aeo] warning is reported.

    {b Bug reports.} TODO.

    {e Release %%VERSION%% - %%AUTHORS%% } *)

(** {1 Renderer} *)


val renderer : ?meta:Vg.meta -> Dom_html.canvasElement Js.t -> Vg.renderer
(** [renderer meta c] is an HTML canvas renderer rendering to [c]. *)

(** {1 Render metadata}  

    The following standard keys are supported:
    
    {ul 
    {- {!Vg.Vgm.resolution}, specifies the rendering resolution.
       If unspecified 11811 dpm (300 dpi) is used in both dimensions.}}
*)

type warning = [ `Unsupported_outline_cut | `Dashes | `Aeo ]
(** The type for rendering warnings. 
    {ul
    {- [`Unsupported_outline_cut], only the {!I.const}, {!I.axial} and
       {!I.radial} primitive images can be cut with an [`O] area.}
    {- [`Dashes], dashed outlines are being used (TODO this seems supported
       now).}
    {- [`Aeo], a path area is defined by the [`Aeo] rule. [`Anz] will
       be used instead.}} *)

val pp_warning : Format.formatter -> warning -> unit
(** [pp_warning ppf w] prints a textual representation of [w] on [ppf]. *)

val warn : (warning -> unit) Vg.key
(** [warn] is called when unsupported capababilites are encountered. *)

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
