(*---------------------------------------------------------------------------
   Copyright 2013 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

open Gg
open Vg

(** Test images for uncut image primitives. *)

let emerald = Color.v 0.314 0.784 0.471 1.
;;

Db.image "uncut-const" ~author:Db.dbuenzli
  ~title:"Uncut constant emerald image"
  ~tags:["uncut"]
  ~note:"Constant emerald color over the rectangle."
  ~size:(Size2.v 120. 60.)
  ~view:(Box2.v P2.o (P2.v 2. 1.))
  begin fun _ -> 
    I.const emerald 
  end;

Db.image "uncut-const-tr" ~author:Db.dbuenzli
  ~title:"Uncut transformed constant image"
  ~tags:["uncut"]
  ~note:"Constant emerald color over the rectangle."
  ~size:(Size2.v 120. 60.)
  ~view:(Box2.v P2.o (P2.v 2. 1.))
  begin fun _ ->
    I.const emerald >> 
    I.move (V2.v 1. 1.) >> I.scale (V2.v 3. 4.) >> I.blend I.void >>
    I.move (V2.v 0.25 0.0)
  end;

Db.image "uncut-axial" ~author:Db.dbuenzli
  ~title:"Uncut axial gradient image"
  ~tags:["uncut"; "gradient"]
  ~note:"From left to right: black to emerald axial gradient."
  ~size:(Size2.v 120. 60.)
  ~view:(Box2.v P2.o (Size2.v 2. 1.))
  begin fun _ -> 
    let stops = [0.0, Color.black; 1.0, emerald] in 
    I.axial stops P2.o (P2.v 2. 0.)
  end;

Db.image "uncut-axial-tr" ~author:Db.dbuenzli
  ~title:"Uncut transformed axial gradient image"
  ~tags:["uncut"; "gradient"]
  ~note:"From left to right: black to emerald axial gradient."
  ~size:(Size2.v 120. 60.)
  ~view:(Box2.v P2.o (Size2.v 2. 1.))
  begin fun _ -> 
    let stops = [0.0, Color.black; 1.0, emerald] in 
    I.axial stops (P2.v (-1.) (0.)) (P2.v 3. 0.) >> 
    I.move (V2.v 0.5 0.0) >> I.scale (V2.v 0.5 1.0) >> I.blend I.void >>
    I.move (V2.v 0.25 0.0) 
  end;

Db.image "uncut-radial" ~author:Db.dbuenzli
  ~title:"Uncut radial gradient image"
  ~tags:["uncut"; "gradient"]
  ~note:"Centered, from inwards to outwards: black to emerald radial gradient."
  ~size:(Size2.v 120. 60.)
  ~view:(Box2.v P2.o (Size2.v 2. 1.))
  begin fun _ -> 
    let stops = [0.0, Color.black; 1.0, emerald] in 
    I.radial stops (P2.v 1.0 0.5) 1.0
  end;

Db.image "uncut-radial-tr" ~author:Db.dbuenzli
  ~title:"Uncut transformed radial gradient image"
  ~tags:["uncut"; "gradient"]
  ~note:"Centered, from inwards to outwards: black to emerald radial gradient."
  ~size:(Size2.v 120. 60.)
  ~view:(Box2.v P2.o (Size2.v 2. 1.))
  begin fun _ -> 
    let stops = [0.0, Color.black; 1.0, emerald] in 
    I.radial stops P2.o 2.0 >>
    I.move (V2.v 0.5 0.5) >> I.scale (V2.v 0.5 0.5) >> I.blend I.void >>
    I.move (V2.v 0.75 0.25) 
  end;

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
