(*----------------------------------------------------------------------------
   Copyright (c) %%COPYRIGHTYEAR%%, Daniel C. Bünzli. All rights reserved.     
   Distributed under a BSD license, see ../LICENSE.                            
  ----------------------------------------------------------------------------*)
open Gg2;;


module P = Backend.P
module I = Backend.I

let ( >> ) = Backend.( >> ) 
let ( & ) = Backend.( & ) 
let ( ++ ) = Backend.( ++ )

module Vgo = struct
  exception Unsupported = Backend.Unsupported

  type page = size * rect * I.t
  type output = Backend.output

  let output_of_channel = Backend.output_of_channel
  let output_of_buffer = Backend.output_of_buffer      
  let output_of_fun = Backend.output_of_fun

  let output_pdf = Pdf.output      
  let output_svg = Svg.output


  module Backend = Backend
end

