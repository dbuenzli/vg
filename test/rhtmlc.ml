(*---------------------------------------------------------------------------
   Copyright 2013 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(* Renders the Vg image database to the HTML canvas element *)

open Gg
open Vg
open Mui

include Db_htmlc

let pp = Format.fprintf 
let pp_dur ppf t = pp ppf "%.4fs" t
let pp_str = Format.pp_print_string 

type t = { sf : string; bf : bool; slf : string list;  prefix : string }
let default = { sf = ""; bf = true; slf = []; prefix = ""} 
            
(* Persistent ui state. *)

module S = struct
  type t = 
    { id : string;                  (* selected image id *)
      white_bg : bool;              (* white background *)
      budget : bool;
      tags : string list;           (* selected tags *)
      prefix : string; }            (* filter prefix *)
  
  let state : t Store.key = Store.key () 
  
  let default = 
    { id = "default";
      white_bg = true; 
      budget = true;
      tags = [];
      prefix = "default" }      

  let set s = Store.add state s
  let get () = match Store.find state with 
  | None -> set default; default
  | Some s -> s

  let ids st = 
    let id i = i.Db.id in
    List.map id (Db.find ~prefixes:[st.prefix] ~tags:st.tags ())
end

let ui () = 
  let db_ids, db_tags = Db.indexes () in
  let s = S.get () in
  let budget, set_budget = Ui.bool s.S.budget in 
  let white, set_white = Ui.bool s.S.white_bg in
  let ids, set_ids = Ui.select pp_str s.S.id db_ids in
  let tags, set_tags = Ui.mselect pp_str s.S.tags db_tags in 
  let title, set_title = Ui.text ~id:"i-title" "" in
  let author, set_author = Ui.text ~id:"i-author" "" in
  let cmd = function
  | `Use_budget b -> 
      Log.msg "Use budget";
      S.set { (S.get ()) with S.budget = b }
  | `Use_white_bg b -> 
      Log.msg "W:%b" b;
      S.set { (S.get ()) with S.white_bg = b }
  | `Select_id id -> 
      Log.msg "Id: %s" id; 
      let s = S.set { (S.get ()) with S.id = id } in
      let i = match Db.find ~ids:[id] () with
      | [id] -> id | l -> 
          List.map (fun i -> Log.msg "%s" i.Db.id) l;
          assert false 
      in
      set_title i.Db.title; 
      set_author i.Db.author;
  | `Use_tags ts ->
      Log.msg "Tags";
      S.set { (S.get ()) with S.tags = ts }
  in
  (* Untying the recursive knot... *)
  Ui.on_change budget (fun b -> cmd (`Use_budget b)); 
  Ui.on_change white (fun b -> cmd (`Use_white_bg b));
  Ui.on_change ids (fun id -> cmd (`Select_id id)); 
  Ui.on_change tags (fun ts -> cmd (`Use_tags ts)); 
  (* Init UI *)
  ignore (cmd (`Select_id s.S.id));
  (* Layout *)
  Ui.group () *> 
    Ui.label "Vg Image database" *>
    (Ui.group ~id:"ui" () *> 
       (Ui.group ~id:"ids" () *> Ui.label "Images" *> ids) *>
       (Ui.group ~id:"tags" () *> Ui.label "Tags" *> tags) *>
       (Ui.group ~id:"rset" () *> 
          Ui.label "Render settings" *> 
          (Ui.label ~ctrl:true "Budget" *> budget) *>
          (Ui.label ~ctrl:true "White background" *> white))) *>
    (Ui.group ~id:"image" () *>
       (Ui.group ~id:"info" () *> title *> author))

let main () = Ui.show (ui ())
  
let () = Ui.main main 

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
