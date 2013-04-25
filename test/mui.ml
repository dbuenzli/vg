(*---------------------------------------------------------------------------
   Copyright 2013 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(* Logging functions. *)

module Log = struct  
  let flush () = Js.string (Format.flush_str_formatter ()) 
  let msg_js a = Firebug.console ## log (a)
  let msg fmt = 
    let flush _ = Firebug.console ## log (flush ()) in
    Format.kfprintf flush Format.str_formatter fmt
    
  let err fmt = 
    let flush _ = Firebug.console ## error (flush ()) in
    Format.kfprintf flush Format.str_formatter fmt
end


module D = struct    
  
  (* Constants *)
  
  let window = Dom_html.window
  let document = Dom_html.document

  (* DOM nodes *)

  let el ?id n = 
    let e = document ## createElement (Js.string n) in 
    (match id with None -> () | Some id -> e ## id <- Js.string id); 
    e


  let txt s = document ## createTextNode (Js.string s) 
  let canvas ?id () = 
    let e : Dom_html.canvasElement Js.t = Js.Unsafe.coerce (el ?id "canvas") in 
    let err = "Sorry, the HTML canvas is unsupported by this browser." in
    Dom.appendChild e (txt err);
    e

end

(* Events *)      

module Ev = struct
  let make = Dom.Event.make 
  let load = make "load"
  include Dom_html.Event
      
  let cb n e f =
    let h = Dom.full_handler (fun n ev -> Js.bool (f n ev)) in
    ignore (Dom.addEventListener n e h (Js.bool false))   
end

module Ui = struct

  (* Dom constructors. *)

  let (<*>) p c = Dom.appendChild p c; p
  let txt s = D.document ## createTextNode (Js.string s) 
  let el ?id f cs = 
    let e = f D.document in
    List.iter (fun c -> e ## classList ## add (c)) cs; 
    match id with 
    | None -> e 
    | Some id -> e ## id <- Js.string id; e

  let rec rem_childs e = match Js.Opt.to_option (e ## firstChild) with
  | None -> () | Some c -> e ## removeChild (c); rem_childs e

  type 'a printer = Format.formatter -> 'a -> unit
  type 'a t = 
    { n : Dom.node Js.t; 
      mutable on_change : 'a -> unit }

  type ('a, 'b) conf = 'a t * ('b -> unit)

  let str pp v =
    Format.fprintf Format.str_formatter "%a" pp v; 
    Format.flush_str_formatter ()

  let node ui = ui.n 
  let on_change ui cb = ui.on_change <- cb
  let nop _ = ()

  let c_group = Js.string "mu-group"
  let group ?id () = 
    let d = el ?id Dom_html.createDiv [c_group] in
    { n = (d :> Dom.node Js.t); on_change = nop }
    
  let c_label_text = Js.string "mu-label-text"
  let c_label = Js.string "mu-label" 
  let label_mut ?id ?(ctrl = false) str =
    if ctrl then 
      let s = el Dom_html.createSpan [c_label_text] <*> txt str in
      let l = el ?id Dom_html.createLabel [c_label] <*> s in
      let ui = { n = (l :> Dom.node Js.t); on_change = nop } in
      let set _ = failwith "TODO" in 
      let cb _ _ = Log.msg "TODO"; false in 
      Ev.cb s Ev.change cb;
      ui, set
    else
      let l = el ?id Dom_html.createH1 [c_label; c_label_text] <*> txt str in
      let ui = { n = (l :> Dom.node Js.t); on_change = nop } in 
      let set _ = failwith "TODO" in 
      let cb _ _ = Log.msg "TODO"; false in 
      Ev.cb l Ev.change cb;
      ui, set

  let label ?id ?ctrl str = fst (label_mut ?id ?ctrl str)

  let c_text = Js.string "mu-text"
  let text ?id str = 
    let p = el ?id Dom_html.createP [c_text] <*> txt str in
    let ui = { n = (p :> Dom.node Js.t); on_change = nop } in
    let set str = match Js.Opt.to_option (p ## firstChild) with 
    | Some t -> ignore (p ## replaceChild (((txt str) :> Dom.node Js.t), t))
    | None -> assert false
    in
    let cb _ _ = Log.msg "TODO"; false in
    Ev.cb p Ev.change cb; 
    ui, set
      
  let c_bool = Js.string "mu-bool" 
  let bool ?id v = 
    let cbox d = Dom_html.createInput ~_type:(Js.string "checkbox") d in
    let c = el ?id cbox [c_bool] in
    let ui = { n = (c :> Dom.node Js.t); on_change = nop } in 
    let set b = c ## checked <- Js.bool b in 
    let cb _ _ = ui.on_change (Js.to_bool (c ## checked)); false in
    set v; Ev.cb c Ev.change cb;
    ui, set

  let c_select = Js.string "mu-select"
  let c_selected = Js.string "mu-selected"
  let select ?id pp sel l = 
    let ul = el ?id Dom_html.createUl [c_select] in 
    let ui = { n = (ul :> Dom.node Js.t); on_change = nop } in
    let selected = ref (el Dom_html.createLi []) (* dumb *) in
    let li v = 
      let sp = el Dom_html.createSpan [] <*> txt (str pp v) in
      let li = el Dom_html.createLi [] <*> sp in 
      let li_cb _ _ = 
        !selected ## classList ## remove (c_selected); 
        selected := li;
        !selected ## classList ## add (c_selected);
        ui.on_change v; false 
      in
      if v = sel then (selected := li; li ## classList ## add (c_selected));
      Ev.cb li Ev.click li_cb;
      li
    in 
    let set l = 
      rem_childs ui.n; List.iter (fun v -> Dom.appendChild ui.n (li v)) l
    in
    set l; 
    ui, set

  let c_mselect = Js.string "mu-mselect" 
  let mselect ?id pp sels l = 
    let ul = el ?id Dom_html.createUl [c_select] in 
    let ui = { n = (ul :> Dom.node Js.t); on_change = nop } in
    let selected = ref [] in
    let li v = 
      let sp = el Dom_html.createSpan [] <*> txt (str pp v) in
      let li = el Dom_html.createLi [] <*> sp in 
      let li_cb _ _ = 
        let _ = li ## classList ## toggle (c_selected) in
        ui.on_change !selected; false
      in
      if List.mem v sels then 
        (selected := v :: !selected; li ## classList ## add (c_selected));
      Ev.cb li Ev.click li_cb;
      li
    in 
    let set l = 
      rem_childs ui.n; List.iter (fun v -> Dom.appendChild ui.n (li v)) l
    in
    set l; 
    ui, set
      
  let ( *> ) p c = Dom.appendChild p.n c.n; p
  let show ui = ignore (D.document ## body <*> ui.n)
  let main m = 
    let main _ _ = m (); false in
    Ev.cb D.window Ev.load main
end

let ( *> ) = Ui.( *> )

(* Persistent storage *)

module Store = struct
  type scope = [ `Session | `Persist ]
               
  let scope_store = function
  | `Session -> Js.Optdef.to_option (D.window ## sessionStorage)
  | `Persist -> Js.Optdef.to_option (D.window ## localStorage) 

  let support scope = scope_store scope <> None

  type 'a key = Js.js_string Js.t

  let key = 
    let id = ref 0 in 
    fun () -> id := !id + 1; Js.string ("k" ^ (string_of_int !id))
        
  let mem ?(scope = `Persist) k = match scope_store scope with 
  | Some s -> Js.Opt.test (s ## getItem (k))
  | None -> false 

  let add ?(scope = `Persist) k v = match scope_store scope with
  | Some s -> s ## setItem (k, Json.output v) | None -> () 
    
  let rem ?(scope = `Persist) k = match scope_store scope with 
  | Some s -> s ## removeItem (k) | None -> () 
    
  let find ?(scope = `Persist) k = match scope_store scope with
  | Some s -> 
      begin match Js.Opt.to_option (s ## getItem (k)) with
      | None -> None 
      | Some vs -> Some (Json.unsafe_input vs)
      end
  | None -> None
    
  let get ?(scope = `Persist) k = match scope_store scope with 
  | Some s -> 
      begin match Js.Opt.to_option (s ## getItem (k)) with
      | None -> invalid_arg "key unbound"
      | Some vs -> Json.unsafe_input vs
      end
  | None -> invalid_arg "store unsupported"
                    
  let clear ?(scope = `Persist) () = match scope_store scope with
  | Some s -> s ## clear () | None -> ()
end

(* Timing functions *)

module Time = struct
  let now () = Js.to_float (jsnew Js.date_now () ## getTime ()) /. 1000.
  let now_date () = 
    let d = jsnew Js.date_now () in 
    (d ## getUTCFullYear (), d ## getUTCMonth (), d ## getUTCDate ()), 
    (d ## getUTCHours (), d ## getUTCMinutes (), d ## getUTCSeconds ())
    
  let duration f v = 
    let start = now () in
    let r = f v in 
    now () -. start, r

end

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
