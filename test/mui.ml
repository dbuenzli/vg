(*---------------------------------------------------------------------------
   Copyright (c) 2013 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(* Logging functions. *)

module Log = struct
  let flush () = Js.string (Format.flush_str_formatter ())
  let msg_js a = Firebug.console ## (log a)
  let msg fmt =
    let flush _ = Firebug.console ## (log (flush ())) in
    Format.kfprintf flush Format.str_formatter fmt

  let err fmt =
    let flush _ = Firebug.console ## (error (flush ())) in
    Format.kfprintf flush Format.str_formatter fmt
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

  let window = Dom_html.window
  let document = Dom_html.document

  let (<*>) p c = Dom.appendChild p c; p
  let txt s = document ## (createTextNode (Js.string s))

  let a_title = Js.string "title"
  let el ?id ?title f cs =
    let e = f document in
    List.iter (fun c -> e ##. classList ## (add c)) cs;
    begin match id with
    | None -> () | Some id -> e ##. id := Js.string id
    end;
    begin match title with
    | None -> () | Some t -> e ## (setAttribute a_title (Js.string t))
    end;
    e

  let canvas ?id _ =
    let canvas d = d ## (createElement (Js.string "canvas")) in
    let e : Dom_html.canvasElement Js.t = Js.Unsafe.coerce (el ?id canvas [])in
    let err = "Sorry, the HTML canvas is unsupported by this browser." in
    Dom.appendChild e (document ## (createTextNode (Js.string err)));
    e

  let rec rem_childs e = match Js.Opt.to_option (e ##. firstChild) with
  | None -> () | Some c -> e ## (removeChild c); rem_childs e

  type 'a printer = Format.formatter -> 'a -> unit
  type 'a t =
    { n : Dom_html.element Js.t;
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
    { n = d; on_change = nop }

  let c_label_text = Js.string "mu-label-text"
  let c_label = Js.string "mu-label"
  let label_mut ?id ?title ?(ctrl = false) str =
    if ctrl then
      let s = el Dom_html.createSpan [c_label_text] <*> txt str in
      let l = el ?id ?title Dom_html.createLabel [c_label] <*> s in
      let ui = { n = (l :> Dom_html.element Js.t); on_change = nop } in
      let set _ = failwith "Unimplemented" in
      let cb _ _ = Log.msg "Unimplemented"; false in
      Ev.cb s Ev.change cb;
      ui, set
    else
      let l = el ?id Dom_html.createH1 [c_label; c_label_text] <*> txt str in
      let ui = { n = l; on_change = nop } in
      let set _ = failwith "Unimplemented" in
      let cb _ _ = Log.msg "Unimplemented"; false in
      Ev.cb l Ev.change cb;
      ui, set

  let label ?id ?title ?ctrl str = fst (label_mut ?id ?title ?ctrl str)

  let c_text = Js.string "mu-text"
  let text ?id str =
    let p = el ?id Dom_html.createSpan [c_text] <*> txt str in
    let ui = { n = p; on_change = nop } in
    let set str = match Js.Opt.to_option (p ##. firstChild) with
    | Some t -> ignore (p ## (replaceChild (((txt str) :> Dom.node Js.t)) t))
    | None -> assert false
    in
    let cb _ _ = Log.msg "Unimplemented"; false in
    Ev.cb p Ev.change cb;
    ui, set

  let c_bool = Js.string "mu-bool"
  let bool ?id v =
    let cbox d = Dom_html.createInput ~_type:(Js.string "checkbox") d in
    let c = el ?id cbox [c_bool] in
    let ui = { n = (c :> Dom_html.element Js.t); on_change = nop } in
    let set b = c ##. checked := Js.bool b in
    let cb _ _ = ui.on_change (Js.to_bool (c ##. checked)); false in
    set v; Ev.cb c Ev.change cb;
    ui, set

  let make_focusable e =
    (e ## (setAttribute (Js.string "tabindex") (Js.string "0"))); e

  let c_link = Js.string "mu-link"
  let a_download = Js.string "download"
  type link_conf = [ `Text of string | `Href of string | `Download of string ]
  let link ?id ?title ~href text =
    let a = el ?id ?title Dom_html.createA [c_link; c_text] <*> txt text in
    let ui = { n = (a :> Dom_html.element Js.t); on_change = nop } in
    let conf = function
    | `Href h -> a ##. href := Js.string h
    | `Text text -> rem_childs a; ignore (a <*> txt text)
    | `Download d ->
        if d = "" then a ## (removeAttribute a_download) else
        a ## (setAttribute a_download (Js.string d))
    in
    let cb _ _ = ui.on_change (); true in
    a ##. href := Js.string href;
    Ev.cb a Ev.click cb;
    ui, conf

  type 'a select_conf = [ `Select of 'a option | `List of 'a list ]
  let c_select = Js.string "mu-select"
  let c_selected = Js.string "mu-selected"
  let select ?id ?title pp s l =
    let ul = make_focusable (el ?id ?title Dom_html.createUl [c_select]) in
    let ui = { n = ul; on_change = nop } in
    let selected = ref None in
    let els = ref [||] in
    let els_v = ref [||] in
    let deselect () = match !selected with
    | None -> () | Some i -> !els.(i) ##. classList ## (remove c_selected)
    in
    let select () = match !selected with
    | None -> ()
    | Some i ->
        let el = !els.(i) in
        el ##. classList ## (add c_selected);
        (* Scroll if needed *)
        let ut = truncate (Js.to_float (ul ##getBoundingClientRect##. top)) in
        let et = truncate (Js.to_float (el ##getBoundingClientRect##. top)) in
        if (et < ut) then
          ul ##. scrollTop := ul ##. scrollTop - (ut - et)
        else if (et >= ut + ul ##. clientHeight) then
          ul ##. scrollTop := ul ##. scrollTop +
              (et + el ##. clientHeight - ut - ul ##. clientHeight)
    in
    let set_selected = function
    | None ->
        begin match !selected with
        | None -> ()
        | Some _ ->
            deselect (); ui.on_change None
        end
    | Some i ->
        let max = Array.length !els - 1 in
        let i = if i < 0 then 0 else (if i > max then max else i) in
        begin match !selected with
        | Some j when i = j -> ()
        | _ ->
            deselect (); selected := Some i; select ();
            ui.on_change (Some !els_v.(i))
        end
    in
    let li i v =
      let span = el Dom_html.createSpan [] <*> txt (str pp v) in
      let li = el Dom_html.createLi [] <*> span in
      let on_click _ _ = set_selected (Some i); false in
      Ev.cb li Ev.click on_click;
      Dom.appendChild ui.n li;
      li
    in
    let conf = function
    | `Select s -> begin match s with
      | None -> set_selected None
      | Some s ->
          Array.iteri (fun i v -> if v = s then set_selected (Some i)) !els_v
      end
    | `List l ->
        rem_childs ui.n;
        selected := None;
        els_v := Array.of_list l;
        els := Array.mapi li !els_v
    in
    let on_keydown _ e = match (e ##. keyCode) with
    | 38 (* Up *) | 37 (* Left *) -> begin match !selected with
      | None -> set_selected (Some 0); false
      | Some i -> set_selected (Some (i - 1)); false
      end
    | 40 (* Down *) | 39 (* Right *) -> begin match !selected with
      | None -> set_selected (Some 0); false
      | Some i -> set_selected (Some (i + 1)); false
      end
    | _ -> (); true
    in
    Ev.cb ul Ev.keydown on_keydown;
    conf (`List l); conf (`Select s);
    ui, conf


  let c_mselect = Js.string "mu-mselect"
  let mselect ?id ?title pp sels l =
    let ul = el ?id ?title Dom_html.createUl [c_select; c_mselect] in
    let ui = { n = ul; on_change = nop } in
    let selected = ref [] in
    let li v =
      let sp = el Dom_html.createSpan [] <*> txt (str pp v) in
      let li = el Dom_html.createLi [] <*> sp in
      let on_click _ _ =
        let add = Js.to_bool (li ##. classList ## (toggle c_selected)) in
        selected :=
          if add then v :: !selected else
          List.filter (fun v' -> v <> v') !selected;
        ui.on_change !selected; false
      in
      if List.mem v sels then
        (selected := v :: !selected; li ##. classList ## (add c_selected));
      Ev.cb li Ev.click on_click;
      li
    in
    let set l =
      rem_childs ui.n; List.iter (fun v -> Dom.appendChild ui.n (li v)) l
    in
    set l;
    ui, set

  type 'a menu_conf = [ `Select of 'a | `List of 'a list ]
  let c_menu = Js.string "mu-menu"
  let menu ?id pp v l =
    let m = el ?id Dom_html.createSelect [c_menu ] in
    let ui = { n = (m :> Dom_html.element Js.t); on_change = nop } in
    let els_v = ref [||] in
    let opt i ov =
      let o = el ?id Dom_html.createOption [] <*> txt (str pp ov) in
      Dom.appendChild ui.n o;
      if v = ov then (m ##. selectedIndex := i);
      o
    in
    let conf = function
    | `Select s -> ()
    | `List l ->
        rem_childs ui.n;
        els_v := Array.of_list l;
        ignore (List.mapi opt l)
    in
    let on_change _ _ = ui.on_change !els_v.((m ##. selectedIndex)); false in
    Ev.cb m Ev.change on_change;
    conf (`List l);
    conf (`Select v);
    ui, conf

  let c_canvas = Js.string "mu-canvas"
  let canvas_data c = Js.to_string (c ## toDataURL)
  let canvas ?id () =
    let c = el ?id canvas [c_canvas] in
    let ui = { n = (c :> Dom_html.element Js.t); on_change = nop } in
    ui, c

  type object_conf =
    [ `Data of string | `Size of float * float | `Name of string ]

  let c_object = Js.string "mu-object"
  let object_ ?id () =
    let o = el ?id Dom_html.createObject [ c_object ] in
    let ui = { n = (o :> Dom_html.element Js.t); on_change = nop } in
    let conf = function
    | `Data d -> o ##. data := Js.string d
    | `Name n ->
        o ##. name := Js.string n;
        o ## (setAttribute a_download (Js.string n));
    | `Size (w, h) ->
        let of_mm mm =
          Printf.sprintf "%d" (truncate (ceil (mm *. 3.78)))
        in
        o ##. width := Js.string (of_mm w);
        o ##. height := Js.string (of_mm h);
        o ##. style ##. width := Js.string (of_mm w);
        o ##. style ##. height := Js.string (of_mm h);
    in
    ui, conf

  let classify_js ui c is_c =
    if is_c then ui.n ##. classList ## (add c) else
    ui.n ##. classList ## (remove c)

  let classify ui c is_c = classify_js ui (Js.string c) is_c
  let c_invisible_relayout = Js.string "mu-invisible-relayout"
  let c_invisible = Js.string "mu-invisible"
  let visible ?(relayout = false) ui visible =
    if visible then begin
      classify_js ui c_invisible_relayout false;
      classify_js ui c_invisible false;
    end else begin
      if relayout then classify_js ui c_invisible_relayout true else
      classify_js ui c_invisible true
    end

  let set_raw_child ui raw = ui.n ##. innerHTML := Js.string raw
  let set_txt_child ui str =
    ui.n ##. innerHTML := Js.string "";
    ignore (ui.n <*> (txt str))

  let set_svg_child ui svg =
    let p = Js.Unsafe.variable "DOMParser" in
    let p = new%js p in
    let svg =
      p ## (parseFromString (Js.string svg) (Js.string "image/svg+xml"))
    in
    ui.n ##. innerHTML := Js.string "";
    ignore (ui.n ## (appendChild svg ##. documentElement))

  let client_size ui = ui.n ##. clientWidth, ui.n ##. clientHeight
  let set_height ui h = ui.n ##. style ##. height := Js.string h
  let set_width ui w = ui.n ##. style ##. width := Js.string w
  let hash () =
    let h = Js.to_string (window ##. location ##. hash) in
    let len = String.length h in
    if len > 0 && h.[0] = '#' then String.sub h 1 (len - 1) else
    h

  let set_hash h =
    let h = if h <> "" then "#" ^ h else h in
    window ##. location ##. hash := Js.string h

  let on_hash_change cb =
    let on_change _ _ = cb (hash ()); false in
    Ev.cb window Ev.hashchange on_change

  let escape_binary d = Js.to_string (Js.escape (Js.bytestring d))

  let ( *> ) p c = Dom.appendChild p.n c.n; p
  let show ui = ignore (document ##. body <*> ui.n)
  let main m =
    let main _ _ = m (); false in
    Ev.cb window Ev.load main
end

let ( *> ) = Ui.( *> )

(* Persistent storage *)

module Store = struct
  type scope = [ `Session | `Persist ]

  let scope_store = function
  | `Session -> Js.Optdef.to_option (Dom_html.window ##. sessionStorage)
  | `Persist -> Js.Optdef.to_option (Dom_html.window ##. localStorage)

  let support scope = scope_store scope <> None

  type 'a key = Js.js_string Js.t

  let key =
    let id = ref 0 in
    fun () -> id := !id + 1; Js.string ("k" ^ (string_of_int !id))

  let version = key ()

  let mem ?(scope = `Persist) k = match scope_store scope with
  | Some s -> Js.Opt.test (s ## (getItem k))
  | None -> false

  let add ?(scope = `Persist) k v = match scope_store scope with
  | Some s -> s ## (setItem k (Json.output v)) | None -> ()

  let rem ?(scope = `Persist) k = match scope_store scope with
  | Some s -> s ## (removeItem k) | None -> ()

  let find ?(scope = `Persist) k = match scope_store scope with
  | None -> None
  | Some s ->
      begin match Js.Opt.to_option (s ## (getItem k)) with
      | None -> None
      | Some vs -> Some (Json.unsafe_input vs)
      end

  let get ?(scope = `Persist) ?absent k = match scope_store scope with
  | None -> invalid_arg "store unsupported"
  | Some s ->
      begin match Js.Opt.to_option (s ## (getItem k)) with
      | None ->
          begin match absent with
          | None -> invalid_arg "key unbound"
          | Some v -> v
          end
      | Some vs -> Json.unsafe_input vs
      end

  let force_version ?(scope = `Persist) v = match scope_store scope with
  | None -> ()
  | Some s ->
      match find ~scope version with
      | None -> add ~scope version v
      | Some sv -> if v <> sv then (s ## clear; add ~scope version v)

  let clear ?(scope = `Persist) () = match scope_store scope with
  | Some s -> s ## clear | None -> ()
end

(* Timing functions *)

module Time = struct
  let now () = Js.to_float ((new%js Js.date_now) ## getTime) /. 1000.
  let now_date () =
    let d = new%js Js.date_now in
    (d ## getUTCFullYear, d ## getUTCMonth, d ## getUTCDate),
    (d ## getUTCHours, d ## getUTCMinutes, d ## getUTCSeconds)

  let duration f v =
    let start = now () in
    let r = f v in
    now () -. start, r

  let delay s f =
    let ms = s *. 1000. in
    ignore (Dom_html.window ## (setTimeout (Js.wrap_callback f) ms))
end

(*---------------------------------------------------------------------------
   Copyright (c) 2013 Daniel C. Bünzli

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
