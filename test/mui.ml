(*---------------------------------------------------------------------------
   Copyright (c) 2013 The vg programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Brr
open Brr_canvas

(* XXX quickly ported to Brr this can likely be rewritten more nicely and
   don't worry if you see non-sense. *)

(* Logging functions. *)

module Log = struct
  let flush () = Jstr.v (Format.flush_str_formatter ())
  let msg_js a = Console.(log [a])
  let msg fmt =
    let flush _ = Console.(log [flush ()]) in
    Format.kfprintf flush Format.str_formatter fmt

  let err fmt =
    let flush _ = Console.(error [flush ()]) in
    Format.kfprintf flush Format.str_formatter fmt
end

(* Events *)

module Ui = struct

  (* Dom constructors. *)

  let txt s = El.txt (Jstr.v s)

  type 'a cons = ?d:El.document -> ?at:At.t list -> 'a -> El.t

  let a_title = Jstr.v "title"
  let el ?id ?title ?(at = []) (f : 'a cons) classes children =
    let at = List.rev_append (List.map At.class' classes) at in
    let at = match id with
    | None -> at | Some id -> At.id (Jstr.v id) :: at
    in
    let at = match title with
    | None -> at | Some t -> At.name (Jstr.v t) :: at
    in
    f ~d:G.document ~at children

  let rem_childs e = List.iter El.remove (El.children e)

  type 'a printer = Format.formatter -> 'a -> unit
  type 'a t =
    { n : El.t;
      mutable on_change : 'a -> unit }

  type ('a, 'b) conf = 'a t * ('b -> unit)

  let str pp v =
    Format.fprintf Format.str_formatter "%a" pp v;
    Format.flush_str_formatter ()

  let node ui = ui.n
  let on_change ui cb = ui.on_change <- cb
  let nop _ = ()

  let c_group = Jstr.v "mu-group"
  let group ?id () =
    let d = el ?id El.div [c_group] [] in
    { n = d; on_change = nop }

  let c_label_text = Jstr.v "mu-label-text"
  let c_label = Jstr.v "mu-label"
  let label_mut ?id ?title ?(ctrl = false) str =
    if ctrl then
      let s = el El.span [c_label_text] [txt str] in
      let l = el ?id ?title El.label [c_label] [s] in
      let ui = { n = l; on_change = nop } in
      let set _ = failwith "Unimplemented" in
      let cb _ = Log.msg "Unimplemented" in
      ignore (Ev.listen Ev.change cb (El.as_target s));
      ui, set
    else
      let l = el ?id El.h1 [c_label; c_label_text] [txt str] in
      let ui = { n = l; on_change = nop } in
      let set _ = failwith "Unimplemented" in
      let cb _ = Log.msg "Unimplemented" in
      ignore (Ev.listen Ev.change cb (El.as_target l));
      ui, set

  let label ?id ?title ?ctrl str = fst (label_mut ?id ?title ?ctrl str)

  let c_text = Jstr.v "mu-text"
  let text ?id str =
    let p = el ?id El.span [c_text] [txt str] in
    let ui = { n = p; on_change = nop } in
    let set str = El.set_children p [El.txt (Jstr.v str)] in
    let cb _ = Log.msg "Unimplemented" in
    ignore (Ev.listen Ev.change cb (El.as_target p));
    ui, set

  let c_bool = Jstr.v "mu-bool"
  let bool ?id v =
    let at = At.[type' (Jstr.v "checkbox")] in
    let c = el ?id ~at El.input [c_bool] () in
    let ui = { n = c; on_change = nop } in
    let set b =
      El.set_at At.Name.checked (Some (Jstr.v (Bool.to_string b))) c
    in
    let cb _  =
      let b = match El.at At.Name.checked c with
      | None -> false
      | Some b ->
          Option.value ~default:false (bool_of_string_opt (Jstr.to_string b))
      in
      ui.on_change b
    in
    set v; ignore (Ev.listen Ev.change cb (El.as_target c));
    ui, set

  let make_focusable e = El.set_at At.Name.tabindex (Some (Jstr.v "0")) e; e

  let c_link = Jstr.v "mu-link"
  let a_download = Jstr.v "download"
  type link_conf = [ `Text of string | `Href of string | `Download of string ]
  let link ?id ?title ~href text =
    let a = el ?id ?title El.a [c_link; c_text] [txt text] in
    let ui = { n = a; on_change = nop } in
    let conf = function
    | `Href h -> El.set_at At.Name.href (Some (Jstr.v h)) a
    | `Text text -> rem_childs a; El.set_children a [txt text]
    | `Download d ->
        if d = "" then El.set_at a_download None a else
        El.set_at a_download (Some (Jstr.v d)) a
    in
    let cb _  = ui.on_change () in
    El.set_at At.Name.href (Some (Jstr.v href)) a;
    ignore (Ev.listen Ev.click cb (El.as_target a));
    ui, conf

  type 'a select_conf = [ `Select of 'a option | `List of 'a list ]
  let c_select = Jstr.v "mu-select"
  let c_selected = Jstr.v "mu-selected"
  let select ?id ?title pp s l =
    let ul = make_focusable (el ?id ?title El.ul [c_select] []) in
    let ui = { n = ul; on_change = nop } in
    let selected = ref None in
    let els = ref [||] in
    let els_v = ref [||] in
    let deselect () = match !selected with
    | None -> ()
    | Some i -> El.set_class c_selected false !els.(i)
    in
    let select () = match !selected with
    | None -> ()
    | Some i ->
        let el = !els.(i) in
        El.set_class c_selected true el;
        (* Scroll if needed *)
        let ut = El.bound_y ul in
        let ub = El.bound_y ul +. El.bound_h ul in
        let et = El.bound_y el in
        if ut >= et then El.scroll_into_view ~align_v:`End el else
        if ub <= et then El.scroll_into_view ~align_v:`Start el else ()
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
      let span = el El.span [] [txt (str pp v)] in
      let li = el El.li [] [span] in
      let on_click _ = set_selected (Some i) in
      ignore (Ev.listen Ev.click on_click (El.as_target li));
      El.append_children ui.n [li];
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
    let on_keydown e =
      match Jstr.to_string (Ev.Keyboard.code (Ev.as_type e)) with
      | "ArrowUp" (* Up *) | "ArrowLeft" (* Left *) ->
          let () = Ev.stop_propagation e in
          let () = Ev.prevent_default e in
          begin match !selected with
          | None -> set_selected (Some 0)
          | Some i -> set_selected (Some (i - 1))
          end
      | "ArrowDown" (* Down *) | "ArrowRight" (* Right *) ->
          let () = Ev.stop_propagation e in
          let () = Ev.prevent_default e in
          begin match !selected with
          | None -> set_selected (Some 0)
          | Some i -> set_selected (Some (i + 1))
          end
      | _ -> ()
    in
    ignore (Ev.listen Ev.keydown on_keydown (El.as_target ul));
    conf (`List l); conf (`Select s);
    ui, conf

  let c_mselect = Jstr.v "mu-mselect"
  let mselect ?id ?title pp sels l =
    let ul = el ?id ?title El.ul [c_select; c_mselect] [] in
    let ui = { n = ul; on_change = nop } in
    let selected = ref [] in
    let li v =
      let sp = el El.span [] [txt (str pp v)] in
      let li = el El.li [] [sp] in
      let on_click e =
        let () = Ev.stop_propagation e in
        let add = not (El.class' c_selected li) in
        El.set_class c_selected add li;
        selected :=
          if add then v :: !selected else
          List.filter (fun v' -> v <> v') !selected;
        ui.on_change !selected
      in
      if List.mem v sels
      then (selected := v :: !selected; El.set_class c_selected true li);
      ignore (Ev.listen Ev.click on_click (El.as_target li));
      li
    in
    let set l = rem_childs ui.n; El.append_children ui.n (List.map li l) in
    set l;
    ui, set

  type 'a menu_conf = [ `Select of 'a | `List of 'a list ]
  let c_menu = Jstr.v "mu-menu"
  let selected_idx = El.Prop.int (Jstr.v "selectedIndex")
  let menu ?id pp v l =
    let m = el ?id El.select [c_menu] [] in
    let ui = { n = m; on_change = nop } in
    let els_v = ref [||] in
    let opt i ov =
      let o = el ?id El.option [] [txt (str pp ov)] in
      El.append_children ui.n [o];
      if v = ov then (El.set_prop selected_idx i m);
      o
    in
    let conf = function
    | `Select s -> ()
    | `List l ->
        rem_childs ui.n;
        els_v := Array.of_list l;
        ignore (List.mapi opt l)
    in
    let on_change e  =
      let () = Ev.stop_propagation e in
      ui.on_change !els_v.(El.prop selected_idx m)
    in
    ignore (Ev.listen Ev.change on_change (El.as_target m));
    conf (`List l);
    conf (`Select v);
    ui, conf

  let c_canvas = Jstr.v "mu-canvas"
  let canvas_data c =
    let c = Canvas.of_el c in
    Jstr.to_string
      (Canvas.to_data_url c |> Console.log_if_error ~use:Jstr.empty)

  let canvas ?id () =
    let err = "Sorry, the HTML canvas is unsupported by this browser." in
    let c = el ?id El.canvas [c_canvas] [El.txt (Jstr.v err)] in
    let ui = { n = c; on_change = nop } in
    ui, c

  type object_conf =
    [ `Data of string | `Size of float * float | `Name of string ]

  let c_object = Jstr.v "mu-object"
  let object_ ?id () =
    let o = el ?id El.object' [c_object] [] in
    let ui = { n = o; on_change = nop } in
    let conf = function
    | `Data d -> El.set_prop (El.Prop.jstr (Jstr.v "data")) (Jstr.v d) o
    | `Name n ->
        El.set_prop (El.Prop.jstr (Jstr.v "name")) (Jstr.v n) o;
        El.set_at a_download (Some (Jstr.v n)) o;
    | `Size (w, h) ->
        let of_mm mm =
          Jstr.v (Printf.sprintf "%d" (truncate (ceil (mm *. 3.78))))
        in
        El.set_prop (El.Prop.jstr (Jstr.v "width")) (of_mm w) o;
        El.set_prop (El.Prop.jstr (Jstr.v "height")) (of_mm h) o;
        El.set_inline_style El.Style.width (of_mm w) o;
        El.set_inline_style El.Style.height (of_mm h) o;
    in
    ui, conf

  let classify_js ui c is_c = El.set_class c is_c ui.n
  let classify ui c is_c = classify_js ui (Jstr.v c) is_c
  let c_invisible_relayout = Jstr.v "mu-invisible-relayout"
  let c_invisible = Jstr.v "mu-invisible"
  let visible ?(relayout = false) ui visible =
    if visible then begin
      classify_js ui c_invisible_relayout false;
      classify_js ui c_invisible false;
    end else begin
      if relayout then classify_js ui c_invisible_relayout true else
      classify_js ui c_invisible true
    end

  let set_inner_html e raw = Jv.set (El.to_jv e) "innerHTML" (Jv.of_string raw)
  let set_txt_child ui str =
    set_inner_html ui.n ""; El.set_children ui.n [txt str]

  let set_svg_child ui svg =
    let svg =
      let dom_parser = Jv.get Jv.global "DOMParser" in
      let dom_parser = Jv.new' dom_parser [||] in
      let d = Jv.call dom_parser "parseFromString"
          Jv.[|of_string svg; of_string "image/svg+xml"|]
      in
      El.of_jv (Jv.get d "documentElement")
    in
    set_inner_html ui.n ""; El.append_children ui.n [svg]

  let client_size ui = El.inner_w ui.n, El.inner_h ui.n
  let set_height ui h =
    El.set_inline_style El.Style.height (Jstr.v h) ui.n

  let set_width ui w =
    El.set_inline_style El.Style.width (Jstr.v w) ui.n

  let hash () = Jstr.to_string (Uri.fragment (Window.location G.window))

  let set_hash h =
    let u = Window.location G.window in
    let u =
      Uri.with_uri ~fragment:(Jstr.v h) u |> Console.log_if_error ~use:u
    in
    Window.set_location G.window u

  let on_hash_change cb =
    let on_change _ = cb (hash ()) in
    ignore (Ev.listen Ev.hashchange on_change (Window.as_target G.window))

  let escape_binary d =
    let data = Base64.data_of_binary_jstr (Jstr.binary_of_octets d) in
    Jstr.to_string (Base64.encode data |> Console.log_if_error ~use:Jstr.empty)

  let ( *> ) p c = El.append_children p.n [c.n]; p
  let show ui = El.set_children (Document.body G.document) [ui.n]
  let main m =
    let main _ = m () in
    ignore (Ev.listen Ev.load main (Window.as_target G.window))
end

let ( *> ) = Ui.( *> )

(* Persistent storage *)

module Store = struct
  type scope = [ `Session | `Persist ]

  let scope_store = function
  | `Session -> Brr_io.Storage.session G.window
  | `Persist -> Brr_io.Storage.local G.window

  type 'a key = Jstr.t

  let key =
    let id = ref 0 in
    fun () -> id := !id + 1; Jstr.v ("k" ^ (string_of_int !id))

  let version : string key = key ()

  let mem ?(scope = `Persist) k =
    Option.is_some (Brr_io.Storage.get_item (scope_store scope) k)

  let add ?(scope = `Persist) k v =
    let b = Jstr.binary_of_octets (Marshal.to_string v []) in
    Brr_io.Storage.set_item (scope_store scope) k b
    |> Console.log_if_error ~use:()

  let rem ?(scope = `Persist) k =
    Brr_io.Storage.remove_item (scope_store scope) k

  let find ?(scope = `Persist) k =
    match Brr_io.Storage.get_item (scope_store scope) k with
    | None -> None
    | Some s ->
        try Some (Marshal.from_string (Jstr.binary_to_octets s) 0) with
        | Failure e -> Console.(error [str e]); None

  let get ?scope ?absent k = match find ?scope k with
  | Some s -> s
  | None ->
      match absent with
      | None -> invalid_arg "key unbound"
      | Some v -> v

  let clear ?(scope = `Persist) () = Brr_io.Storage.clear (scope_store scope)

  let force_version ?scope v =
    match find ?scope version with
    | None -> add ?scope version v
    | Some sv -> if v <> sv then (clear ?scope (); add ?scope version v)
end

(* Timing functions *)

module Time = struct
  let now () =
    let d = Jv.new' (Jv.get Jv.global "Date") [||] in
    (Jv.to_float (Jv.call d "getTime" [||]) /. 1000.)

  let duration f v =
    let start = now () in
    let r = f v in
    now () -. start, r

  let delay s f = ignore (G.set_timeout ~ms:(truncate (s *. 1000.)) f)
end
