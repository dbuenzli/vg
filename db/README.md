The Vg image database
=====================

The Vg image database is a collection of test images. The database
contains:

* Renderer independent images in [`db_any.ml`](db_any.ml)
* Renderer dependent image in `db_$RENDERER.ml` files. 

Renderer independent images should be rendered identically by all
renderers but due to differences in renderer expressiveness and
purposes this may not be possible or desirable.

Well written and appealing BSD3 licensed contributions showing Vg's
expressiveness are gladly accepted. They will however have to suit
Vg's author taste... Follow the instructions below.

## Adding a new image

To add a new image either add it to an existing relevant file or
create a new file for it. In the latter case, if the image is renderer
independent include the new file in [`db_any.ml`](db_any.ml), if it is
not include the file in `db_$RENDERER.ml`.

An image is added by calling the function `Db.image` (see [`db.mli`](db.mli))
at the toplevel of the file:

```ocaml
Db.image "a-unique-string-id"                                
  ~author:"Author <author@example.org>"
  ~title:"Title for the image"
  ~tags:["a"; "list"; "of"; "relevant"; "tags";]
  ~subject:"Optional subject for the image"
  ~note:"Optional note for particular things to look for in the result"
  ~meta:Vgm.(add empty quality `Best)           (* optional render metadata. *)
  ~size:(Size2.v 200 400)                          (* renderer surface size. *)
  ~view:(Box2.v P2.o (Size2.v 100 200)              (* image view rectangle. *)
  begin fun () ->
   (* Define a value of type Vg.image here *)
    I.void
  end;
```

The thunked image value *should* be self-contained. While this may
imply code redundancy between similar test images, it turns them into
example code that be simply copy-pasted to play around.


