# Vg image database

The Vg image database is a collection of test images. 

Most images should be rendered identically by all renderers. However
due to differences in renderer expressiveness and purposes this may
not be possible or desirable.

Well written and appealing BSD3 licensed contributions showing Vg's
expressiveness are gladly accepted. They will however have to suit
Vg's author taste... Follow the instructions below.

## Adding a new image

To add a new image either add it to an existing relevant file or
create a new file for it. In the latter case, include the new file in
[`db_contents.ml`](db_contents.ml).

An image is added by calling the function `Db.image` (see [`db.mli`](db.mli))
at the toplevel of the file:

```ocaml
Db.image "a-unique-string-id"                                
  ~author:("Author name", "uri to author")
  ~title:"Title for the image"
  ~tags:["a"; "list"; "of"; "relevant"; "tags";]
  ~note:"Note about the image and things to look for."
  ~size:(Size2.v 200 400)                          (* renderer surface size. *)
  ~view:(Box2.v P2.o (Size2.v 100 200))             (* image view rectangle. *)
  begin fun view ->                 (* view is the preceding view parameter. *)
   (* Define a value of type Vg.image here *)
    I.void
  end;
```

It's better if the thunked image value is self-contained, so that I
can be copy-pasted to play around.


