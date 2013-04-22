Vg's image database
===================

Vg's image database is a collection of test images. 

Images from `db_any.ml` are renderer independent and should be
rendered identically by all renderers. However due to differences in
renderer expressiveness and purposes this may not be possible or
desirable. `db_$RENDERER.ml` includes `db_any.ml` and images that are
specific to the `Vgr_$RENDERER` renderer.


## Adding an image

To add an image either add it to an existing file with a relevant name
or create a new file for it. 

In the latter case the new file must be `include`d in `db_any.ml` if
it is renderer independent or in `db_$RENDERER.ml` if it is specific 
to the `Vgr_$RENDERER` renderer. 

An image is added by calling the function `Db.image` (see `db.mli`)
at the toplevel of the file:

```ocaml
Db.image "a-unique-string-id"                                
 ~author: "Author <author@example.org>"
 ~title: "Optional title for the image"
 ~subject: "Optional subject for the image"
 ~note: "Optional note for particular things to look for in the result"
 ~tags: ["a"; "list"; "of"; "relevant"; "tags";]
 ~meta: Vgm.(add empty quality `Best)           (* optional render metadata. *)
 ~size: (Size2.v 200 400)                          (* renderer surface size. *)
 ~view: (Box2.v P2.o (Size2.v 100 200)              (* image view rectangle. *)
 begin fun () ->
   (* Define a value of type Vg.image here *)
   I.void
 end;
```

The thunked image value *should* be self-contained. While this may
imply code redundancy between similar test images, it turns them into
example code that be simply copy-pasted to play around.


