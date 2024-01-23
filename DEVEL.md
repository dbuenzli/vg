A few development tips.

# Test image database 

The test image database in [`test/db`][test/db] is a collection of
test images.

Most images should render identically by with all renderers. However
due to differences in renderer expressiveness and purposes this may
not be possible or desirable.

Each renderer has its own test executable, see `b0 list`. For example
for `Vgr_pdf` you can issue:

    b0 -- test_vgr_pdf 

which renders all the test images to pdf in `/tmp`, invoke with `--help`
for more options.

Backends can be compared directly to each other by using the database
viewer which runs in your browser: 

    b0 -- .show-url db_viewer

## Adding a new test image

To add a new image either add it to an existing relevant file or
create a new file for it. In the latter case, include the new file in
[`db_contents.ml`](db_contents.ml).

An image is added by calling the function `Db.image` 
(see [`test/db.mli`](test/db.mli)) at the toplevel of the file:

```ocaml
Db.image "a-unique-string-id" __POS__
  ~author:("Author name", "scheme://uri/to/author")
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





