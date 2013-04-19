Test images for Vg
==================

The images are a collection of renderer independent tests. Ideally,
for each test, renderers should produce identical results. However due
to differences in renderer expressiveness and purposes this may not be
possible or desirable.

## Using the tests

Renderer writers that want to use the database should consult
`test.mli` and `../vgtest.ml`. For non-interactive renderers, it should
be a matter of adding a line to `vgtest.ml`.


## Adding new tests

To add a new test either add it to an existing file with a relevant
name or create a new file for it. In the latter case the new file must
be included at the top of test.ml so that ocamlbuild can link it in
automatically.

A test is added by calling the function test defined in testing.mli as
follows :

```ocaml
 test "mytest"                                (* test name, must be unique. *) 
 ~info: "description of the image"
 ~note: "particular things to look for in the result"          (* optional. *)
 ~size: (Size2.v w h)                               (* output surface size. *)
 ~view: (Box2.v (P2. x y) (Size2.v w' h')         (* output view rectangle. *)
 & lazy begin
   (* Define a value of type Vg.image here *)
   I.void
 end
```

To make it easy to extract the image value of a test, its definition
should be self-contained. This may imply a lot of code redundancy
between similar tests but it turns tests into educational examples
that can be simply copy-pasted to play around.

