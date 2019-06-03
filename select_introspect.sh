#!/bin/sh
if ocamlc -config | grep 'tagprinter:'; then
  cp tag_introspect.mli introspect.mli
  cp tag_introspect.ml  introspect.ml
else
  cp raw_introspect.mli introspect.mli
  cp raw_introspect.ml  introspect.ml
fi
