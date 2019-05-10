# Modulectomy

Dissect OCaml compiled programs, and weight their content.

`modulectomy` allows you to visualize the size of your OCaml programs and the
contributions by various libraries and modules. Very WIP.

Currently support:
- js files compiled with `js_of_ocaml`, if they have source maps.
- Will work on native as soon as https://github.com/let-def/owee/issues/3 is fixed.

See [an example](https://drup.github.io/modulectomy/example/planet.html).

## Install

Requires a patched version of js_of_ocaml available [here](https://github.com/ocsigen/js_of_ocaml/pull/795) and the dev version of `tree_layout`.
