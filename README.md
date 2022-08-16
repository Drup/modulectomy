# Modulectomy

Dissect OCaml compiled programs, and weight their content.

`modulectomy` allows you to visualize the size of your OCaml programs and the
contributions by various libraries/modules. 

* Input: Currently only supports ELF binaries, optimally compiled with debug-information. JS support might come back.
* Output:
  * An interactive SVG + CSS treemap, showing how much space each module/library uses relative to eachother.
  * Optionally a scale-SVG extension, showing how much space the treemap constitutes of the full binary.

See [an example](https://builds.robur.coop/job/tlstunnel/build/7f0afdeb-0a52-4de1-b96f-00f654ce9249/viztreemap).

## Usage

For simple usage, se the definition of `squarify` in `src/main.ml`.

For more advanced usage, including scale-SVG, see how it's used in
[builder-web](https://git.robur.io/robur/builder-web/src/branch/main/bin/visualizations/builder_viz.ml#L18).