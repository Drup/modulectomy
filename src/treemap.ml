
let visualization_version = 1
(** Remember to increment this when anything changes that can affect the 
    visualization, e.g.:
      * algorithm change
      * UI change
      * certain library-dependency changes 
*)

let sp = Printf.sprintf

module T = Tree_layout

type node = {
  path : Info.name list ;
  label : Info.name ;
  size : float ;
  data : Info.data
}

type t = {
  rect : T.Common.rectangle ;
  trees : (node * T.Common.rectangle) T.tree Iter.t ;
}

let rec area = function
  | Tree_layout.Node (x,[||]) -> x.size
  | Node (v, a) ->
    let s = areal @@ Iter.of_array a in
    s +. v.size

and areal a = Iter.sumf @@ Iter.map area a

let rec to_tree_layout path (Info.T.T t) =
  Info.SMap.to_iter t
  |> Iter.map (node_to_tree_layout path)
  |> Iter.sort ~cmp:(fun t1 t2 -> - (Float.compare (area t1) (area t2)))

and node_to_tree_layout path (label, {value; children}) =
  let new_path = path @ [label] in
  let a = to_tree_layout new_path children in
  let size, children = match value.Info.size, Iter.to_array a with
    | (None | Some 0L), a -> 0., a
    | s, [||] -> CCOption.map_or ~default:0. Int64.to_float s , [||]
    | Some i, a ->
      let size = Int64.to_float i in
      let v = { path = new_path ; label = "" ; size ; data = value } in
      let internal_node = T.Node (v, [||]) in
      0., Array.append [|internal_node|] a 
  in
  Tree_layout.Node ({path ; label ; size ; data = value}, children)


let ratio = 1.

let rect_of_tree t : Tree_layout.Common.rectangle =
  let a = areal t in
  let h = sqrt (a/.ratio) in
  let w = ratio *. h in
  { p = { x = 0. ; y = 0. } ; w ; h }

let sub { Tree_layout.Common. p ; w ; h } =
  let h' = h /.1.1 in
  let dy = h -. h' in
  let p = { p with y = p.y +. dy } in
  Tree_layout.Common.{ p ; w ; h = h'} 

let of_tree l =
  let l = to_tree_layout [] l in
  let rect = rect_of_tree l in
  { rect ; trees = Tree_layout.treemap ~area rect l}

module Render = struct
  open Tyxml

  let stroke_width = 0.6

  let css = sp {|
.treemap-unlocated {
  filter:blur(0.1);
}
.treemap-functor {
  fill:#867613;
}
.treemap-function {
  fill:#F1E8AE;
}
.treemap-module {
  fill:#123557;
}
.treemap-value {
  fill:#74899D;
}
.treemap-primitive {
  fill:#CE6C6C;
}
.treemap-unknown {
  fill:#6AFF8F;
}
.treemap-border {
  stroke:gray;
  fill:none;
}
.treemap-label,.treemap-header {
  font-family:monospace;
  fill:black;
  stroke:none;
}
.treemap-leaf:hover > .treemap-fill,
.treemap-node:hover > .treemap-fill {
  filter: brightness(1.4);
}
.treemap-node > .treemap-header:hover ~ * .treemap-fill,
.treemap-node > .treemap-fill:hover ~ * .treemap-fill {
  filter: brightness(1.4);
}
.treemap-functor > text, .treemap-module > text {
  fill:white;
}


.treemap-scale-header {
  fill:white;
}
.treemap-scale-fill:hover {
  filter: grayscale(0%%) !important;
}
.treemap-scale-fill:hover ~ g {
  filter: grayscale(0%%) !important;
}
.treemap-scale-node > .treemap-scale-fill:hover ~ * .treemap-scale-fill {
  filter: grayscale(0%%) !important;
}
.treemap-scale-line {
  stroke-width: %f;
}

.treemap-svg-wrap {
  stroke-width: 0;
}
|} stroke_width
  
  let scoped_class s = "treemap-"^s

  module Treemap = struct

    open Tree_layout.Common

    let area_of_pos {w ; h ; _ } = h *. w

    let class_from_info (info : node) =
      let l = match info.data.location with
        | Some _ -> []
        | None -> [scoped_class "unlocated"]
      in
      let kind_class =
        info.data.kind
        |> Info.to_string
        |> scoped_class
      in
      kind_class :: l

    let title_of_info info area =
      let area = truncate area in
      (* let pp_file ppf = function
       *   | None -> ()
       *   | Some (f,_,_) ->
       *     Format.fprintf ppf "@.file: %a"
       *       Fpath.pp
       *       Fpath.(normalize @@ v f)
       * in *)
      let sep : _ format = if info.data.kind = Primitive then "-" else "." in
      let pp_path = CCFormat.(list ~sep:(return sep) string) in
      let s =
        Format.asprintf
          "name: %a@.size: %a@.type: %s"
          pp_path (info.path @ [info.label])
          Fmt.byte_size area
          (Info.to_string info.data.kind)
          (* pp_file info.data.location *)
      in
      Svg.(title (txt s))

    let make_border { p ; w ; h } =
      (* let stroke = exp (-. 1.5 *. float level) in *)
      let stroke = 20. in
      Svg.[
        rect ~a:[
          a_class [scoped_class "border"] ;
          a_x (p.x, None) ; a_y (p.y, None) ;
          a_width (w, None) ; a_height (h, None) ;
          a_stroke_width (stroke, None) ;
        ] []
      ]

    let make_rect { p ; w ; h } =
      Svg.[
        rect ~a:[
          a_class [scoped_class "fill"];
          a_x (p.x, None) ; a_y (p.y, None) ;
          a_width (w, None) ; a_height (h, None) ;
        ] []
      ]

    let a_center_position { p ; w ; h } = Svg.[
      a_x_list [p.x +. w/.2., None] ;
      a_y_list [p.y +. h/.2., None] ;
      a_text_anchor `Middle;
    ]

    let a_left_position p = Svg.[
      a_x_list [p.x, None] ;
      a_dx_list [1.,Some `Px] ;
      a_y_list [p.y, None] ;
      a_text_anchor `Start;
    ]

    let leaf ~info pos =
      (* let angle = -.180.*.tanh (pos.h/.pos.w)/.Float.pi in
       * let center = pos.p.x+.pos.w/.2. , pos.p.y+.pos.h/.2. in *)
      let label = 
        Svg.[text ~a:(
          a_class [scoped_class "label"] ::
          a_dominant_baseline `Central ::
          (* a_transform [`Rotate ((angle, None), Some center)] :: *)
          (a_font_size @@ string_of_float @@ (pos.w+.pos.h)/.20.) ::
          a_center_position pos;
        ) [txt @@ info.label] ;
        ]
      in
      let title = title_of_info info @@ area_of_pos pos in
      Svg.g
        ~a:[Svg.a_class (scoped_class "leaf" :: class_from_info info)]
        (title :: make_rect pos @ label @ make_border pos)

    let header_node ~info pos =
      let header_pos = {pos with h = pos.h/.13.} in
      let label =
        Svg.[text ~a:(
          a_class [scoped_class "header"] ::
          a_dominant_baseline `Hanging ::
          (a_font_size @@ string_of_float @@ header_pos.h) ::
          a_left_position pos.p;
        ) [txt @@ info.label] ;
        ]
      in
      make_rect pos @ label

    let node ~info pos children =
      let title = title_of_info info @@ area_of_pos pos in
      let header = header_node ~info pos in
      Svg.g
        ~a:[Svg.a_class (scoped_class "node" :: class_from_info info)]
        (title :: header @ children @ make_border pos)

    let list_map_array f a = List.map f @@ Array.to_list a
    let list_flatmap_array f a =
      List.concat @@ list_map_array f a

    let viewbox_of_rect { p ; w ; h } = Svg.a_viewBox (p.x, p.y, w, h)

    let rec svg_rect level (T.Node ((info,r), a)) =
      if Array.length a = 0 then
        let eps = 0.0001 in
        if info.size < eps then None else
          Some (leaf ~info r)
      else
        let children = svg_rects (level+1) @@ Iter.of_array a in
        Some (node ~info r children)
    and svg_rects level a =
      Iter.filter_map (svg_rect level) a |> Iter.to_list

    let make r trees =
      let a = Svg.[
        viewbox_of_rect r;
        a_class [ scoped_class "svg-wrap" ];
      ]
      and t = svg_rects 0 trees in
      a, t

  end

  module H = Html

  module Scale = struct

    module Rose_tree = struct 

      type 'a t = Node of 'a * 'a t list

      let node v t = Node (v, t)

    end

    let pct x = x, Some `Percent

    let style_of_color (r, g, b) =
      let color_str = sp "rgb(%d,%d,%d)" r g b in
      sp "stroke: %s; fill: %s; filter: grayscale(100%%);" color_str color_str 
    
    let rect ~color ~w ~h ~x ~y =
      let style_str = style_of_color color in
      Svg.(
        rect ~a:[
          a_class [scoped_class "scale-fill"]; 
          a_style style_str;
          a_x @@ pct x;
          a_y @@ pct y;
          a_width @@ pct w;
          a_height @@ pct h;
        ] []
      )

    let make_label label = Svg.(
      text ~a:[
        a_class [scoped_class "scale-header"];
        a_dominant_baseline `Hanging;
        a_text_anchor `Start;
        a_font_size @@ "0.115em";
        a_x_list [ pct 1. ];
        a_y_list [ pct 25. ];
      ] [txt @@ label] ;
    )

    let line ~x0 ~y0 ~x1 ~y1 =
      Svg.(line ~a:[
        a_class [ scoped_class "scale-line" ];
        a_x1 @@ pct x0;
        a_y1 @@ pct y0;
        a_x2 @@ pct x1;
        a_y2 @@ pct y1;
      ]) []

    let make_scale_pointer ~color ~pct =
      let line_width = stroke_width in
      let padding_vert = 7.0 in
      let scale_line =
        let lr_stump_y = 100. in
        let line_y = 75. +. padding_vert /. 2. in 
        let line_x0 = 0. in
        let l_stump_x = line_x0 +. line_width /. 2. in
        let line_x1 = 100. in
        let r_stump_x = line_x1 -. line_width /. 2. in
        let m_stump_x = pct /. 2. in
        let m_stump_y = 50. in
        let stumps = Svg.g [
          line ~x0:m_stump_x ~y0:m_stump_y ~x1:m_stump_x ~y1:line_y;
          line ~x0:l_stump_x ~y0:lr_stump_y ~x1:l_stump_x ~y1:line_y;
          line ~x0:r_stump_x ~y0:lr_stump_y ~x1:r_stump_x ~y1:line_y; 
        ]
        in
        let a = [ Svg.a_style (style_of_color color) ] in
        Svg.g ~a [
          stumps;
          line ~x0:line_x0 ~y0:line_y ~x1:line_x1 ~y1:line_y
        ]
      in
      scale_line

    let make_scale_tree tree =
      let rec aux (acc_children, acc_pct) = function
        | Rose_tree.Node ((pct, title, scale_pointer), children) ->
          let children_svgs, _ =
            children |> List.fold_left aux ([], acc_pct) in
          let c max_v =
            (0.4 +. 1.0 *. (1. -. pct /. 100.))
            *. max_v
            |> truncate in
          let color = c 109., c 109., c 255. in
          let svg_content = match scale_pointer with
            | true -> [
                Svg.title @@ Svg.txt title;
                rect ~color ~w:pct ~h:50. ~x:acc_pct ~y:0.;
                make_scale_pointer ~color ~pct;
                Svg.g children_svgs
              ]
            | false -> [
                Svg.title @@ Svg.txt title;
                rect ~color ~w:pct ~h:50. ~x:acc_pct ~y:0.;
                Svg.g children_svgs
              ]
            (*< TODO return aspect ratio (or something else) 
              to be able to make correctly sized container 
              (though treemap might always be square) *)
            (* make_label @@ sp "%.0f%%" pct; *)
          in
          let svg = Svg.g ~a:[
            Svg.a_class [scoped_class "scale-node"];
          ] svg_content
          in
          svg :: acc_children, pct +. acc_pct
      in
      aux ([], 0.) tree |> fst

    let make ~treemap_size ~binary_size ~sub_chunks =
      let binary_size = float binary_size in
      assert (binary_size >= treemap_size);
      let treemap_pct = 100. *. treemap_size /. binary_size in
      let size_string tag size =
        (*TODO: use integers for sizes throughout*)
        let size = truncate size in 
        Format.asprintf "%s: %a" tag Fmt.byte_size size
      in
      let input_subtrees =
        sub_chunks |> List.map (fun (tag, size) ->
          let size = Int64.to_float size in
          let pct = 100. *. size /. binary_size in
          Rose_tree.node (pct, size_string tag size, false) []
        )
      in
      let scale_tree = Rose_tree.(
        node (100., size_string "Binary" binary_size, false) (
          node (treemap_pct, size_string "Treemap" treemap_size, true) []
          :: input_subtrees
        ))
      in
      let scale_svg = make_scale_tree scale_tree in
      let a = Svg.[
        a_viewBox (0., 0., 100., 6.);
        a_class [ scoped_class "svg-wrap" ];
      ] in
      a, scale_svg

  end

  let merge_css = String.concat "\n"
  
  let html_with_scale
      ~binary_size
      ~scale_chunks
      ?(override_css="")
      { rect; trees }
    =
    let a_tree, treemap = Treemap.make rect trees in
    let treemap_size = rect.w *. rect.h in
    let a_scale, scale =
      Scale.make ~treemap_size ~binary_size ~sub_chunks:scale_chunks in
    H.html
      (H.head (H.title (H.txt "Treemap")) [
          H.style [H.Unsafe.data @@ merge_css [ css; override_css ]]
        ])
      (H.body [
          H.svg ~a:a_scale scale;
          H.svg ~a:a_tree treemap;
        ])

  let html ?(override_css="") { rect; trees } =
    let a_tree, treemap = Treemap.make rect trees in
    H.html
      (H.head (H.title (H.txt "Treemap")) [
          H.style [H.Unsafe.data @@ merge_css [ css; override_css ]]
        ])
      (H.body [
          H.svg ~a:a_tree treemap;
        ])

end

let to_html = Render.html
(** [Treemap.to_html ?override_css tree] renders the interactive Treemap-SVG 
    as HTML including the needed CSS*)

let to_html_with_scale = Render.html_with_scale
(** [Treemap.to_html_with_scale ?override_css ~binary_size ~scale_chunks tree] 
    Renders both the interactive Treemap-SVG and Scale-SVG as HTML, 
    including their needed CSS. 
    The Scale-SVG shows the size of data rendered by the Treemap, relative to 
    the binary size and other 'scale_chunks' of data. 

    The [scale_chunks] is a list of names and sizes of chunks of the binary,
    which are not included in the treemap. Can e.g. be used to show excluded
    modules.

    The full [binary_size] in bytes needs to be supplied. 

    [override_css] lets you supply a CSS string that is appended, which 
    therefore lets you add new, or override existing CSS selectors.
*)


