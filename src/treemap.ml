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

  let pp_size ppf f =
    let (fmt : _ format), f =
      if f < 1024. then "%.0fB", f
      else if f < 1024.*.1024. then "%.2fkB", f/.1024.
      else "%.2fMB", f/.1024./.1024.
    in              
    Format.fprintf ppf fmt f

  let stroke_width = 0.6

  let css = sp {|
.unlocated {
  filter:blur(0.1);
}
.functor {
  fill:#867613;
}
.function {
  fill:#F1E8AE;
}
.module {
  fill:#123557;
}
.value {
  fill:#74899D;
}
.primitive {
  fill:#CE6C6C;
}
.unknown {
  fill:#6AFF8F;
}
.border {
  stroke:gray;
  fill:none;
}
.label,.header {
  font-family:monospace;
  fill:black;
  stroke:none;
}
.leaf:hover > .fill,
.node:hover > .fill {
  filter: brightness(1.4);
}
.node > .header:hover ~ * .fill,
.node > .fill:hover ~ * .fill {
  filter: brightness(1.4);
}
.functor > text, .module > text {
  fill:white;
}


.scale-header {
  fill:white;
}
.scale-fill:hover {
  filter: grayscale(0%%) !important;
}
.scale-fill:hover ~ g {
  filter: grayscale(0%%) !important;
}
.scale-node > .scale-fill:hover ~ * .scale-fill {
  filter: grayscale(0%%) !important;
}
.scale-line {
  stroke-width: %f;
}
svg {
  stroke-width: 0;
}
|} stroke_width
(* stroke: rgb(0,0,0); *)
(* svg {
 *   padding-top: 8px;
 * } *)
  
  module Treemap = struct

    open Tree_layout.Common

    let area_of_pos {w ; h ; _ } = h *. w

    let class_from_info (info : node) =
      let l = match info.data.location with
        | Some _ -> []
        | None -> ["unlocated"]
      in
      Info.to_string info.data.kind :: l

    let title_of_info info area =
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
          pp_size area
          (Info.to_string info.data.kind)
          (* pp_file info.data.location *)
      in
      Svg.(title (txt s))

    let mk_border ~level { p ; w ; h } =
      (* let stroke = exp (-. 1.5 *. float level) in *)
      let stroke = 20. in
      Svg.[
        rect ~a:[
          a_class ["border"] ;
          a_x (p.x, None) ; a_y (p.y, None) ;
          a_width (w, None) ; a_height (h, None) ;
          a_stroke_width (stroke, None) ;
        ] []
      ]
    let mk_rect { p ; w ; h } =
      Svg.[
        rect ~a:[
          a_class ["fill"];
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

    let leaf ~info ~level pos =
      (* let angle = -.180.*.tanh (pos.h/.pos.w)/.Float.pi in
       * let center = pos.p.x+.pos.w/.2. , pos.p.y+.pos.h/.2. in *)
      let label = 
        Svg.[text ~a:(
          a_class ["label"] ::
          a_dominant_baseline `Central ::
          (* a_transform [`Rotate ((angle, None), Some center)] :: *)
          (a_font_size @@ string_of_float @@ (pos.w+.pos.h)/.20.) ::
          a_center_position pos;
        ) [txt @@ info.label] ;
        ]
      in
      let title = title_of_info info @@ area_of_pos pos in
      Svg.g
        ~a:[Svg.a_class ("leaf" :: class_from_info info)]
        (title :: mk_rect pos @ label @ mk_border ~level pos)

    let header_node ~info pos =
      let header_pos = {pos with h = pos.h/.13.} in
      let label =
        Svg.[text ~a:(
          a_class ["header"] ::
          a_dominant_baseline `Hanging ::
          (a_font_size @@ string_of_float @@ header_pos.h) ::
          a_left_position pos.p;
        ) [txt @@ info.label] ;
        ]
      in
      mk_rect pos @ label

    let node ~info ~level pos children =
      let title = title_of_info info @@ area_of_pos pos in
      let header = header_node ~info pos in
      Svg.g
        ~a:[Svg.a_class ("node" :: class_from_info info)]
        (title :: header @ children @ mk_border ~level pos)

    let list_map_array f a = List.map f @@ Array.to_list a
    let list_flatmap_array f a =
      List.concat @@ list_map_array f a

    let viewbox_of_rect { p ; w ; h } = Svg.a_viewBox (p.x, p.y, w, h)

    let rec svg_rect level (T.Node ((info,r), a)) =
      if Array.length a = 0 then
        leaf ~info ~level r
      else
        let children = svg_rects (level+1) @@ Iter.of_array a in
        node ~info ~level r children
    and svg_rects level a =
      Iter.map (svg_rect level) a |> Iter.to_list

    let make r trees =
      let a = [
        (* a_style "width:100%;height:auto"; *)
        viewbox_of_rect r ;
      ]
      and t = (
        svg_rects 0 trees
      )
      in
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
          a_class ["scale-fill"]; 
          a_style style_str;
          a_x @@ pct x;
          a_y @@ pct y;
          a_width @@ pct w;
          a_height @@ pct h;
        ] []
      )

    (*goto howto
      * either render blocks beside eachother, 
        * or render on top, like the tree does - 
          * ! so on mouse-over the actual range is highlighted 
      * note for tree-rendering;
        * scale is a rosetree of 1 level
          * root is whole binary 
          * children are siblings under root
      * ui
        * add pct text to each leaf-block?
    *)

    let render_label label = Svg.(
      text ~a:[
        a_class ["scale-header"];
        a_dominant_baseline `Hanging;
        a_text_anchor `Start;
        a_font_size @@ "0.115em";
        a_x_list [ pct 1. ];
        a_y_list [ pct 25. ];
      ] [txt @@ label] ;
    )

    let line ~x0 ~y0 ~x1 ~y1 =
      Svg.(line ~a:[
        a_class [ "scale-line" ];
        a_x1 @@ pct x0;
        a_y1 @@ pct y0;
        a_x2 @@ pct x1;
        a_y2 @@ pct y1;
      ]) []

    let render_scale_pointer ~color ~pct =
      let stump_len = 17. in
      let line_width = stroke_width in
      let padding_horiz = 0.5 in
      let padding_vert = 7.0 in
      let scale_line =
        let lr_stump_y = 100. in
        (* let line_y = lr_stump_y -. stump_len -. line_width /. 2. in  *)
        let line_y = 75. +. padding_vert /. 2. in 
        let line_x0 = 0. (* +. padding_horiz *) in
        let l_stump_x = line_x0 +. line_width /. 2. -. 0.00 in
        (* let line_x1 = treemap_pct -. padding_horiz in *)
        let line_x1 = 100. (* -. padding_horiz *) in
        let r_stump_x = line_x1 -. line_width /. 2. +. 0.00  in
        let m_stump_x = pct /. 2. in
        let m_stump_y = 50. (* +. padding_vert *) in
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

    let render_scale_tree tree =
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
                render_scale_pointer ~color ~pct;
                Svg.g children_svgs
              ]
            | false -> [
                Svg.title @@ Svg.txt title;
                rect ~color ~w:pct ~h:50. ~x:acc_pct ~y:0.;
                Svg.g children_svgs
              ]
            (*< goto return aspect ratio (or something else) 
              to be able to make correctly sized iframe*)
            (* render_label @@ sp "%.0f%%" pct; *)
          in
          let svg = Svg.g ~a:[
            Svg.a_class ["scale-node"];
          ] svg_content
          in
          svg :: acc_children, pct +. acc_pct
      in
      aux ([], 0.) tree |> fst

    let make ~treemap_size ~binary_size ~subtrees:input_subtrees =
      let binary_size = float binary_size in
      assert (binary_size >= treemap_size);
      let treemap_pct = 100. *. treemap_size /. binary_size in
      let size_string tag size =
        Format.asprintf "%s: %a" tag pp_size size
      in
      let input_subtrees =
        input_subtrees |> List.map (fun (tag, size) ->
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
      let scale_svg = render_scale_tree scale_tree in
      let a = [ Svg.a_viewBox (0., 0., 100., 6.) ] in
      a, scale_svg

  end

  let svg { rect; trees } =
    let a, t = Treemap.make rect trees in
    Svg.svg ~a t

  let html_with_scale ~binary_size ~scale_chunks { rect; trees } =
    let a_tree, treemap = Treemap.make rect trees in
    let treemap_size = rect.w *. rect.h in
    let a_scale, scale =
      Scale.make ~treemap_size ~binary_size ~subtrees:scale_chunks in
    H.html
      (H.head (H.title (H.txt "Treemap")) [H.style [H.Unsafe.data css]])
      (H.body [
          H.svg ~a:a_scale scale;
          H.svg ~a:a_tree treemap;
        ])

  let html { rect; trees } =
    let a_tree, treemap = Treemap.make rect trees in
    H.html
      (H.head (H.title (H.txt "Treemap")) [H.style [H.Unsafe.data css]])
      (H.body [
          H.svg ~a:a_tree treemap;
        ])

end

let to_svg = Render.svg
let to_html = Render.html
let to_html_with_scale = Render.html_with_scale


