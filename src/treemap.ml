
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
  Info.SMap.to_seq t
  |> Seq.map (node_to_tree_layout path)
  |> List.of_seq
  |> List.sort (fun t1 t2 -> - (Float.compare (area t1) (area t2)))
  |> Iter.of_list

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

module Doc = struct
  open Tyxml
  open Tree_layout.Common

  let css = {|
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
  stroke:black;
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
|}

  let area_of_pos {w ; h ; _ } = h *. w
  
  let class_from_info (info : node) =
    let l = match info.data.location with
      | Some _ -> []
      | None -> ["unlocated"]
    in
    Info.to_string info.data.kind :: l

  let title_of_info info area =
    let pp_size ppf f =
      let (fmt : _ format), f =
        if f < 1024. then "%.0fB", f
        else if f < 1024.*.1024. then "%.2fkB", f/.1024.
        else "%.2fMB", f/.1024./.1024.
      in              
      Format.fprintf ppf fmt f
    in
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
    let stroke = exp (-. 1.5 *. float level) in
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

  let treemap r trees =
    let a = [
        (* a_style "width:100%;height:auto"; *)
        viewbox_of_rect r ;
      ]
    and t = (
      svg_rects 0 trees
    )
    in
    a, t

  let svg {rect; trees} =
    let a, t = treemap rect trees in
    Svg.svg ~a t
  
  let html {rect; trees} =
    let attr, t = treemap rect trees in
    let open Html in
    html
      (head (title (txt "Treemap")) [style [Unsafe.data css]])
      (body [svg ~a:attr t])
  
end

let svg = Doc.svg
let html = Doc.html
let doc = html
