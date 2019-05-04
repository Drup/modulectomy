
module T = Tree_layout


type node = { name : Info.name ; size : float ; data : Info.data option }

let rec area = function
  | Tree_layout.Node (x,[||]) -> x.size
  | Node (x, a) ->
    let v = areal a in
    assert (x.size >= v);
    x.size
and areal a = Iter.sumf @@ Iter.map area @@ Iter.of_array a

let rec to_tree_layout' (Info.T.T t) =
  Info.SMap.to_seq t
  |> Iter.map node_to_tree_layout
  |> Iter.to_array
  |> (fun x ->
      Array.sort (fun (T.Node (x1, _)) (Node (x2, _)) ->
          - (Float.compare x1.size x2.size)) x;
      x)
and node_to_tree_layout (name, {value; children}) =
  let a = to_tree_layout' children in
  let size =
    match value with
    | [] -> areal a
    | l -> 
      Iter.of_list l
      |> Iter.filter_map (fun x -> CCOpt.map Int64.to_float x.Info.size)
      |> Iter.sumf
  in
  Tree_layout.Node ({name ; size ; data = CCList.head_opt value}, a)

let to_tree_layout x =
  let a = to_tree_layout' x in
  let size = areal a in
  Tree_layout.Node ({name ="" ; size; data = None}, a)

let ratio = 2.

let rect_of_tree t : Tree_layout.Common.rectangle =
  let a = area t in
  let h = sqrt (a/.ratio) in
  let w = ratio *. h in
  { p = { x = 0. ; y = 0. } ; w ; h }
let of_tree t =
  let t = to_tree_layout t in
  Tree_layout.treemap ~area (rect_of_tree t) t


let rec cut n (T.Node (x, a)) =
  if n <= 0
  then T.Node (x, [||])
  else Node (x, Array.map (cut (n-1)) a)

module Doc = struct
  open Tyxml
  open Tree_layout.Common

  let style_of_info info =
    match info with
    | None | Some {data = None ; _} -> "fill:transparent;stroke:black"
    | Some {data = Some {Info.kind = Function ; _ }; _}->
      "fill:#A3A2FF;stroke:black"
    | Some {data = Some {kind = Value ; _ } ; _} ->
      "fill:#32CE47;stroke:black"
    | Some _ ->
      "fill:#E4F866;stroke:black"

  let rect ?info ~level { p ; w ; h } =
    Svg.[
      rect ~a:[
        a_x (p.x, None) ; a_y (p.y, None) ;
        a_width (w, None) ; a_height (h, None) ;
        a_stroke_width (exp (-. 2. *. float level), None) ;
        a_style (style_of_info info) ;
      ][]
    ] @
    match info with
    | None -> []
    | Some info ->
      Svg.[text ~a:[
          a_x_list [p.x +. w/.2., None] ; a_y_list [p.y+.h/.2., None] ;
          a_text_anchor `Middle; a_dy_list [0.4, Some `Em];
          a_font_size @@ string_of_float @@ (w+.h)/.20. ;
        ] [txt @@ info.name] ;
        ]

  let list_map_array f a = List.map f @@ Array.to_list a
  let list_flatmap_array f a =
    List.concat @@ list_map_array f a

  let viewbox_of_rect { p ; w ; h } = Svg.a_viewBox (p.x, p.y, w, h)

  let rec svg_rect level (T.Node ((info,r), a)) =
    svg_rects (level+1) a @ rect ~info ~level r
  and svg_rects level a =
    list_flatmap_array (svg_rect level) a

  let treemap (T.Node ((_,r),a)) =
    let a = [
        (* a_style "width:100%;height:auto"; *)
        viewbox_of_rect r ;
      ]
    and t = (
      Svg.(title (txt @@ Printf.sprintf "Module treemap"))::
      svg_rects 0 a
    )
    in
    a, t

  let svg t =
    let a, t = treemap t in
    Svg.svg ~a t
  
  let html t =
    let attr, t = treemap t in
    let open Html in
    html
      (head (title (txt "Treemap")) [])
      (body [svg ~a:attr t])
  
end

let svg = Doc.svg
let html = Doc.html
let doc = html
