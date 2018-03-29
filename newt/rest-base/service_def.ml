type service = {
  mutable gists : string array;
  mutable types : string array;
  mutable graph : (string * string * int) Owl_graph.node; 
}

let get_gists s = s.gists 
let get_types s = s.types
let get_graph s = s.graph

let in_types s = 
  let lst = Array.to_list (get_types s) in
  List.(lst |> rev |> tl |> rev) |> Array.of_list

let out_type s = 
  let lst = Array.to_list (get_types s) in
  List.(lst |> rev |> hd)
