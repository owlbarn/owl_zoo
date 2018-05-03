(*
 * OWL - an OCaml numerical library for scientific computing
 * Copyright (c) 2016-2017 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)


open Owl_zoo_utils

(** Consts *)

let conf_name = Owl_zoo_path.conf_name
let zoo_root  = Owl_zoo_path.dir

type t = {
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


(** Core functions *)
let make_snode name gist types =
  let gists = [|gist|] in
  let pn = Array.length types in
  let graph = Owl_graph.node (name, gist, pn) in
  {gists; types; graph}


let types_from_config config =
  Yojson.Basic.from_string config
    |> Yojson.Basic.Util.to_assoc
    |> List.map filter_str


let make_services gist =
  Owl_zoo_cmd.download_gist gist; (* should use cache if possible *)
  let conf_json = Owl_zoo_cmd.load_file ~gist conf_name in
  let nt_lst = types_from_config conf_json in
  let services = Hashtbl.create 1024 in
  List.iteri (fun i (n, t) ->
    Hashtbl.add services n (make_snode n gist (Array.of_list t))
  ) nt_lst;
  services


let get_service_info gist =
  let conf_json = Owl_zoo_cmd.load_file ~gist conf_name in
  let nt_lst = types_from_config conf_json in
  let info = ref "" in
  List.iteri (fun i (n, t) ->
    info := !info ^
      Printf.sprintf "[gist]  %s\n" gist ^
      Printf.sprintf "[name]  %s\n" n ^
      Printf.sprintf "[type]  %s\n" (t |> Array.of_list |> join ~delim:" -> ")
  ) nt_lst;
  print_endline !info


let list_nodes s =
  let g = get_graph s in
  let result = ref "" in
  let iterfun node =
    let name, gist, pn = Owl_graph.attr node in
    result := !result ^
      Printf.sprintf "node: (%s, %s, %d)\n" name gist pn
  in
  Owl_graph.iter_descendants iterfun [|g|];
  !result


(* generate service.json *)
let generate_conf ?(dir=".") service mname =
  let name = String.capitalize_ascii mname ^ ".main" in
  let types = get_types service |> join ~delim:" -> " in
  let json = `Assoc [(name, `String types)] in
  let dir = if dir = "." then Sys.getcwd () else dir in
  Yojson.Basic.to_file (dir ^ "/" ^ conf_name) json


(* generate an entry file called mname.ml based on service *)
let generate_main ?(dir=".") service mname =
  let header = ref "" in
  Array.iter (fun gist ->
    header := !header ^ (Printf.sprintf "#zoo \"%s\"\n" gist)
  ) (get_gists service);

  let p_num = Array.length (in_types service) in
  let params = Array.make p_num "" in
  for i = 0 to (p_num - 1) do
    params.(i) <- "p" ^ (string_of_int i)
  done;
  let p_str = join params in

  (* get number of nodes *)
  let cnt  = ref 0 in
  let iter_count _ =
    cnt := !cnt + 1
  in
  cnt := !cnt - 1;
  Owl_graph.iter_descendants iter_count [|(get_graph service)|];

  (* build body reversely *)
  let body = ref (Printf.sprintf "  r%d\n" !cnt) in
  let pcnt = ref p_num in
  let iterfun node =
    let name, gist, pn = Owl_graph.attr node in
    let pn = pn - 1 in
    let ps =
      if !cnt = 0 then
        (pcnt := !pcnt - (pn - 1) - 1;
        join (Array.sub params !pcnt pn))
      else
        (pcnt := !pcnt - (pn - 1);
        let p_str' = join (Array.sub params !pcnt (pn - 1)) in
        "r" ^ (string_of_int (!cnt - 1)) ^ " " ^ p_str') (* not general enough *)
    in
    body := (Printf.sprintf "  let r%d = %s %s in\n" !cnt name ps) ^ !body;
    cnt := !cnt - 1
  in
  Owl_graph.iter_descendants iterfun [|(get_graph service)|];

  let output_string = "#!/usr/bin/env owl\n" ^ !header ^
    (Printf.sprintf "let main %s =\n%s" p_str !body) in

  let dir = if dir = "." then Sys.getcwd () else dir in
  Owl_io.write_file (dir ^ "/" ^ mname ^ ".ml") output_string


let save_service service name =
  let tmp_dir = Owl_zoo_utils.mk_temp_dir "service" in
  generate_main ~dir:tmp_dir service name;
  generate_conf ~dir:tmp_dir service name;
  Owl_io.write_file (tmp_dir ^ "/#readme.md") name;
  let gist = Owl_zoo_cmd.upload_gist tmp_dir in
  gist


(** Compose operations *)

let connect_service ?(name="") inputlist output =
  (* manual type check *)
  let connect_two s1 s2 idx =
    assert (Array.mem (out_type s1) (in_types s2)); (* incompatible service type *)
    assert (out_type s1 = (Array.get (in_types s2) idx)); (* incompatible argement type *)

    let gists = merge_array (get_gists s1) (get_gists s2) in
    let types = Owl_utils_array.replace idx 1 (get_types s2) (in_types s1) in
    let graph = get_graph s2 in
    let graph_cld = get_graph s1 in
    Owl_graph.connect [|graph|] [|graph_cld|];
    {gists; types; graph}
  in

  let len = List.length inputlist in
  let input_rev = List.rev inputlist in
  let result = ref output in
  List.iteri (fun i x ->
    result := connect_two x !result (len - 1 - i);
  ) input_rev;
  [!result]


(** operators *)

let ( $> ) = connect_service
let ( $  ) = make_services
let ( $~ ) = Hashtbl.find
