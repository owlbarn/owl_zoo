open Owl_zoo_utils


let conf_name = "service.json"
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


let make_services gist =
  Owl_zoo_cmd.download_gist gist; (* should use cache if possible *)
  let conf_json = Owl_zoo_cmd.load_file gist conf_name in
  let nt_lst = Yojson.Basic.from_string conf_json
    |> Yojson.Basic.Util.to_assoc
    |> List.map filter_str
  in
  let services = Array.make (List.length nt_lst)
    (make_snode "" "" [|""|]) in
  List.iteri (fun i (n, t) ->
    services.(i) <- make_snode n gist (Array.of_list t)
  ) nt_lst;
  services

let get_service_info gist =
  let conf_json = Owl_zoo_cmd.load_file gist conf_name in
  let nt_lst = Yojson.Basic.from_string conf_json
    |> Yojson.Basic.Util.to_assoc
    |> List.map filter_str
  in
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


(* generate a entry file called mname.ml based on service *)
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
  save_file (dir ^ "/" ^ mname ^ ".ml") output_string


let save_service service name =
  let tmp_dir = Filename.get_temp_dir_name () ^ "/" ^
    (string_of_int (Random.int 100000)) in
  Sys.command ("mkdir " ^ tmp_dir) |> ignore;

  generate_main ~dir:tmp_dir service name;
  generate_conf ~dir:tmp_dir service name;
  save_file (tmp_dir ^ "/readme.md") name;
  let gist = Owl_zoo_cmd.upload_gist tmp_dir in
  gist
