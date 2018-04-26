open Owl_zoo_utils

module JS = struct

type backend_typ = {
  mutable fname: string  (* js file name *)
}

let generate_jbuild dir name =
  let output_str = Printf.sprintf "
  (jbuild_version 1)

  (executables
   ((libraries (owl_base owl_zoo))
    (names (%s))))

    (rule
     ((targets (%s.js))
      (action
        (run ${bin:js_of_ocaml} --noruntime ${lib:js_of_ocaml-compiler:runtime.js}
             --source-map ${path:%s.bc} -o ${@} --pretty))))
  " name name name
  in
  save_file (dir ^ "/jbuild") output_str


let _replace_str a b dir =
  let cmd = Printf.sprintf "find %s -name \"*.ml\" -exec sed -i 's/%s/%s/g' {} \\;" dir a b in
  Sys.command(cmd) |> ignore

let preprocess dir =
  let cmd1 = Printf.sprintf "find %s -name \"*.ml\" -exec sed -i '/^#/d' {} \\;" dir in
  Sys.command(cmd1) |> ignore;
  _replace_str "open Owl" "open Owl_base" dir;
  _replace_str "Dense.Ndarray" "Owl_base_dense_ndarray" dir


let get_sname dir =
  let json_lst = Yojson.Basic.from_file (dir ^ "/server.json")
  |> Yojson.Basic.Util.to_assoc
  |> List.map filter_str
  in
  let n, _ = List.hd json_lst in
  n |> Str.split (Str.regexp ".") |> List.hd |> String.lowercase_ascii

let gen_build_files dir gist =
  let name = get_sname dir in
  generate_jbuild dir name


let build_exec dir =
  let name = get_sname dir in
  Sys.command(Printf.sprintf "(cd dir; jbuilder build %s.js)" name) |> ignore


let postprocess dir name = ()

end
