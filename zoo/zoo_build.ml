(*module B = Owl_newt.Backends.Container_REST *)

(* assumes no file name collision among different gists and no subdir *)

let rec extract_zoo_gist f added dir =
  let s = Owl_utils.read_file_string f in
  let regex = Str.regexp "^#zoo \"\\([0-9A-Za-z]+\\)\"" in
  try
    let pos = ref 0 in
    while true do
      pos := Str.search_forward regex s !pos;
      let gist = Str.matched_group 1 s in
      pos := !pos + (String.length gist);
      process_dir_zoo ~added gist dir
    done
  with Not_found -> ()

and process_dir_zoo ?added gist dir =
  let gid, vid, _, _ = Owl_zoo_ver.parse_gist_string gist in
  let gist' = Printf.sprintf "%s/%s" gid vid in

  let added = match added with
    | Some h -> h
    | None   -> Hashtbl.create 128
  in
  if Hashtbl.mem added gist' = false then (
    Hashtbl.add added gist' gist';
    Owl_zoo_dir.download_gist gid vid;

    let dir_gist = Owl_zoo_path.gist_path gid vid in
    Sys.readdir (dir_gist)
    |> Array.to_list
    |> List.filter (fun s -> Filename.check_suffix s "ml")
    |> List.iter (fun l ->
        let f = Printf.sprintf "%s/%s" dir_gist l in
        extract_zoo_gist f added dir
      );

    let cmd = Printf.sprintf "find ~/.owl/zoo/%s -iname '*' -exec cp {} %s"
      gist' dir in
    Sys.command cmd |> ignore;
  )

let collect_source_files gist =
  let tmp_dir = Zoo_utils.mk_temp_dir "newt" in
  process_dir_zoo gist tmp_dir

let build gist =
  let temp_dir = collect_source_files gist in

  (* B.preprocess temp_dir;
  B.gen_build_file temp_dir;
  B.build_exec temp_dir;
  B.wrap temp_dir *)

  ()
