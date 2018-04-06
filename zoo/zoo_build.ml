module B = Owl_newt.Backends.Container_REST

let collect_source_files = ()

let build gist =
  let temp_dir = collect_source_files gist in
  B.preprocess temp_dir;
  B.gen_build_file temp_dir;
  B.build_exec temp_dir;
  B.wrap temp_dir
