(* TODO *)

val preprocess : string -> unit
(**
``preprocess dir``
*)

val gen_build_files : string -> string -> unit
(**
``gen_build_files dir gist``
*)

val build_exec : string -> unit
(**
``build_exec dir ``
*)

val postprocess : string -> string -> unit
(**
``postprocess dir name``
*)
