module JS : sig

type backend_typ = {
  mutable fname: string  (* js file name *)
}

val preprocess : string -> unit
(**
``preprocess dir``removes the zoo header from ".ml" files in a ``dir``, and replace the usage of Owl library to Owl-base.
*)

val gen_build_files : string -> string -> unit
(**
``gen_build_files dir gist`` generates a jbuild file in ``dir``.
*)

val build_exec : string -> unit
(**
``build_exec dir `` runs the jbuilder system command to build the JavaScript.
*)

val postprocess : string -> string -> unit
(**
``postprocess dir name`` renames the generated JavaScript file.
*)

end
