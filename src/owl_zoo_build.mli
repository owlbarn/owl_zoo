open Owl_zoo_backend

val collect_source_files : string -> string
(**
``collect_source_files gist`` parses all zoo gist dependencies of given gist,
collect all the files, put into a temporary directory, and return the location
of this directory.
*)


type backend =
  | CREST of CREST.backend_typ
  | JS    of JS.backend_typ
(** Types of backend. *)


val preprocess : backend -> string -> unit
(**
``preprocess dir``removes the zoo header from ".ml" files in a ``dir``.
*)

val gen_build_files : backend -> string -> string -> unit
(**
``gen_build_files dir gist``
*)

val build_exec : backend -> string -> unit
(**
``build_exec dir ``
*)

val postprocess : backend -> string -> unit
(**
``postprocess dir name``
*)

val build : backend -> string -> unit
(**
``build backend gist`` creates given ``gist`` script into suitable backends:container, JavaScript file, etc. that of given name of backend.
Currently, uploading to remote server is not included in this part.
*)
