val preprocess : string -> unit
(**
``preprocess dir``removes the zoo header from ".ml" files in a ``dir``.
*)

val gen_build_files : string -> string -> unit
(**
``gen_build_files dir gist`` generates a cohttp web server file, a jbuild file, and a Dockerfile in ``dir``.
*)

val build_exec : string -> unit
(**
``build_exec dir `` runs the jbuilder system command to build executable of the web server  that serves RESTful API.
*)

val postprocess : string -> string -> unit
(**
``postprocess dir name`` runs the docker system command to build a docker container that runs the web server executable.
*)
