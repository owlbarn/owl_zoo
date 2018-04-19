
val collect_source_files : string -> string
(**
``collect_source_files gist`` parses all zoo gist dependencies of given gist,
collect all the files, put into a temporary directory, and return the location
of this directory.
*)

module Make
  (B : Backend_Sig)
  = struct

  val build : string -> string -> unit
  (**
  ``build gist name`` creates given ``gist`` script into suitable backends: container, JavaScript file, etc. that of given ``name``.
  Currently, uploading to remote server is not included in this part.
  *)

end
