(*
 * OWL - an OCaml numerical library for scientific computing
 * Copyright (c) 2016-2018 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

val add_dir_zoo : unit -> unit
(** Add directive "zoo" to OCaml toploop. *)

val download_gist : string -> string -> unit
(** Download gist of gid/vid to the Zoo directory *)