(*
 * OWL - an OCaml numerical library for scientific computing
 * Copyright (c) 2016-2017 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)


type t = {
  mutable gists : string array;
  mutable types : string array;
  mutable graph : (string * string * int) Owl_graph.node;
}

val make_services : string -> (string, t) Hashtbl.t

val connect_service : ?name:string -> t list -> t -> t list

val save_service : t -> string -> string


val list_nodes : t -> string

val get_service_info : string -> unit


val ( $> ) : ?name:string -> t list -> t -> t list
val ( $  ) : string -> (string, t) Hashtbl.t
val ( $~ ) : (string, t) Hashtbl.t -> string -> t
