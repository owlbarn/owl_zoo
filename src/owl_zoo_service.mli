
type t = {
  mutable gists : string array;
  mutable types : string array;
  mutable graph : (string * string * int) Owl_graph.node;
}

val make_services : string -> t Owl_zoo_utils.Dict.dict

val connect_service : ?name:string -> t list -> t -> t list

val save_service : t -> string -> string


val list_nodes : t -> string

val get_service_info : string -> unit
