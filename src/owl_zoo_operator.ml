(*
 * OWL - an OCaml numerical library for scientific computing
 * Copyright (c) 2016-2017 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)


module Operator = struct

  let ( $> ) = Owl_zoo_service.connect_service

  let ( $ )  = Owl_zoo_service.make_services

  let ( $~ ) = Hashtbl.find

end
