module Operator = struct

  let ( $> ) = Owl_zoo_service.connect_service

  let ( $ )  = Owl_zoo_service.make_services

  let ( $~ ) = Hashtbl.find

end