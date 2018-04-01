open Arg
open Thrift
open Newt_types

let add_lib = Splus.plus;;
let port = 9527;;

exception Die;;
let sod = function
    Some v -> v
  | None -> raise Die;;

class pred_handler = 
object (self)
  inherit Predict.iface

  method add a =
    (* preprocess each type (request to sth); 
     * call the function(sths to sth); 
     * then process output format (sth to request) *)
    Int32.of_int (add_lib (Int32.to_int (sod a)))
end
;;

let run () = 
  let h = new pred_handler in
  let proc = new Predict.processor h in
  let pf = new TBinaryProtocol.factory in
  let server = new TThreadedServer.t proc
        (new TServerSocket.t port)
        (new Transport.factory)
        pf pf
  in
  server#serve
;;

run ();;
