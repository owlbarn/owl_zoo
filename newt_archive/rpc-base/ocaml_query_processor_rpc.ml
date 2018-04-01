open Arg
open Thrift
open Newt_types


let host = "127.0.0.1";;
let port = 9527;;

exception Die;;
let sod = function
    Some v -> v
  | None -> raise Die;;

type connection = {
  trans : Transport.t ;
  proto : Thrift.Protocol.t;
  pred  : Predict.client;
}

let connect ~host port =
  let tx = new TSocket.t host port in
  let proto = new TBinaryProtocol.t tx in
  let pred = new Predict.client proto proto in
    tx#opn;
    { trans = tx ; proto = proto; pred = pred }
;;

let run_client () = 
  let cli = connect ~host port in
  try
    (let sum = cli.pred#add (Int32.of_int 12) in
       Printf.printf "%d + 1 = %ld\n" 12 sum ;
       flush stdout);
  with Transport.E (_,what) ->
    Printf.printf "ERROR: %s\n" what ; flush stdout
;;

run_client ();;
