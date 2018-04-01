open Lwt
open Cohttp
open Cohttp_lwt_unix
open Owl_zoo_types

let port = 9527
let fn0 = Foobar.main

let save_file file string =
  let channel = open_out file in
  output_string channel string;
  close_out channel

let save_file_byte data =
  let tmp = Filename.temp_file "temp" "byte" in
  Owl_utils.marshal_to_file data tmp;
  tmp

let syscall cmd =
  let ic, oc = Unix.open_process cmd in
  let buf = Buffer.create 16 in
  (try
     while true do
       Buffer.add_channel buf ic 1
     done
   with End_of_file -> ());
  Unix.close_process (ic, oc) |> ignore;
  (Buffer.contents buf)

let encode_base64 filename =
  let cmd = "openssl base64 -in " ^ filename in
  syscall cmd

let decode_base64 filename bytestr =
  let tmp_byte = Filename.temp_file "tempbyte" ".b64" in
  save_file tmp_byte bytestr;
  let cmd = "openssl base64 -d -in " ^ tmp_byte ^ " -out " ^ filename in
  syscall cmd

let decode_base64_string bytestr = 
  let tmp = Filename.temp_file "temp" ".byte" in
  decode_base64 tmp bytestr |> ignore;
  Owl_utils.marshal_from_file tmp

let param_str uri n =
  let params = Array.make n ("", "") in
  Array.iteri (fun i t ->
    let p = Uri.get_query_param uri ("input" ^ (string_of_int (i + 1))) in
    let p = match p with
      | Some x -> x
      | None   -> failwith "invalid input"
    in
    params.(i) <- (t, p)
  ) (Array.make n "");
  params

let callback _conn req body =
  let uri = Request.uri req in
  match Uri.path uri with
| "/predict/main" -> 
let params = param_str uri 2 in
let t0, v0 = params.(0) in
let t1, v1 = params.(1) in
decode_base64 v0 v1 |> ignore;
let v0 = img_of_string v0 "" in
let result = Foobar.main v0 in
let result = result|> string_of_text in
Server.respond_string ~status:`OK ~body:(result ^ "") ()

| _ ->
    Server.respond_string ~status:`Not_found ~body:"Route not found" ()

let server =
  Server.create ~mode:(`TCP (`Port port)) (Server.make ~callback ())

let () = ignore (Lwt_main.run server)
