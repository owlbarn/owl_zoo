open Yojson
open Owl_zoo_utils

let decode t =
  match t with
  | "int"     -> fun x -> "int_of_string " ^ x.(0)
  | "float"   -> fun x -> "float_of_string" ^ x.(0)
  | "string"  -> fun x -> x.(0) ^ " in"
  | "ndarray" -> fun x -> Printf.sprintf "decode_base64_string %s" x.(0)
  | "img"     -> fun x -> Printf.sprintf
    "decode_base64 %s %s;\nlet %s = img_of_string %s \"\""
    x.(0) x.(1) x.(0) x.(0)
  | "text"    -> fun x -> x.(0) ^ " |> text_of_string"
  | _         -> failwith "decode: unsupported type"

let encode t =
  match t with
  | "int"     -> fun x -> "string_of_int " ^ x
  | "float"   -> fun x -> "string_of_float " ^ x
  | "string"  -> fun x -> x
  | "ndarray" -> fun x -> Printf.sprintf "%s |> save_file_byte |> encode_base64" x
  | "text"    -> fun x -> x ^ " |> string_of_text"
  | _         -> failwith "encode: unsupported type"


let gen_single_param decoded n =
  Printf.sprintf "let t%d, v%d = params.(%d) in\n" n n n ^
  Printf.sprintf "let v%d = %s in\n" n decoded, 1

let gen_two_params decoded n =
  Printf.sprintf "let t%d, v%d = params.(%d) in\n" n n n ^
  Printf.sprintf "let t%d, v%d = params.(%d) in\n" (n + 1) (n + 1) (n + 1)  ^
  Printf.sprintf "%s in\n"

let gen_lst_params typ n = ()


let gen_str typ n =
  if (typ <> "img" && typ <> "voice") then (
    let decoded = decode typ [|"v" ^ (string_of_int n)|] in
    gen_single_param decoded n
  ) else if () (
    let decoded = decode typ [|"v" ^ (string_of_int n);
      "v" ^ (string_of_int (n+1))|] in
    gen_two_params decoded n
  ) else {
    let sub_typ = get_sub_type typ in
    gen_lst_params sub_typ n
  }

let generate_server config_file =

  let fun_lst = Yojson.Basic.from_file config_file
      |> Yojson.Basic.Util.to_assoc
      |> List.map filter_str
  in

  let branch_str = ref "" in
  List.iteri (fun i (n, t) ->
    let c = ref 0 in
    let func_str = ref "" in
    let vars = ref "" in
    let th, tl = divide_lst t in

    List.iter (fun typ ->
      vars := !vars ^ (Printf.sprintf " v%d" !c);
      let p_str, pc = gen_str typ !c in
      c := !c + pc;
      func_str := !func_str ^ p_str
    ) th;

    let header = "| \"/predict/" ^ (get_funame n) ^ "\" -> \n" ^
      "let params = param_str uri " ^ (string_of_int !c) ^ " in\n" in
    let footer =  "let result = " ^ n ^ !vars ^
      " in\nlet result = " ^ (encode tl "result") ^
      " in\nServer.respond_string ~status:`OK ~body:(result ^ \"\") ()\n\n"
    in
    branch_str := !branch_str ^ header ^ !func_str ^ footer;
  ) fun_lst;

  let output_string =
"open Lwt
open Cohttp
open Cohttp_lwt_unix
open Owl_zoo_types

let port = 9527

let save_file file string =
  let channel = open_out file in
  output_string channel string;
  close_out channel

let save_file_byte data =
  let tmp = Filename.temp_file \"temp\" \"byte\" in
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
  let cmd = \"openssl base64 -in \" ^ filename in
  syscall cmd

let decode_base64 filename bytestr =
  let tmp_byte = Filename.temp_file \"tempbyte\" \".b64\" in
  save_file tmp_byte bytestr;
  let cmd = \"openssl base64 -d -in \" ^ tmp_byte ^ \" -out \" ^ filename in
  syscall cmd |> ignore

let decode_base64_string bytestr =
  let tmp = Filename.temp_file \"temp\" \".byte\" in
  decode_base64 tmp bytestr |> ignore;
  Owl_utils.marshal_from_file tmp

let param_str uri n =
  let params = Array.make n (\"\", \"\") in
  Array.iteri (fun i t ->
    let p = Uri.get_query_param uri (\"input\" ^ (string_of_int (i + 1))) in
    let p = match p with
      | Some x -> x
      | None   -> failwith \"param_str: invalid input\"
    in
    params.(i) <- (t, p)
  ) (Array.make n \"\");
  params

let callback _conn req body =
  let uri = Request.uri req in
  match Uri.path uri with
"
^ !branch_str ^
"| _ ->
    Server.respond_string ~status:`Not_found ~body:\"Route not found\" ()

let server =
  Server.create ~mode:(`TCP (`Port port)) (Server.make ~callback ())

let () = ignore (Lwt_main.run server)
"

  in
  save_file "server.ml" output_string



let generate_dockerfile gist  =
  let output_str = "
  #FROM ryanrhymes/owl
  FROM matrixanger/zoo-base
  MAINTAINER John Smith

  RUN opam install -y lwt cohttp cohttp-lwt-unix yojson jbuilder

  RUN apt-get update -y \\
      && apt-get -y install wget imagemagick

  RUN mkdir /service
  WORKDIR /service

  COPY server.ml jbuild /service/

  ENV GIST " ^ gist ^ "
  RUN owl -run " ^ gist ^ " \\
      && find ~/.owl/zoo -iname '*' -exec cp \\{\\} . \\; \\
      && find . -name \"*.ml\" -exec sed -i '/^#/d' \\{\\} \\;

  RUN eval `opam config env` && jbuilder build server.bc

  ENTRYPOINT [\"./_build/default/server.bc\"]
  "
  in
  save_file "Dockerfile" output_str

let generate_jbuild dir =
  let output_str = "
  (jbuild_version 1)

  (executable
   ((name server)
    (libraries (owl owl_zoo lwt cohttp.lwt cohttp-lwt-unix))))
  "
  in
  save_file (dir ^ "jbuild") output_str


let preprocess dir =
  let cmd = Printf.sprintf "find %s -name \"*.ml\" -exec sed -i '/^#/d' {}" dir in
  Sys.command(cmd) |> ignore

let gen_build_file dir gist =
  generate_server "service.json";
  generate_dockerfile gist;
  generate_jbuild dir

let build_exec dir =
  Sys.command("cd dir && jbuilder build server.bc")

let wrap dir name =
  Sys.command("docker build -t " ^ name );
  name
