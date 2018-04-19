open Yojson
open Owl_zoo_utils

let decode t =
  let img_fun typ x = Printf.sprintf
    "decode_base64 %s %s;\nlet %s = %s %s \"\""
    x.(0) x.(1) x.(0) typ x.(0)
  in
  let text_fun typ x = typ ^ " " ^ x.(0) in
  match t with
  | "int"     -> fun x -> "int_of_string " ^ x.(0)
  | "float"   -> fun x -> "float_of_string " ^ x.(0)
  | "string"  -> fun x -> x.(0)
  | "bytes"   -> fun x -> "Bytes.of_string " ^ x.(0)
  | "bool"    -> fun x -> "bool_of_string " ^ x.(0)
  | "ndarray" -> fun x -> Printf.sprintf "decode_base64_string %s" x.(0)
  | "png_img" -> img_fun "PNG"
  | "jpeg_img"-> img_fun "JPG"
  | "ppm_img" -> img_fun "PPM"
  | "en_text" -> text_fun "ENT"
  | "fr_text" -> text_fun "FRT"
  | _         -> failwith "unsupported type"

let encode t =
  let img_fun  x = Printf.sprintf "%s |> string_of_img |> encode_base64" x in
  let text_fun x = x ^ " |> string_of_text" in
  match t with
  | "int"     -> fun x -> "string_of_int " ^ x
  | "float"   -> fun x -> "string_of_float " ^ x
  | "string"  -> fun x -> x
  | "bytes"   -> fun x -> "Bytes.to_string " ^ x
  | "bool"    -> fun x -> "string_of_bool " ^ x
  | "ndarray" -> fun x -> Printf.sprintf "%s |> save_file_byte |> encode_base64" x
  | "png_img" -> img_fun
  | "jpeg_img"-> img_fun
  | "ppm_img" -> img_fun
  | "en_text" -> text_fun
  | "fr_text" -> text_fun
  | _         -> failwith "unsupported type"


let gen_single_param decoded n =
  Printf.sprintf "let t%d, v%d = params.(%d) in\n" n n n ^
  Printf.sprintf "let v%d = %s in\n" n decoded, 1

let gen_two_params decoded n =
  Printf.sprintf "let t%d, v%d = params.(%d) in\n" n n n ^
  Printf.sprintf "let t%d, v%d = params.(%d) in\n" (n + 1) (n + 1) (n + 1)  ^
  Printf.sprintf "%s in\n" decoded, 2

(* TODO *)
let gen_lst_params typ n = "", 0

let gen_str typ n =
  if (typ <> "png_img" && typ <> "en_voice" && typ <> "list") then (
    let decoded = decode typ [|"v" ^ (string_of_int n)|] in
    gen_single_param decoded n
  ) else if (typ <> "list") then (
    let decoded = decode typ [|"v" ^ (string_of_int n);
      "v" ^ (string_of_int (n+1))|] in
    gen_two_params decoded n
  ) else (
    (* let sub_typ = get_sub_type typ in *)
    gen_lst_params typ n
  )

let generate_server dir =

  let config_file = dir ^ "/service.json" in
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
open Owl_newt
open Owl_newt.Owl_zoo_utils
open Owl_newt.Owl_zoo_types

let port = 9527

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
  save_file (dir ^ "/server.ml") output_string


let generate_dockerfile dir gist  =
  let output_str = "
  #FROM ryanrhymes/owl
  FROM matrixanger/zoo-base
  MAINTAINER John Smith

  RUN opam install -y lwt cohttp cohttp-lwt-unix yojson jbuilder

  RUN apt-get update -y \\
      && apt-get -y install wget imagemagick

  RUN mkdir /service
  WORKDIR /service

  COPY * /service/
  ENTRYPOINT [\"./_build/default/server.bc\"]
  "
  in
  save_file (dir ^ "/Dockerfile") output_str

let generate_jbuild dir =
  let output_str = "
  (jbuild_version 1)

  (executable
   ((name server)
    (libraries (owl owl_newt lwt cohttp.lwt cohttp-lwt-unix))))
  "
  in
  save_file (dir ^ "/jbuild") output_str

let preprocess dir =
  let cmd = Printf.sprintf "find %s -name \"*.ml\" -exec sed -i '/^#/d' {} \\; " dir in
  Sys.command(cmd) |> ignore

let gen_build_files dir gist =
  generate_server dir;
  generate_jbuild dir;
  generate_dockerfile dir gist

let build_exec dir =
  let cmd = Printf.sprintf "(cd %s; jbuilder build server.bc)" dir in
  Sys.command cmd |> ignore

let postprocess dir name =
  let cmd = Printf.sprintf "(cd %s; docker build -t %s)" dir name in
  Sys.command cmd |> ignore
