(*
 * OWL - an OCaml numerical library for scientific computing
 * Copyright (c) 2016-2017 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)


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


let save_file_byte data =
  let tmp = Filename.temp_file "temp" ".byte" in
  Owl_io.marshal_to_file data tmp;
  tmp


let encode_base64 filename =
  let s = Owl_io.read_file_string filename in
  B64.encode s


let decode_base64 filename bytestr =
  let base = Filename.remove_extension filename in
  let ext = Filename.extension filename in
  let tmp = Filename.temp_file base ext in
  let s = B64.decode bytestr in
  Owl_io.write_file tmp s;
  tmp


let decode_base64_string bytestr =
  let tmp = decode_base64 "temp64.byte" bytestr in
  Owl_io.marshal_from_file tmp


let strip_string s =
  Str.global_replace (Str.regexp "[\r\n\t ]") "" s


let filter_str (x, y) = x,
  Yojson.Basic.Util.to_string y
  |> Str.split (Str.regexp "->")
  |> List.map strip_string


let uniq lst =
  let unique_set = Hashtbl.create (List.length lst) in
  List.iter (fun x -> Hashtbl.replace unique_set x ()) lst;
  Hashtbl.fold (fun x () xs -> x :: xs) unique_set []


let split n lst =
  let rec aux i acc = function
    | [] -> List.rev acc, []
    | h :: t as l ->
      if i = 0 then List.rev acc, l
      else aux (i-1) (h :: acc) t in
  aux n [] lst


let merge_array a b =
  Array.append a b |> Array.to_list
  |> uniq |> Array.of_list


let join ?(delim=" ") arr =
  String.concat delim (Array.to_list arr)


let rand_digits () =
  let rand = Random.State.(bits (make_self_init ()) land 0xFFFFFF) in
  Printf.sprintf "%06x" rand


let mk_temp_dir ?(mode=0o700) ?dir pat =
  let dir = match dir with
  | Some d -> d
  | None   -> Filename.get_temp_dir_name ()
  in
  let raise_err msg = raise (Sys_error ("mk_temp_dir: " ^ msg)) in
  let rec loop count =
    if count < 0 then raise_err "too many failing attemps" else
    let dir = Printf.sprintf "%s/%s%s" dir pat (rand_digits ()) in
    try (Unix.mkdir dir mode; dir) with
    | Unix.Unix_error (Unix.EEXIST, _, _) -> loop (count - 1)
    | Unix.Unix_error (Unix.EINTR, _, _)  -> loop count
    | Unix.Unix_error (e, _, _)           ->
      raise_err (Unix.error_message e)
  in
  loop 1000


let get_funame s =
  let lst = String.split_on_char '.' s in
  List.nth lst 1


let divide_lst lst =
  List.(lst |> rev |> tl |> rev),
  List.(lst |> rev |> hd)
