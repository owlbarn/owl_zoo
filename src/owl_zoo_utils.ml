module Dict = struct

  type key = string
  type 'a dict = (key * 'a) list

  let make () : 'a dict = []

  let insert (d : 'a dict) (k : key) (x : 'a) : 'a dict = (k, x) :: d

  exception NotFound

  let rec lookup (d : 'a dict) (k : key) : 'a =
  match d with
  | [] -> raise NotFound
  | (k', x) :: rest -> if k = k' then x else lookup rest k

  let map (f : 'a -> 'b) (d : 'a dict) =
    List.map (fun (k, a) -> (k, f a)) d

end

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

let save_file file string =
  let channel = open_out file in
  output_string channel string;
  close_out channel

let save_file_byte data =
  let tmp = Filename.temp_file "temp" "byte" in
  Owl_utils.marshal_to_file data tmp;
  tmp

let encode_base64 filename =
  let cmd = "openssl base64 -in " ^ filename in
  syscall cmd

let decode_base64 filename bytestr =
  let tmp_byte = Filename.temp_file "tempbyte" ".b64" in
  save_file tmp_byte bytestr;
  let cmd = "openssl base64 -d -in " ^ tmp_byte ^ " -out " ^ filename in
  syscall cmd |> ignore

let decode_base64_string bytestr =
  let tmp = Filename.temp_file "temp" ".byte" in
  decode_base64 tmp bytestr;
  Owl_utils.marshal_from_file tmp


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

(* split([1;2;3;4;5],3) --> [1;2;3], [4;5]*)
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

let rec remove_nth n = function
  | [] -> []
  | h :: t ->
    if n = 0 then t
    else h :: remove_nth (n-1) t

(* replace array a's idx-th elem with array b *)
let replace a b idx =
  assert (idx >= 0 && idx < (Array.length b));
  let x, y = a |> Array.to_list |> remove_nth idx |> split idx in
  (x @ (Array.to_list b) @ y) |> Array.of_list

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
