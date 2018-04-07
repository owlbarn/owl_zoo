let save_file file string =
  let channel = open_out file in
  output_string channel string;
  close_out channel

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
  let rec loop count =
    if count < 0 then Owl_log.error "mk_temp_dir: too many failing attemps" else
    let dir = Printf.sprintf "%s/%s%s" dir pat (rand_digits ()) in
    try Ok (Unix.mkdir dir mode; dir) with
    | Unix.Unix_error (Unix.EEXIST, _, _) -> loop (count - 1)
    | Unix.Unix_error (Unix.EINTR, _, _)  -> loop count
    | Unix.Unix_error (e, _, _)           -> Owl_log.error (Unix.error_message e)
  in
  match loop 100 with
  | Ok dir as r  -> r
  | Error _ as e -> e
