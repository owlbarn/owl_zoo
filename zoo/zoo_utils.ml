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

(* TODO: improve it *)
let mk_temp_dir () =
  let rand_num = Random.int 1000000 |> string_of_int in
  let tmp_dir = Filename.get_temp_dir_name () ^ "/" ^ rand_num in
  try
    Unix.mkdir tmp_dir 0o600;
    tmp_dir
  with _ -> raise (Sys_error "Cannot create temp dir")
