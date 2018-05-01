(*
 * OWL - an OCaml numerical library for scientific computing
 * Copyright (c) 2016-2017 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

val syscall : string -> string
(** ``syscall cmd`` runs a system command and returns the execution result. *)

val save_file_byte : 'a -> string
(** ``save_file_byte data`` marshalls data to a temporary file and returns the location of this file. *)

val encode_base64 : string -> string
(** ``encode_base64 filename`` encodes a file into base64 code. *)

val decode_base64 : string -> string -> string
(** ``decode_base64 filename bytes`` decode base64-coded string into a temporary file of the same type as filename; returns the temp file location. *)

val decode_base64_string : string -> 'a
(** ``decode_base64_string bytes`` decode base64-coded string into a file of given name, and then marshalls from this file. *)

val strip_string : string -> string
(** ``strip_string str`` removes the whitespaces from ``str``.*)

val filter_str : 'a * Yojson.Basic.json -> 'a * string list
(** ``filter_str`` parses service configuration json object. *)

val uniq : 'a list -> 'a list
(** ``uniq lst`` finds all the unique elements in a list. *)

val split : int -> 'a list -> 'a list * 'a list
(** ``split lst idx`` splits a list into two according to a specific index. E.g., split [1;2;3;4;5] 3 --> [1;2;3], [4;5]. *)

val merge_array : 'a array -> 'a array -> 'a array
(** ``merge_array a b`` merges two arrays. *)

val join : ?delim:string -> string array -> string
(** ``join ~delim arr`` combines strings in array ``arr`` into one string using delimiter ``delim`` that is default to a whitespace. *)

val mk_temp_dir : ?mode:int -> ?dir:string -> string -> string
(** ``mk_temp_dir ~mode ~dir pat`` makes temporary directory "dir/patxxxxxx" and return its location. *)

val get_funame : string -> string
(** ``get_funname name`` *)

val divide_lst : 'a list -> 'a list * 'a
(** ``divide_lst lst`` returns the first n-1 elements and the last element of a list. *)
