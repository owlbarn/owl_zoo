module type Sig = sig

  (** preprocess dir *)
  val preprocess : string -> unit

  (** gen_build_file dir *)
  val gen_build_file : string -> unit

  (** build_exec dir *)
  val build_exec : string -> unit

  (** wrap dir name -> a string of id *)
  val wrap : string -> string -> string

end
