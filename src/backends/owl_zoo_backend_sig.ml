module type Sig = sig

  (** preprocess dir *)
  val preprocess : string -> unit

  (** gen_build_files dir gist *)
  val gen_build_files : string -> string -> unit

  (** build_exec dir *)
  val build_exec : string -> unit

  (** wrap dir name -> a string of id *)
  val postprocess : string -> string -> string

end
