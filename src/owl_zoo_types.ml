type png
type jpeg
type ppm

type en
type fr

type _ img =
  | PNG : string -> png  img
  | JPG : string -> jpeg img
  | PPM : string -> ppm  img

type _ text =
  | ENT : string -> en text
  | FRT : string -> fr text

type _ voice =
  | ENV : string -> en voice
  | FRV : string -> fr voice

let string_of_img (type el) (x:el img) =
  match x with
  | PNG a -> a
  | JPG a -> a
  | PPM a -> a

let string_of_text (type el) (x:el text) =
  match x with
  | ENT a -> a
  | FRT a -> a

let string_of_voice (type el) (x:el voice) =
  match x with
  | ENV a -> a
  | FRV a -> a

type z =
  | Z_string of string
  | Z_float of float
  | Z_int of int
  | Z_bytes of bytes
  | Z_bool of bool
  | Z_ndarray_s of Owl.Dense.Ndarray.S.arr
  | Z_ndarray_d of Owl.Dense.Ndarray.D.arr
  | Z_png_img  of png img
  | Z_jpg_img of jpeg img
  | Z_ppm_img of ppm img
  | Z_en_text of en text
  | Z_fr_text of fr text
  | Z_en_voice of en voice
  | Z_fr_voice of fr voice
  | Z_list of z list
  | Z_array of z array


type backend = CONTAINER_REST | CONTAINER_RPC | JS
