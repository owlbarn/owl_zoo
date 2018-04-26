open Owl_newt
open Owl_zoo_service
open Owl_zoo_utils

let ss1 = make_services "aa36ee2c93fad476f4a46dc195b6fd89";;
let s1  = ss1 $~ "Squeezenet.infer"
let s2  = ss1 $~ "Squeezenet.to_json"

let ss2 = make_services "7f32af9c1691fbfcf4f4340bd3780ee8";;
let s3  = ss2 $~ "Word_count.word_count"

let new_service = [s1] $> s2 $> s3

(* let gist = save_service new_service "foobar" *)
