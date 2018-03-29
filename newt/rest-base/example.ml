(* example 1 *)

let ss = make_services "aa36ee2c93fad476f4a46dc195b6fd89";;
let s1 = ss.(0);; (*img -> ndarray *)
let s2 = ss.(1);; (*ndarray -> text *)
let s3 = seq s1 s2 0;; (* compose *)
publish_s s3 "foo" "matrixanger/foo";; (* df8a0359cb9dd6528643b456ff7d7f3e *)
deploy "matrixanger/foo" "10.20.30.40" "stark";;

(* example 2 *)

let ss1 = make_services "df8a0359cb9dd6528643b456ff7d7f3e";; (* img -> text *)
let ss2 = make_services "7f32af9c1691fbfcf4f4340bd3780ee8";; (* text -> int *)
let s4 = seq ss1.(0) ss2.(0) 0;; (* compose *)
publish_s s4 "bar" "matrixanger/bar";;

(* bad example *)

let s5 = seq s1 ss2.(0) 0;; (* leads to runtime failure *)