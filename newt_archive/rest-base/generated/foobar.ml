#!/usr/bin/env owl
#zoo "aa36ee2c93fad476f4a46dc195b6fd89"
let main p0 =
  let r0 = Squeezenet.infer p0 in
  let r1 = Squeezenet.to_json r0  in
  r1
