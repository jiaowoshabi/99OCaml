let rec last_two lst = match lst with
  | a::[] -> None
  | a::b::[] -> Some (a,b)
  | f::r -> last_two r;;
  

let test0 = last_two [ "a" ; "b" ; "c" ; "d" ] = Some ("c", "d");;
let test1 = last_two [ "a" ] = None;;
