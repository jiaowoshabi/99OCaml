let duplicate lst =
  let rec helper acc =function
    | [] -> acc
    | f::r -> [f;f] @ (helper acc r)
  in helper [] lst;;


let test0 = duplicate ["a";"b";"c";"c";"d"] = ["a"; "a"; "b"; "b"; "c"; "c"; "c"; "c"; "d"; "d"];;
