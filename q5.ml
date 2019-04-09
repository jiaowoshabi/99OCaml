let rev lst =
  let rec rev_helper acc lst = match lst with
    | [] -> acc
    | f::r -> rev_helper (f::acc) r
  in rev_helper [] lst;;

let test0 = rev ["a";"b";"c"] = ["c";"b";"a"];;
let test1 = rev [] = [];;
