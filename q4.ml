let length lst =
  let rec length_helper lst acc = match lst with
    | [] -> acc
    | f::r -> length_helper r (acc+1)
                 in length_helper lst 0;;

let test0 = length ["a"; "b"; "c"] = 3;;
let test1 = length [] = 0;;
