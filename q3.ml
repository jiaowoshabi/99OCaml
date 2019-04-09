let rec at idx lst = match (idx, lst) with
  | (_, []) -> None
  | (1, f::r) -> Some f
  | (a, f::r) -> at (a-1) r

let test0 = at 3 [ "a" ; "b"; "c"; "d"; "e" ] = Some "c";;
let test1 = at 3 ["a"] = None;;
