let remove_at idx lst =
  let rec helper idx lst acc =
    match idx, lst with
    | _, [] -> List.rev acc
    | 0, f::r -> (List.rev acc)@r
    | a, f::r -> helper (idx-1) r (f::acc)
  in helper idx lst []

let test0 = remove_at 1 ["a";"b";"c";"d"] = ["a"; "c"; "d"];;
