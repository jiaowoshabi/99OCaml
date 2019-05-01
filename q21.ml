let insert_at str count lst =
  let rec helper str count lst acc =
    match count, lst with
    | _, [] -> List.rev (str::acc)
    | 0, f::r -> (List.rev acc)@[str]@(f::r)
    | a, f::r -> helper str (a-1) r (f::acc)
  in helper str count lst [];;
   
let test0 = insert_at "alfa" 1 ["a";"b";"c";"d"] = ["a"; "alfa"; "b"; "c"; "d"];;
let test1 = insert_at "alfa" 3 ["a";"b";"c";"d"] = ["a"; "b"; "c"; "alfa"; "d"];;
let test2 = insert_at "alfa" 4 ["a";"b";"c";"d"] = ["a"; "b"; "c"; "d"; "alfa"];;
