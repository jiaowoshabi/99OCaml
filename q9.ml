let pack lst = match lst with
  | [] -> []
  | _ ->
  let rec helper acc rep = function
    | [] -> rep::acc
    | f::r -> match (List.mem f rep) with
              | true -> helper acc (f::rep) r
              | false -> helper (rep::acc) [f] r
       in List.rev (helper [] [(List.hd lst)] (List.tl lst));;

let test0 = pack ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"d";"e";"e";"e";"e"] = [["a"; "a"; "a"; "a"]; ["b"]; ["c"; "c"]; ["a"; "a"]; ["d"; "d"];
 ["e"; "e"; "e"; "e"]];;
