let split lst count =
  let rec helper lst count acc =
    match count, lst with
    | _, [] -> acc, []
    | 1, hd::tl -> (hd::acc), tl
    | a, hd::tl -> helper tl (count-1) (hd::acc)
  in List.rev (fst (helper lst count [])), snd (helper lst count []);; 

split ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3;;

let test0 = split ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3 = (["a"; "b"; "c"], ["d"; "e"; "f"; "g"; "h"; "i"; "j"]);;

let test1 = split ["a";"b";"c";"d"] 5 = (["a"; "b"; "c"; "d"], []);;
