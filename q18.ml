let slice lst start stop =
  let rec helper lst start stop acc =
    match start, stop, lst with
    | s, 0, hd::tl -> List.rev (hd::acc)
    | 0, e, hd::tl -> helper tl 0 (e-1) (hd::acc)
    | s, e, hd::tl -> helper tl (s-1) (e-1) acc
    | s, e, [] -> List.rev acc
  in helper lst start stop [];;

let test0 = slice ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 2 6 = ["c"; "d"; "e"; "f"; "g"];;
