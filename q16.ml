let drop lst count =
  let rec helper lst count acc =
    match count, lst  with
    | 1, hd::tl -> helper tl 3 acc 
    | a, hd::tl -> helper tl (a-1) (hd::acc)
    | _, [] -> acc
  in List.rev (helper lst count []);;

let test0 = drop ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3 = ["a"; "b"; "d"; "e"; "g"; "h"; "j"];;                 
