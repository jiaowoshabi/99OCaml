let rotate lst count =
  let rec helper lst count acc =
    match (compare count 0), lst with
    | _, [] -> List.rev acc
    | 0, hd::tl -> (hd::tl) @ (List.rev acc)
    | -1, hd::tl -> helper (hd::tl) ((List.length (hd::tl))+count) []
    | 1, hd::tl -> helper tl (count-1) (hd::acc)
  in helper lst count []
   
let test0 = rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 3 = ["d"; "e"; "f"; "g"; "h"; "a"; "b"; "c"];;
let test1 = rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] (-2) = ["g"; "h"; "a"; "b"; "c"; "d"; "e"; "f"];;

                                                     
