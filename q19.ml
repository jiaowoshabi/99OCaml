let rotate lst count =
  let rec helper lst count acc =
    match count, lst with
    | _, [] -> List.rev acc
    | 0, hd::tl -> (hd::tl) @ (List.rev acc)
    | a, hd::tl -> helper tl (a-1) (hd::acc)
  in helper lst (((count mod (List.length lst)) + (List.length lst)) mod (List.length lst)) []
   
let test0 = rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 3 = ["d"; "e"; "f"; "g"; "h"; "a"; "b"; "c"];;
let test1 = rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] (-2) = ["g"; "h"; "a"; "b"; "c"; "d"; "e"; "f"];;

                                                     
