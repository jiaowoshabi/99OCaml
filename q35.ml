let factors n =
  let rec aux n1 curr acc = match n1 = curr with
    | true -> List.rev (n1::acc)
    | false -> match (mod) n1 curr with
               | 0 -> aux (n1/curr) curr (curr::acc)
               | _ -> aux n1 (curr+1) acc
  in aux n 2 []

let test0 = factors 315 = [3;3;5;7]      
