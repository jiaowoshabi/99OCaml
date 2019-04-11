let encode lst = match lst with
  | [] -> []
  | _ ->
     let rec helper acc count curr = function
       | [] -> (count, curr)::acc
       | f::r -> match f = curr with
                 | true -> helper acc (count+1) curr r
                 | false -> helper ((count, curr)::acc) 1 f r
     in List.rev (helper [] 1 (List.hd lst) (List.tl lst));;

let test0 = encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"] = [(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")];;
