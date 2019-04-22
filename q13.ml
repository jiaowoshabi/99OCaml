type 'a rle =
    | One of 'a
    | Many of int * 'a;;

let encode lst = match lst with
  | [] -> []
  | _ -> let rec helper acc count curr = function
           | [] -> (Many (count, curr))::acc
           | f::r -> match f = curr with
                     | true -> helper acc (count+1) curr r
                     | false -> match count with
                                | 1 -> helper ((One curr)::acc) 1 f r
                                | a -> helper ((Many (count, curr))::acc) 1 f r
         in List.rev (helper [] 1 (List.hd lst) (List.tl lst));;

(*test case*)
let test0 = encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"] =
[Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d";
 Many (4, "e")];;
