#use "q35.ml";;

let encode lst = match lst with
  | [] -> []
  | _ ->
     let rec helper acc count curr = function
       | [] -> (curr, count)::acc
       | f::r -> match f = curr with
                 | true -> helper acc (count+1) curr r
                 | false -> helper ((curr, count)::acc) 1 f r
     in List.rev (helper [] 1 (List.hd lst) (List.tl lst));;

let factors1 n =
  encode (factors n)

let test0 = factors1 315 = [(3, 2); (5, 1); (7, 1)];;  
