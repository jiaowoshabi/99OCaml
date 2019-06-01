#use "q33.ml";;

let phi n =
  let rec aux n incr acc = match (n >= incr)  with
    | false -> acc
    | true -> match (coprime n incr) with
              | true -> aux n (incr+1) (acc+1)
              | false -> aux n (incr+1) acc
  in aux n 1 0

let test0 = phi 10 = 4;;
let test1 = phi 13 = 12;;
                         
