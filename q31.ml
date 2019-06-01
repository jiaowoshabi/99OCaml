let is_prime n =
  let rec aux n n1 upper = match n with
    | 1 -> false
    | 2 -> true
    | _ -> 
       match (n1 <= upper) with
       | true -> if ((mod) n n1 ) = 0 then false else aux n (n1+1)  upper
       | false -> true
  in aux n 2 (int_of_float (sqrt (float_of_int n)))

let test0 = not(is_prime 1) = true;;
let test1 = is_prime 7 = true;;
let test2 = not (is_prime 12);;
                               
