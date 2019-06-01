(* Euclid's algorithm*)
let gcd a b =
  let rec aux a b c = match c=0 with
    | true  -> b
    | false -> aux b c ((mod) b c)
  in if (a >= b) then aux a b  ((mod) a  b)
     else aux b a ((mod) b a)

let test0 = gcd 13 27 = 1;;
let test1 = gcd 20536 7826 = 2;;
