#use "q32.ml"

let coprime a b = not ((gcd a b) > 1)

let test0 = coprime 13 27 = true;;
let test1 = not (coprime 20536 7826) = true;;
