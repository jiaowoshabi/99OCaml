
let is_palindrome lst = (=) (List.rev lst) lst;;

let test0 = is_palindrome [ "x" ; "a" ; "m" ; "a" ; "x" ];;
let test1 = not (is_palindrome [ "a" ; "b" ]);;
