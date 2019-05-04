let range a b =
  let rec helper a b acc =
    match a<b with
    | true -> helper (a+1) b (a::acc)
    | false -> match a=b with
               | true -> List.rev (a::acc)
               | false -> helper (a-1) b (a::acc)
  in helper a b []

let test0 = range 4 9 = [4; 5; 6; 7; 8; 9];;
let test1 = range 9 4 = [9; 8; 7; 6; 5; 4];;
