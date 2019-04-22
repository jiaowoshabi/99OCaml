type 'a rle =
    | One of 'a
    | Many of int * 'a;;

let rec decode lst = match lst with
  | [] -> []
  | f::r -> let rec create_lst = function
              | Many (count, char) -> let rec dd acc cnt ch = match cnt with
                                        | 0 -> acc
                                        | a -> dd (ch::acc) (a-1) ch
                                             in dd [] count char
              | One char -> [char]
  in (create_lst f) @ (decode r);;


let test0 = decode [Many (4,"a"); One "b"; Many (2,"c"); Many (2,"a"); One "d"; Many (4,"e")] = ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;
