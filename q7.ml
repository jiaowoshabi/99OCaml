type 'a node =
     | One of 'a
     | Many of 'a node list;;

let flatten lst =
  let rec helper acc = function  
    | [] -> acc
    | f::r -> match f with
      	      | One a -> helper (a::acc) r
	      | Many b -> helper (helper acc b) r
                        in List.rev(helper [] lst);;

let test0 = flatten [ One "a" ; Many [ One "b" ; Many [ One "c" ; One "d" ] ; One "e" ] ] = ["a"; "b"; "c"; "d"; "e"];;
