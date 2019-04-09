let last lst = match (List.rev lst) with
		| [] -> None
		| f::r -> Some f;;

let test0 = last ["a"; "b"; "c"; "d"] = Some "d";;

let test1 = last [] = None;;

1;;
