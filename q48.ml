type bool_expr =
	| Var of string
	| Not of bool_expr
	| And of bool_expr * bool_expr
	| Or of bool_expr * bool_expr;;

let rec aux lst exp = match exp with
| Var x -> List.assoc x lst 
| Not e1 -> not (aux lst e1)
| And (e1, e2) -> (aux lst e1) && (aux lst e2)
| Or (e1, e2) -> (aux lst e1) || (aux lst e2)

let rec make_table lst exp acc = match lst with
| [] -> [List.rev acc, aux acc exp]
| f::r -> make_table r exp ((f, true)::acc) @
            make_table r exp ((f, false)::acc)

let table lst exp = make_table lst exp []

let test0 = table ["a"; "b"] (And(Var "a", Or(Var "a", Var "b"))) = [([("a", true); ("b", true)], true); ([("a", true); ("b", false)], true);
 ([("a", false); ("b", true)], false); ([("a", false); ("b", false)], false)];;

let test1 = let a = Var "a" and b = Var "b" and c = Var "c" in
table ["a"; "b"; "c"] (Or(And(a, Or(b,c)), Or(And(a,b), And(a,c)))) = [([("a", true); ("b", true); ("c", true)], true);
 ([("a", true); ("b", true); ("c", false)], true);
 ([("a", true); ("b", false); ("c", true)], true);
 ([("a", true); ("b", false); ("c", false)], false);
 ([("a", false); ("b", true); ("c", true)], false);
 ([("a", false); ("b", true); ("c", false)], false);
 ([("a", false); ("b", false); ("c", true)], false);
 ([("a", false); ("b", false); ("c", false)], false)];;