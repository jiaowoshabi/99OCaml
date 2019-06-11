type bool_expr =
    | Var of string
    | Not of bool_expr
    | And of bool_expr * bool_expr
    | Or of bool_expr * bool_expr;;

let rec aux a a_val b b_val exp = match exp with
| Var x -> if x = a then a_val 
			else if x = b then b_val
			else failwith "The expression contains an invalid variable"
| And (e1, e2) -> (aux a a_val b b_val e1) && (aux a a_val b b_val e2)
| Or (e1, e2) -> (aux a a_val b b_val e1) || (aux a a_val b b_val e2)
| Not e1 -> not (aux a a_val b b_val e1)

let table2 a b exp = [
	(true, true, aux a true b true exp);
	(true, false, aux a true b false exp);
	(false, true, aux a false b true exp);
	(false, false, aux a false b false exp);
]

let test0 = table2 "a" "b" (And(Var "a", Or(Var "a", Var "b"))) = [(true, true, true); (true, false, true); (false, true, false);
 (false, false, false)];;