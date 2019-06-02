#use "q36.ml"
(* Naive power function. *)
let rec pow n p = if p < 1 then 1 else n * pow n (p-1);;

let phi_improved n = 
	let rec aux acc = function
	| [] -> acc
	| (t1, t2)::r -> aux (acc * (t1-1) * (pow t1 (t2-1))) r
in aux 1 (factors1 n)

let test0 = phi_improved 10 = 4;;
let test1 = phi_improved 13 = 12;;