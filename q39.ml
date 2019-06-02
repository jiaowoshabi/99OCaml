#use "q31.ml"

let all_primes st ed = 
	let rec aux curr ed acc = match (curr <= ed) with
	| false -> acc
	| true -> match is_prime curr with
				| true -> aux (curr+1) ed (curr::acc)
				| false -> aux (curr+1) ed acc
in aux st ed []

let test0 = List.length (all_primes 2 7920) = 1000;;