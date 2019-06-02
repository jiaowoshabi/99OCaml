#use "q31.ml"

let goldbach n = 
	let rec aux a n = 
		match (is_prime a) &&  (is_prime (n-a))with
		| true -> a, n-a
		| false -> aux (a+1) n
	in aux 2 n

let test0 = goldbach 28 = (5, 23);;