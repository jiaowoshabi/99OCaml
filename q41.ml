#use "q40.ml"

let goldbach_list s e = 
	let rec aux s e acc = 
		if (s mod 2) = 0 then
			match (s <= e) with
			| false -> List.rev acc
			| true -> aux (s+1) e ((s, (goldbach s))::acc)
		else aux (s+1) e acc
	in aux s e []

let test0 = goldbach_list 9 20 = [(10, (3, 7)); (12, (5, 7)); (14, (3, 11)); (16, (3, 13)); (18, (5, 13));(20, (3, 17))]

let goldbach_limit s e limit = 
	let rec aux limit acc = function
		| [] -> List.rev acc
		| (h1, (h2, h3))::r -> match (h2 >= limit) with
								| true -> aux limit ((h1, (h2, h3))::acc) r
								| false -> aux limit acc r
	in aux limit [] (goldbach_list s e)

let test1 = goldbach_limit 1 2000 50 = [(992, (73, 919)); (1382, (61, 1321)); (1856, (67, 1789)); (1928, (61, 1867))]