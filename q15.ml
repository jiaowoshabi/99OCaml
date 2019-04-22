let replicate lst count =
  let rec helper acc l cnt =
    let rec dd a i c = match c with
      | 0 -> a
      | v -> dd (i::a) i (v-1)
    in match l with
       | [] -> acc
       | f::r -> helper ((dd [] f cnt) @ acc) r cnt
  in List.rev (helper [] lst count);;

let test0 = replicate ["a";"b";"c"] 3 = ["a"; "a"; "a"; "b"; "b"; "b"; "c"; "c"; "c"];;
