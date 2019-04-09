let compress lst =
  let rec helper acc = function
    | [] -> acc
    | f::r -> match ((=) f (List.hd acc)) with
              | true -> helper acc r
              | false -> helper (f::acc) r
                 in List.rev (helper [List.hd lst] (List.tl lst))

let test = compress ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"] = ["a"; "b"; "c"; "a"; "d"; "e"];;                     
