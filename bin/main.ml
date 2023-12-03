let () =
  let root = Ewe.Dom.root () in
  let child = Ewe.Dom.create "x" "y" Ewe.Dom.empty in
  let next = Ewe.Dom.add root child in
  let res = Ewe.Dom.foldl (fun acc x -> acc ^ x) next "" in
  Format.print_string res
