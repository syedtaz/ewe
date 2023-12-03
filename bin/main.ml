(* let () =
  let root = Ewe.Dom.root () in
  let child = Ewe.Dom.create "x" "y" Ewe.Dom.empty in
  let next = Ewe.Dom.add child root in
  let res = Ewe.Dom.foldl (fun acc x -> acc ^ x) next "" in
  Format.print_string res *)

let () =
  let open Ewe.Dom in
  let root = root () in
  let node = create "node" (String "value") [] in
  let next = add node root in match next.value with
    | String x -> Format.print_string x
    | Int x -> Format.print_int x
in
  ()