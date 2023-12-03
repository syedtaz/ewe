module Node = struct
  module M = Map.Make (String)

  type t = { key : string; value : string; children : t M.t }

  let create k v c = { key = k; value = v; children = c }
  let empty = M.empty

  let add parent node =
    match parent with
    | { key = k; value = v; children = c } ->
        { key = k; value = v; children = M.add node.key node c }
end

module Dom = struct
  open Node

  type t = Node.t

  let root () = { key = "root"; value = "root"; children = M.empty }


end

include Dom
include Node
