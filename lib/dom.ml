module Node = struct
  module M = Map.Make (String)

  type node =
  { key : string
  ; value : element
  ; children : node list
  }
  and element =
    | Int of int
    | String of string

  let create key value children = { key = key; value = value; children = children }
  let add child { key; value; children} = { key = key; value = value; children = child :: children}
end

module Dom = struct
  open Node

  let root () =  create "root" (String "rootval") []
end

include Dom
include Node
