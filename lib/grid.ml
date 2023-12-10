module Grid = struct
  type sunit =
    | Px of int
    | Fr of int

  type t =
    (* | RowDef of sunit list *)
    | ColDef of sunit list
    (* | RowIdx of int *)
    | ColIdx of int * int
end

include Grid
