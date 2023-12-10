module Grid = struct
  type sunit = Px of int
  (* | Fr of int *)

  type t =
    (* | RowDef of sunit list *)
    | ColDef of sunit list
    (* | RowIdx of int *)
    | ColIdx of int * int

  let px x = Px x
  let coldef x = ColDef x
  let colidx x = ColIdx (x, x)
  let colidx2 y x = ColIdx (y, x)
end

include Grid
