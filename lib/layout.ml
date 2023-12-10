module Layout = struct
  (* open Vdom *)
  open Core
  open Async

  let stdout = force Writer.stdout

  type cursor = int * int

  type position =
    { start : cursor
    ; fin : cursor
    }

  let write cursor payload =
    let out = Termutils.move_cursor cursor ^ payload in
    Writer.write stdout out
  ;;

  let size node = Vdom.repr node |> String.length
  let rows, cols = Termutils.tsize ()

  (* let rec position (crow, ccol) (ncols, colsize) = function
      | Element { grid = g; children = c; _} -> match g with
        | ColDef defs ->
        | ColIdx -> *)
end

include Layout
