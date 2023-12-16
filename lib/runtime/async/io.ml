module IO = struct
  open Core
  open Async

  let stdout = force Writer.stdout
  let stdin = force Reader.stdin
  let stderr = force Writer.stderr
  let write_out s = Writer.write stdout s
end

include IO
