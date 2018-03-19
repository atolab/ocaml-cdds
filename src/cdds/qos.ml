open Ctypes

module Durability = struct
  type t = int
  let t =  int
  let id = 2
  let volatile = 0
  let transient_local = 1
  let transient = 2
  let persistent = 3
end

module Reliability = struct
  type t = int
  let t = int
  let id = 11
  let best_effort = 0
  let reliable = 1
end


module History = struct
  type t = { kind: int; depth: int }
  let id = 13
  let keep_last_kind = 0
  let keep_all_kind = 1
  let keep_all = { kind = keep_all_kind; depth = 0; }
  let keep_last n = {kind = keep_last_kind; depth = n }
end
