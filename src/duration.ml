
type t = int64
let t = Ctypes.int64_t
let zero = Int64.zero
let infinity = Int64.max_int
let of_secs s = Int64.mul (Int64.of_int s) (Int64.of_int 1000000000)
let of_millisec ms = Int64.mul (Int64.of_int ms) (Int64.of_int 1000000)
let of_microsec us = Int64.mul (Int64.of_int us) (Int64.of_int 1000)
let of_nanosec ns = ns
