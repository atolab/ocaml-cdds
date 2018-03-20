open Ctypes
open Foreign


module PolicyId = struct
  include Int32
  let t = int32_t

  let invalid = of_int 0
  let user_data = of_int 1
  let durability = of_int 2
  let presentation = of_int 3
  let deadline = of_int 4
  let latency_budget = of_int 5
  let ownership = of_int 6
  let ownership_strenght = of_int 7
  let liveliness = of_int 8
  let time_based_filter = of_int 9
  let partition = of_int 10
  let reliability = of_int 11
  let destination_order = of_int 12
  let history = of_int 13
  let resource_limits = of_int 14
  let entity_factory = of_int 15
  let writer_lifecycle = of_int 16
  let reader_lifecycle = of_int 17
  let topic_data = of_int 18
  let group_data = of_int 19
  let transport_priority = of_int 20
  let lifespan = of_int 21
  let durability = of_int 22
end

module Qos : sig
  type t
  val t : t structure typ
  val none : t structure ptr
  val make :  unit -> t structure ptr
  val destroy : t structure ptr -> unit
end = struct
  type t
  let t : t structure typ = structure "dds_qos_t"
  let none = from_voidp t null

  let create_qos =
      foreign "dds_qos_create" (void @-> returning @@ ptr t)

  let delete_qos =
    foreign "dds_qos_delete" (ptr t @-> returning void)

  let make = create_qos

  let destroy = delete_qos
end

(*
 * QoS Operations
 *)


let qset_durability =
  foreign "dds_qset_durability" (ptr Qos.t @-> PolicyId.t @-> returning void)

let qset_history =
  foreign "dds_qset_history" (ptr Qos.t @-> PolicyId.t @-> int32_t @-> returning void)

let qset_reliability =
  foreign "dds_qset_reliability" (ptr Qos.t @-> PolicyId.t @-> Duration.t @-> returning void)

let qset_ownership =
  foreign "dds_qset_ownership" (ptr Qos.t @-> PolicyId.t  @-> returning void)

let qset_ownership_strength =
  foreign "dds_qset_ownership_strength" (ptr Qos.t @-> PolicyId.t @-> int32_t @-> returning void)

let qset_destination_order =
  foreign "dds_qset_destination_order" (ptr Qos.t @-> PolicyId.t @-> returning void)

let _qset_partition =
  foreign "dds_qset_partition" (ptr Qos.t @-> uint32_t @-> ptr string @-> returning void)

let qset_partition qos plist =
  let module Array = CArray in
  let len = Unsigned.UInt32.of_int @@ List.length plist in
  let parr  = Array.of_list string plist in
  ignore ( _qset_partition qos len ( Array.start parr))


module Durability = struct
  module Kind = struct
    include Int32
    let t = int32_t
    let volatile = of_int 0
    let transient_local = of_int 1
    let transient = of_int 2
    let persistent= of_int 3
  end
  type t = { pid : PolicyId.t; kind: Kind.t }
  let t =  int
  let volatile = {pid = PolicyId.durability; kind = Kind.volatile}
  let transient_local = {pid = PolicyId.durability; kind = Kind.transient_local}
  let transient = {pid = PolicyId.durability; kind = Kind.transient}
  let persistent = {pid = PolicyId.durability; kind = Kind.persistent}
end

module Reliability = struct
  module Kind = struct
    include Int32
    let t = int32_t
    let best_effort = of_int 0
    let reliable = of_int 1
  end
  type t = { pid : PolicyId.t; kind : Kind.t; blocking_time : Duration.t }
  let best_effort = { pid = PolicyId.reliability; kind = Kind.best_effort; blocking_time = Duration.zero }
  let reliable  = {pid = PolicyId.reliability; kind = Kind.reliable; blocking_time = Duration.infinity}
end


module History = struct
  module Kind = struct
    include Int32
    let t = int32_t
    let keep_last = of_int 0
    let keep_all = of_int 1
  end
  type t = {pid: PolicyId.t; kind: Kind.t; depth : int}

  let keep_all = { pid = PolicyId.history; kind = Kind.keep_all; depth = -1 }
  let keep_last n = {pid = PolicyId.history; kind = Kind.keep_last; depth = n }
end


module Policy = struct
  type t =
      Durability of Durability.t
    | Reliability of Reliability.t
    | History of History.t
end

type policies = Policy.t list

let set_policies qos plist =
  List.iter
    (fun p -> match p with
       | Policy.History h -> ()
       | Policy.Reliability r -> ()
       | Policy.Durability d -> ())
    plist
