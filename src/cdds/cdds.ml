open Ctypes
open PosixTypes
open Foreign


module FreeOption = struct
  type t = int
  let t = int
  let free_key = 0x01
  let free_content = 0x03
  let free_all = 0x07
end


let entity_t            = int32_t
let time_t              = int64_t
let duration_t          = int64_t
let instance_handle_t   = int64_t
let domain_id_t         = int32_t
let sample_state_t      = int32_t
let view_state_t        = int32_t
let instance_state_t    = int32_t
let policy_t            = int32_t
let return_t            = int32_t



type qos_t
let qos_t : qos_t structure typ = structure "dds_qos_t"

let no_qos = from_voidp qos_t null

type listener_t
let listener_t : listener_t structure typ = structure "dds_listener_t"

let no_listener = from_voidp listener_t null


type topic_descriptor_t
let topic_descriptor_t : topic_descriptor_t structure typ = structure "dds_topic_descriptor_t"

let dds_bit_SKeyBValue_desc = foreign_value "dds_bit_SKeyBValue_desc" topic_descriptor_t



module SKeySValue = struct
  type t
  let t : t structure typ = structure "dds_bit_SKeySValue"
  let key  = field t "key" string
  let value = field t "value" string
  let () = seal t

  let desc = foreign_value "dds_bit_SKeySValue_desc" topic_descriptor_t

  let make k v =
    let e = make t in
    setf e key k ;
    setf e value v;
    e

  let make_array n = CArray.make t n

  let get_key v = getf v key
  let get_value v = getf v value
end

module BitBytes = struct
  type t
  let t : t structure typ = structure "dds_bit_bytes"
  let _maximum = field t "_maximum" uint32_t
  let _length = field t "_length" uint32_t
  let _buffer = field t "_buffer" (ptr uint8_t)
  let _release = field t "_release" bool
  let () = seal t

end

module SKeyBValue = struct
  type t
  let t : t structure typ = structure "dds_bit_SKeyBValue"
  let key  = field t "key" string
  let value = field t "value" BitBytes.t
  let () = seal t
end

module SampleInfo = struct
  type t
  let t : t structure typ = structure "dds_sample_info"
  let sampe_state = field t "sample_state" sample_state_t
  let view_state = field t "view_state" view_state_t
  let instance_statew = field t "instance_state" instance_state_t
  let valid_data = field t "valid_data" bool
  let source_timestamp = field t "source_timestamp" time_t
  let instance_handle = field t "instance_handle" instance_handle_t
  let publication_handle = field t "publication_handle" instance_handle_t
  let disposed_generation_count = field t "disposed_generation_count" uint32_t
  let no_writers_generation_count = field t "no_writers_generation_count" uint32_t
  let sample_rank = field t "sample_rank" uint32_t
  let generation_rank = field t "generation_rank" uint32_t
  let absolute_generation_rank = field t "absolute_generation_rank" uint32_t
  let reception_timestamp = field t "reception_timestamp" time_t
  let () = seal t

  let make_array n = CArray.make t n

end
(*
 * Memory Management
 *)
let alloc =
  foreign "dds_alloc" (uint32_t @-> returning @@ ptr void)


let free_sample =
  foreign "dds_sample_free" (ptr void @-> ptr topic_descriptor_t @-> FreeOption.t @-> returning void )


(*
 * Participant Operations
 *)
let create_participant =
  foreign "dds_create_participant" ( domain_id_t @-> ptr qos_t @-> ptr listener_t @-> returning entity_t)

let get_parent =
  foreign "dds_get_parent" (entity_t @-> returning entity_t)

let get_participant =
  foreign "dds_get_participant" (entity_t @-> returning entity_t)

(*
 * Topic Operations
 *)
let create_topic =
  foreign "dds_create_topic" (entity_t @-> ptr topic_descriptor_t @-> string @-> ptr qos_t @-> ptr listener_t @-> returning entity_t)

let find_topic =
  foreign "dds_find_topic" (entity_t @-> string @-> returning entity_t)

(*
 * Publisher/Subscriber Operations
 *)
let create_publisher =
  foreign "dds_create_publisher" (entity_t @-> ptr qos_t @-> ptr listener_t @-> returning entity_t)

let get_publisher =
  foreign "dds_get_publisher" (entity_t @-> returning entity_t)

let create_subscriber =
  foreign "dds_create_subscriber" (entity_t @-> ptr qos_t @-> ptr listener_t @-> returning entity_t)

let get_subscriber =
  foreign "dds_get_subscriber" (entity_t @-> returning entity_t)


(*
 * Reader/Writer Operations
 *)
let create_reader =
  foreign "dds_create_reader" (entity_t @-> entity_t @-> ptr qos_t @-> ptr listener_t @-> returning entity_t)

let reader_wait_for_historical_data =
  foreign "dds_reader_wait_for_historical_data" (entity_t @-> duration_t @-> returning return_t)

let wait_for_acks =
  foreign "dds_wait_for_acks" (entity_t @-> duration_t @-> returning return_t)

let read_wl =
  foreign "dds_read_wl" (entity_t @-> ptr void @-> ptr SampleInfo.t @-> int @-> returning  return_t)

let read =
  foreign "dds_read" (entity_t @-> ptr void @-> ptr SampleInfo.t @-> int @-> int @-> returning  return_t)

let return_loan =
  foreign "dds_return_loan" (entity_t @-> ptr void @-> int @-> returning return_t)

let create_writer =
  foreign "dds_create_writer" (entity_t @-> entity_t @-> ptr qos_t @-> ptr listener_t @-> returning entity_t)

let _write =
  foreign "dds_write" (entity_t @-> ptr void @-> returning return_t)

let write w s = _write w  @@ to_voidp @@ addr s

let write_list w lst = List.map  (fun s -> write w s)  lst


(*
 * QoS Operations
 *)

let create_qos =
  foreign "dds_qos_create" (void @-> returning @@ ptr qos_t)

let delete_qos =
  foreign "dds_qos_delete" (ptr qos_t @-> returning void)

let qset_durability =
  foreign "dds_qset_durability" (ptr qos_t @-> policy_t @-> returning void)

let qset_history =
  foreign "dds_qset_history" (ptr qos_t @-> policy_t @-> int32_t @-> returning void)

let qset_reliability =
  foreign "dds_qset_reliability" (ptr qos_t @-> policy_t @-> duration_t @-> returning void)

let qset_ownership =
  foreign "dds_qset_ownership" (ptr qos_t @-> policy_t  @-> returning void)

let qset_ownership_strength =
  foreign "dds_qset_ownership_strength" (ptr qos_t @-> policy_t @-> int32_t @-> returning void)

let qset_destination_order =
  foreign "dds_qset_destination_order" (ptr qos_t @-> policy_t @-> returning void)

let _qset_partition =
  foreign "dds_qset_partition" (ptr qos_t @-> uint32_t @-> ptr string @-> returning void)

let qset_partition qos plist =
  let module Array = CArray in
  let len = Unsigned.UInt32.of_int @@ List.length plist in
  let parr  = Array.of_list string plist in
  ignore ( _qset_partition qos len ( Array.start parr))



(*
let s_create_participant =
  foreign "s_create_participant" (int @-> returning entity_t)

let create_topic_sksv =
  foreign "s_create_topic_sksv" (entity_t @-> string @-> returning entity_t)

let create_topic_sv =
  foreign "s_create_topic_sv" (entity_t @-> string @-> returning entity_t)

let create_pub =
  foreign "s_create_pub" (entity_t @-> returning entity_t)

let create_pub_wp =
  foreign "s_create_pub_wp" (entity_t @-> string @-> returning entity_t)

let create_sub =
  foreign "s_create_sub" (entity_t @-> returning entity_t)

let create_sub_wp =
  foreign "s_create_sub_wp" (entity_t @-> string @-> returning entity_t)

let on_data_available_fun_t = entity_t @-> ptr void @-> returning void

let create_state_reader =
  foreign "s_create_state_reader" (entity_t @-> entity_t @-> returning entity_t)

let create_event_reader =
  foreign "s_create_event_reader" (entity_t @-> entity_t @-> returning entity_t)

let create_state_reader_wl =
  foreign "s_create_state_reader_wl" (entity_t @-> entity_t @-> funptr on_data_available_fun_t @-> returning entity_t)

let create_event_reader_wl =
  foreign "s_create_event_reader_wl" (entity_t @-> entity_t @-> funptr on_data_available_fun_t @-> returning entity_t)


(* let create_ereader s t = create_event_reader_wl s t on_data_available_ereader *)


let create_sreader s t l =
  let on_data_available_create_sreader _ _ =
    print_endline "on data available called...";
    l []
  in
  print_endline "creating reader";
  create_state_reader_wl s t on_data_available_create_sreader


let create_state_writer =
  foreign "s_create_state_writer" (entity_t @-> entity_t @-> returning entity_t)

let create_event_writer =
  foreign "s_create_event_writer" (entity_t @-> entity_t @-> returning entity_t)

let write_sksv =
  foreign "s_write_sksv" (entity_t @-> ptr dds_bit_SKeySValue  @-> returning int)

(* let write_sv =
  foreign "s_write_sksv" (entity_t @-> ptr dds_bit_SValue @-> returning int) *)

let take_sksv_a =
  foreign "s_take_sksv_a" (entity_t @-> ptr dds_bit_SKeySValue @-> returning int)

let take_sksv =
  foreign "s_take_sksv" (entity_t @-> returning (ptr dds_bit_SKeySValue))

(* let take_sv =
  foreign "s_take_sv" (entity_t @-> returning (ptr dds_bit_SValue)) *)

let write_key_value =
  foreign "s_write_key_value" (entity_t @-> string @-> string @-> returning int)

let write_value =
  foreign "s_write_value" (entity_t @-> string  @-> returning int)

let create_ereader s t callback =
  let on_data_available_ereader r _ =
    let rec take_samples r xs =
      let sample = make dds_bit_SKeySValue in
      match take_sksv_a r (addr sample) with
      | 0 -> xs
      | _ -> take_samples r (sample :: xs)
    in
    let ys = take_samples r [] in
    callback ys
  in
    create_event_reader_wl s t on_data_available_ereader *)

(*

let take_for_key =
  foreign "s_take_for_key" (entity_t @-> string -> returning string)

let take_value =
  foreign "s_take_value" (entity_t -> returning string)

*)
