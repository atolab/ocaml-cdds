open Ctypes
open PosixTypes
open Foreign

type dds_entity_t = unit ptr
let dds_entity_t : dds_entity_t typ = ptr void

type qos_t        = unit ptr
let qos_t : qos_t typ = ptr void

type listner_t    = unit ptr
let listner_t : listner_t typ = ptr void

type dds_topic_descriptor_t = unit ptr
let dds_topic_descriptor_t : dds_topic_descriptor_t typ = ptr void

type dds_bit_SKeySValue
let dds_bit_SKeySValue : dds_bit_SKeySValue structure typ = structure "dds_bit_SKeySValue"
let key  = field dds_bit_SKeySValue "key" string
let value = field dds_bit_SKeySValue "value" string
let () = seal dds_bit_SKeySValue

(* type dds_bit_SValue
let dds_bit_SValue : dds_bit_SValue structure typ = structure "dds_bit_SValue"
let key  = field dds_bit_SValue "key" string
let () = seal dds_bit_SValue *)


let create_participant =
  foreign "s_create_participant" (int @-> returning dds_entity_t)

let create_topic_sksv =
  foreign "s_create_topic_sksv" (dds_entity_t @-> string @-> returning dds_entity_t)

let create_topic_sv =
  foreign "s_create_topic_sv" (dds_entity_t @-> string @-> returning dds_entity_t)

let create_pub =
  foreign "s_create_pub" (dds_entity_t @-> returning dds_entity_t)

let create_pub_wp =
  foreign "s_create_pub_wp" (dds_entity_t @-> string @-> returning dds_entity_t)

let create_sub =
  foreign "s_create_sub" (dds_entity_t @-> returning dds_entity_t)

let create_sub_wp =
  foreign "s_create_sub_wp" (dds_entity_t @-> string @-> returning dds_entity_t)

let on_data_available_fun_t = dds_entity_t @-> ptr void @-> returning void

let create_state_reader =
  foreign "s_create_state_reader" (dds_entity_t @-> dds_entity_t @-> returning dds_entity_t)

let create_event_reader =
  foreign "s_create_event_reader" (dds_entity_t @-> dds_entity_t @-> returning dds_entity_t)

let create_state_reader_wl =
  foreign "s_create_state_reader_wl" (dds_entity_t @-> dds_entity_t @-> funptr on_data_available_fun_t @-> returning dds_entity_t)

let create_event_reader_wl =
  foreign "s_create_event_reader_wl" (dds_entity_t @-> dds_entity_t @-> funptr on_data_available_fun_t @-> returning dds_entity_t)


(* let create_ereader s t = create_event_reader_wl s t on_data_available_ereader *)


let create_sreader s t l =
  let on_data_available_create_sreader r a =
    print_endline "on data available called...";
    l []
  in
  print_endline "creating reader";
  create_state_reader_wl s t on_data_available_create_sreader


let create_state_writer =
  foreign "s_create_state_writer" (dds_entity_t @-> dds_entity_t @-> returning dds_entity_t)

let create_event_writer =
  foreign "s_create_event_writer" (dds_entity_t @-> dds_entity_t @-> returning dds_entity_t)

let write_sksv =
  foreign "s_write_sksv" (dds_entity_t @-> ptr dds_bit_SKeySValue  @-> returning int)

(* let write_sv =
  foreign "s_write_sksv" (dds_entity_t @-> ptr dds_bit_SValue @-> returning int) *)

let take_sksv_a =
  foreign "s_take_sksv_a" (dds_entity_t @-> ptr dds_bit_SKeySValue @-> returning int)

let take_sksv =
  foreign "s_take_sksv" (dds_entity_t @-> returning (ptr dds_bit_SKeySValue))

(* let take_sv =
  foreign "s_take_sv" (dds_entity_t @-> returning (ptr dds_bit_SValue)) *)

let write_key_value =
  foreign "s_write_key_value" (dds_entity_t @-> string @-> string @-> returning int)

let write_value =
  foreign "s_write_value" (dds_entity_t @-> string  @-> returning int)

let create_ereader s t callback =
  let on_data_available_ereader r a =    
    let rec take_samples r xs =
      let sample = make dds_bit_SKeySValue in
      match take_sksv_a r (addr sample) with
      | 0 -> xs
      | _ -> take_samples r (sample :: xs)
    in
    let ys = take_samples r [] in
    callback ys
  in
    create_event_reader_wl s t on_data_available_ereader

(*

let take_for_key =
  foreign "s_take_for_key" (dds_entity_t @-> string -> returning string)

let take_value =
  foreign "s_take_value" (dds_entity_t -> returning string)

*)
