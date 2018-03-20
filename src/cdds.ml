open Ctypes
open PosixTypes
open Foreign

module FreeOption = Types.FreeOption
module Time = Types.Time
module Duration = Types.Duration
module DomainId = Types.DomainId
module Entity = Types.Entity
module InstanceHandle = Types.InstanceHandle
module SampleState = Types.SampleState
module ViestState = Types.ViewState
module InstanceState = Types.InstanceState
module ReturnValue = Types.ReturnValue
module Attachment = Types.Attachment
module Listener = Types.Listener
module TopicDescriptor = Types.TopicDescriptor
module SKeySValue = Types.SKeySValue
module SKeyBValue = Types.SKeyBValue
module BitBytes = Types.BitBytes
module SampleInfo = Types.SampleInfo

module Qos = Policies.Qos
module PolicyId = Policies.PolicyId

(*
 * Memory Management
 *)
let alloc =
  foreign "dds_alloc" (uint32_t @-> returning @@ ptr void)


let free_sample =
  foreign "dds_sample_free" (ptr void @-> ptr TopicDescriptor.t @-> FreeOption.t @-> returning void )


(*
 * Participant Operations
 *)
let create_participant =
  foreign "dds_create_participant" ( DomainId.t @-> ptr Qos.t @-> ptr Listener.t @-> returning Entity.t)

let get_parent =
  foreign "dds_get_parent" (Entity.t @-> returning Entity.t)

let get_participant =
  foreign "dds_get_participant" (Entity.t @-> returning Entity.t)

(*
 * Topic Operations
 *)
let create_topic =
  foreign "dds_create_topic" (Entity.t @-> ptr TopicDescriptor.t @-> string @-> ptr Qos.t @-> ptr Listener.t @-> returning Entity.t)

let find_topic =
  foreign "dds_find_topic" (Entity.t @-> string @-> returning Entity.t)

(*
 * Publisher/Subscriber Operations
 *)
let create_publisher =
  foreign "dds_create_publisher" (Entity.t @-> ptr Qos.t @-> ptr Listener.t @-> returning Entity.t)

let get_publisher =
  foreign "dds_get_publisher" (Entity.t @-> returning Entity.t)

let create_subscriber =
  foreign "dds_create_subscriber" (Entity.t @-> ptr Qos.t @-> ptr Listener.t @-> returning Entity.t)

let get_subscriber =
  foreign "dds_get_subscriber" (Entity.t @-> returning Entity.t)

(*
 * Listener Operations
 *
 * @TODO: This code should be refactored using functors to avoid some repetition.
 *)

module InconsistentTopic = struct
  type status
  let status : status structure typ = structure "dds_inconsistent_topic_status_t"
  let total_count  = field status "total_count" uint32_t
  let total_count_change = field status "total_count_change" uint32_t
  let () = seal status

  type callback
  let callback = (Entity.t @-> status @-> ptr void @-> returning void)

  let count s = getf s total_count
  let count_change s = getf s total_count_change
end

module LivelinessLost = struct
  type status
  let status : status structure typ = structure "dds_liveliness_lost_status_t"
  let total_count  = field status "total_count" uint32_t
  let total_count_change = field status "total_count_change" uint32_t
  let () = seal status

  type callback
  let callback = (Entity.t @-> status @-> ptr void @-> returning void)

  let count s = getf s total_count
  let count_change s = getf s total_count_change
end

module RequestedDeadlineMissed = struct
  type status
  let status : status structure typ = structure "dds_requested_deadline_missed_status_t"
  let total_count  = field status "total_count" uint32_t
  let total_count_change = field status "total_count_change" uint32_t
  let last_instance_handle = field status "last_instance_handle" InstanceHandle.t
  let () = seal status

  type callback
  let callback = (Entity.t @-> status @-> ptr void @-> returning void)

  let count s = getf s total_count
  let count_change s = getf s total_count_change
  let instance_handle s = getf s last_instance_handle
end

module OfferedDeadlineMissed = struct
  type status
  let status : status structure typ = structure "dds_offered_deadline_missed_status_t"
  let total_count  = field status "total_count" uint32_t
  let total_count_change = field status "total_count_change" uint32_t
  let last_instance_handle = field status "last_instance_handle" InstanceHandle.t
  let () = seal status

  type callback
  let callback = (Entity.t @-> status @-> ptr void @-> returning void)

  let count s = getf s total_count
  let count_change s = getf s total_count_change
  let instance_handle s = getf s last_instance_handle
end

module RequestedIncompatibleQos = struct
  type status
  let status : status structure typ = structure "dds_requested_deadline_missed_status"
  let total_count  = field status "total_count" uint32_t
  let total_count_change = field status "total_count_change" uint32_t
  let last_policy_id = field status "last_policy_id" InstanceHandle.t
  let () = seal status

  type callback
  let callback = (Entity.t @-> status @-> ptr void @-> returning void)

  let count s = getf s total_count
  let count_change s = getf s total_count_change
  let policy_id s = getf s last_policy_id
end

module OfferedIncompatibleQos = struct
  type status
  let status : status structure typ = structure "dds_offered_incompatible_qos_status_t"
  let total_count  = field status "total_count" uint32_t
  let total_count_change = field status "total_count_change" uint32_t
  let last_policy_id = field status "last_policy_id" InstanceHandle.t
  let () = seal status

  type callback
  let callback = (Entity.t @-> status @-> ptr void @-> returning void)

  let count s = getf s total_count
  let count_change s = getf s total_count_change
  let policy_id s = getf s last_policy_id
end


module SampleLost = struct
  type status
  let status : status structure typ = structure "dds_sample_lost_status_t"
  let total_count  = field status "total_count" uint32_t
  let total_count_change = field status "total_count_change" uint32_t
  let () = seal status

  type callback
  let callback = (Entity.t @-> status @-> ptr void @-> returning void)

  let count s = getf s total_count
  let count_change s = getf s total_count_change
end

module SampleRejected = struct
  type status
  let status : status structure typ = structure "dds_sample_rejected_status_t"
  let total_count  = field status "total_count" uint32_t
  let total_count_change = field status "total_count_change" uint32_t
  let last_reason = field status "last_reason" int
  let last_instance_handle = field status "last_instance_handle" InstanceHandle.t
  let () = seal status

  type callback
  let callback = (Entity.t @-> status @-> ptr void @-> returning void)

  let count s = getf s total_count
  let count_change s = getf s total_count_change
  let reason s = getf s last_reason
  let instance_handle s = getf s last_instance_handle
end

module LivelinessChanged = struct
  type status
  let status : status structure typ = structure "dds_liveliness_changed_status"
  let alive_count  = field status "alive_count" uint32_t
  let not_alive_count = field status "not_alive_count" uint32_t
  let alive_count_change = field status "alive_count_change" uint32_t
  let not_alive_count_change = field status "not_alive_count_change" uint32_t
  let last_publication_handle = field status "last_publication_handle" InstanceHandle.t
  let () = seal status

  type callback
  let callback = (Entity.t @-> status @-> ptr void @-> returning void)

  let alive s = getf s alive_count
  let not_alive s = getf s not_alive_count
  let alive_change s = getf s alive_count_change
  let not_alive_change s = getf s not_alive_count_change
  let publication_handle s = getf s last_publication_handle
end

module PublicationMatched = struct
  type status
  let status : status structure typ = structure "dds_publication_matched_status_t"
  let total_count  = field status "total_count" uint32_t
  let total_count_change = field status "total_count_change" uint32_t
  let last_reason = field status "last_reason" int
  let last_instance_handle = field status "last_instance_handle" InstanceHandle.t
  let () = seal status

  type callback
  let callback = (Entity.t @-> status @-> ptr void @-> returning void)

  let count s = getf s total_count
  let count_change s = getf s total_count_change
  let reason s = getf s last_reason
  let instance_handle s = getf s last_instance_handle
end

module SubscriptionMatched = struct
  type status
  let status : status structure typ = structure "dds_subscription_matched_status_t"
  let total_count  = field status "total_count" uint32_t
  let total_count_change = field status "total_count_change" uint32_t
  let last_reason = field status "last_reason" int
  let last_instance_handle = field status "last_instance_handle" InstanceHandle.t
  let () = seal status

  type callback
  let callback = (Entity.t @-> status @-> ptr void @-> returning void)

  let count s = getf s total_count
  let count_change s = getf s total_count_change
  let reason s = getf s last_reason
  let instance_handle s = getf s last_instance_handle
end


let data_on_readers_callback_t = (Entity.t @-> ptr void @-> returning void)

let data_available_callback_t = (Entity.t @-> ptr void @-> returning void)


let create_listener_w_attch =
  foreign "dds_listener_create" (ptr void @-> returning @@ ptr Listener.t)

let create_listener () = create_listener_w_attch @@ to_voidp null


let delete_listener =
  foreign "dds_listener_delete" (ptr Listener.t @-> returning void)

let reset_listener =
  foreign "dds_listener_reset" (ptr Listener.t @-> returning void)

let lset_liveliness_lost =
  foreign "dds_lset_liveliness_lost" (ptr Listener.t @-> funptr LivelinessLost.callback @-> returning void)

let lset_inconsistent_topic =
  foreign "dds_lset_inconsistent_topic" (ptr Listener.t @-> funptr InconsistentTopic.callback @-> returning void)

let lset_offered_deadline_missed =
  foreign "dds_lset_offered_deadline_missed" (ptr Listener.t @-> funptr OfferedDeadlineMissed.callback @-> returning void)

let lset_offered_incompatible_qos =
  foreign "dds_lset_offered_incompatible_qos" (ptr Listener.t @-> funptr OfferedIncompatibleQos.callback @-> returning void)

let lset_data_on_readers =
  foreign "dds_lset_data_on_readers" (ptr Listener.t @-> funptr data_on_readers_callback_t @-> returning void)

let lset_sample_lost =
  foreign "dds_lset_sample_lost" (ptr Listener.t @-> funptr SampleLost.callback @-> returning void)
  (**)
let lset_data_available =
  foreign "dds_lset_data_available" (ptr Listener.t @-> funptr data_available_callback_t @-> returning void)

let lset_sample_rejected =
  foreign "dds_lset_sample_rejected" (ptr Listener.t @-> funptr SampleRejected.callback @-> returning void)

let lset_liveliness_changed =
  foreign "dds_lset_liveliness_changed" (ptr Listener.t @-> funptr LivelinessChanged.callback @-> returning void)

let lset_requested_deadline_missed =
  foreign "dds_lset_requested_deadline_missed" (ptr Listener.t @-> funptr RequestedDeadlineMissed.callback @-> returning void)

let lset_publication_matched =
  foreign "dds_lset_publication_matched" (ptr Listener.t @-> funptr PublicationMatched.callback @-> returning void)

let lset_subscription_matched =
  foreign "dds_lset_subscription_matched" (ptr Listener.t @-> funptr SubscriptionMatched.callback @-> returning void)

let lset_requested_incompatible_qos =
  foreign "dds_lset_requested_incompatible_qos" (ptr Listener.t @-> funptr RequestedIncompatibleQos.callback @-> returning void)


(*
 * Reader/Writer Operations
 *)
let create_reader =
  foreign "dds_create_reader" (Entity.t @-> Entity.t @-> ptr Qos.t @-> ptr Listener.t @-> returning Entity.t)

let reader_wait_for_historical_data =
  foreign "dds_reader_wait_for_historical_data" (Entity.t @-> Duration.t @-> returning ReturnValue.t)

let wait_for_acks =
  foreign "dds_wait_for_acks" (Entity.t @-> Duration.t @-> returning ReturnValue.t)

let _read =
  foreign "dds_read" (Entity.t @-> ptr void @-> ptr SampleInfo.t @-> int @-> int @-> returning  ReturnValue.t)

let read e samples info buf_len max_samples  = _read e (to_voidp @@ CArray.start samples)  (CArray.start info) buf_len max_samples

let _read_wl =
  foreign "dds_read_wl" (Entity.t @-> ptr void @-> ptr SampleInfo.t @-> int @-> returning  ReturnValue.t)

let read_wl e samples info max_samples = _read_wl e (to_voidp @@ CArray.start samples)  (CArray.start info) max_samples

let _read_mask_wl =
  foreign "dds_read_mask_wl" (Entity.t @-> ptr void @-> ptr SampleInfo.t @-> int @-> int @-> returning  ReturnValue.t)

let read_mask_wl e samples info max_samples mask = _read_mask_wl e (to_voidp @@ CArray.start samples)  (CArray.start info) max_samples mask

let _take =
  foreign "dds_take" (Entity.t @-> ptr void @-> ptr SampleInfo.t @-> int @-> int @-> returning  ReturnValue.t)

let take e samples info buf_len max_samples  = _take e (to_voidp @@ CArray.start samples)  (CArray.start info) buf_len max_samples

let _take_wl =
  foreign "dds_take_wl" (Entity.t @-> ptr void @-> ptr SampleInfo.t @-> int @-> returning  ReturnValue.t)

let take_wl e samples info max_samples = _take_wl e (to_voidp @@ CArray.start samples)  (CArray.start info) max_samples

let _take_mask_wl =
  foreign "dds_read_mask_wl" (Entity.t @-> ptr void @-> ptr SampleInfo.t @-> int @-> int @-> returning  ReturnValue.t)

let take_mask_wl e samples info max_samples mask = _take_mask_wl e (to_voidp @@ CArray.start samples)  (CArray.start info) max_samples mask

let _return_loan =
  foreign "dds_return_loan" (Entity.t @-> ptr void @-> int32_t @-> returning ReturnValue.t)

let return_loan e samples stored_samples = _return_loan e (to_voidp @@ CArray.start samples) stored_samples

let create_writer =
  foreign "dds_create_writer" (Entity.t @-> Entity.t @-> ptr Qos.t @-> ptr Listener.t @-> returning Entity.t)

let _write =
  foreign "dds_write" (Entity.t @-> ptr void @-> returning ReturnValue.t)

let write w s = _write w  @@ to_voidp @@ addr s

let write_list w lst = List.map  (fun s -> write w s)  lst


(*
 * QoS Operations
 *)

let create_qos =
  foreign "dds_qos_create" (void @-> returning @@ ptr Qos.t)

let delete_qos =
  foreign "dds_qos_delete" (ptr Qos.t @-> returning void)

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
