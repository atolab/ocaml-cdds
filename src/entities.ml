open Ctypes
open Foreign
open Types
open Policies


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


module Participant = struct
  type t = Entity.t
  let make did   = create_participant did Qos.none Listener.none
  let parent eid = get_parent eid
  let participant eid = get_participant eid
end


(*
 * Topic Operations
 *)
let create_topic =
  foreign "dds_create_topic" (Entity.t @-> ptr TopicDescriptor.t @-> string @-> ptr Qos.t @-> ptr Listener.t @-> returning Entity.t)

let find_topic =
  foreign "dds_find_topic" (Entity.t @-> string @-> returning Entity.t)

module Topic  : sig
  type t
  val make : ?policies:Policy.t list -> Entity.t -> string ->  Entity.t
  val find : Entity.t -> string -> Entity.t option
end = struct
  type t = Entity.t

  let make ?(policies=[]) dp name  =
    let qos = from_policies policies in
    create_topic dp SKeySValue.Type.desc name qos Listener.none

  let find dp name =
    match find_topic dp name with
    | eid when eid > Int32.of_int 0 -> Some(eid)
    | _ -> None
end

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


module Publisher = struct
  type t = Entity.t

  let default dp = create_publisher dp Qos.none Listener.none

  let make ?(policies=[]) dp  =
    let qos = from_policies policies in
    create_publisher dp qos Listener.none


  let get eid = get_publisher eid
end

module Subscriber = struct
  type t = Entity.t

  let default dp = create_subscriber dp Qos.none Listener.none

  let make ?(policies=[]) dp =
    let qos = from_policies policies in
    create_subscriber dp qos Listener.none

  let get eid = get_subscriber eid
end



let create_writer =
  foreign "dds_create_writer" (Entity.t @-> Entity.t @-> ptr Qos.t @-> ptr Listener.t @-> returning Entity.t)

let _write =
  foreign "dds_write" (Entity.t @-> ptr void @-> returning ReturnValue.t)

let write w s = _write w  @@ to_voidp @@ addr s

let write_list w lst = List.map  (fun s -> write w s)  lst


module Writer = struct
  type t = Entity.t

  let make ?(policies=QosPattern.state) pub topic =
    let qos = from_policies policies in
    create_writer pub topic qos Listener.none


  let write dw key value =
    let s = SKeySValue.make key value in
    write dw s

  let write_list dw ksvs =
    List.iter (fun (k,v) ->  ignore @@ write dw k v) ksvs
end

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

let set_listener =
  foreign "dds_set_listener" (Entity.t @-> ptr Listener.t @-> returning ReturnValue.t)

let create_listener_w_attch =
  foreign "dds_listener_create" (ptr void @-> returning @@ ptr Listener.t)

let create_listener () = create_listener_w_attch @@ to_voidp null


let delete_listener =
  foreign "dds_listener_delete" (ptr Listener.t @-> returning void)

let reset_listener =
  foreign "dds_listener_reset" (ptr Listener.t @-> returning void)

let lset_liveliness_lost =
  foreign "dds_lset_liveliness_lost" (ptr Listener.t @-> funptr  ~thread_registration:true ~runtime_lock:true LivelinessLost.callback @-> returning void)

let lset_inconsistent_topic =
  foreign "dds_lset_inconsistent_topic" (ptr Listener.t @-> funptr  ~thread_registration:true ~runtime_lock:true InconsistentTopic.callback @-> returning void)

let lset_offered_deadline_missed =
  foreign "dds_lset_offered_deadline_missed" (ptr Listener.t @-> funptr  ~thread_registration:true ~runtime_lock:true OfferedDeadlineMissed.callback @-> returning void)

let lset_offered_incompatible_qos =
  foreign "dds_lset_offered_incompatible_qos" (ptr Listener.t @-> funptr  ~thread_registration:true ~runtime_lock:true OfferedIncompatibleQos.callback @-> returning void)

let lset_data_on_readers =
  foreign "dds_lset_data_on_readers" (ptr Listener.t @-> funptr  ~thread_registration:true ~runtime_lock:true data_on_readers_callback_t @-> returning void)

let lset_sample_lost =
  foreign "dds_lset_sample_lost" (ptr Listener.t @-> funptr  ~thread_registration:true ~runtime_lock:true SampleLost.callback @-> returning void)

let lset_data_available =
  foreign "dds_lset_data_available" (ptr Listener.t @-> funptr ~thread_registration:true ~runtime_lock:true  data_available_callback_t @-> returning void)

let lset_sample_rejected =
  foreign "dds_lset_sample_rejected" (ptr Listener.t @-> funptr  ~thread_registration:true ~runtime_lock:true SampleRejected.callback @-> returning void)

let lset_liveliness_changed =
  foreign "dds_lset_liveliness_changed" (ptr Listener.t @-> funptr  ~thread_registration:true ~runtime_lock:true LivelinessChanged.callback @-> returning void)

let lset_requested_deadline_missed =
  foreign "dds_lset_requested_deadline_missed" (ptr Listener.t @-> funptr  ~thread_registration:true ~runtime_lock:true RequestedDeadlineMissed.callback @-> returning void)

let lset_publication_matched =
  foreign "dds_lset_publication_matched" (ptr Listener.t @-> funptr  ~thread_registration:true ~runtime_lock:true PublicationMatched.callback @-> returning void)

let lset_subscription_matched =
  foreign "dds_lset_subscription_matched" (ptr Listener.t @-> funptr  ~thread_registration:true ~runtime_lock:true SubscriptionMatched.callback @-> returning void)

let lset_requested_incompatible_qos =
  foreign "dds_lset_requested_incompatible_qos" (ptr Listener.t @-> funptr  ~thread_registration:true ~runtime_lock:true RequestedIncompatibleQos.callback @-> returning void)

module ListenerSet = struct
  type t = Listener.t structure ptr
  type  dmap = { mutable callbacks : (Entity.t * (unit -> unit)) list }
  let callbackMap = {callbacks = []}

  let make () = create_listener ()
  let make_w_attch attch = create_listener_w_attch attch

  let data_available lset callback = lset_data_available lset callback
end

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
  foreign "dds_read" (Entity.t @-> ptr void @-> ptr SampleInfo.Type.t @-> int @-> int @-> returning  ReturnValue.t)

let read e samples info buf_len max_samples  = _read e (to_voidp @@ CArray.start samples)  (CArray.start info) buf_len max_samples

let _read_wl =
  foreign "dds_read_wl" (Entity.t @-> ptr void @-> ptr SampleInfo.Type.t @-> int @-> returning  ReturnValue.t)

let read_wl e samples info max_samples = _read_wl e (to_voidp @@ CArray.start samples)  (CArray.start info) max_samples

let _read_mask_wl =
  foreign "dds_read_mask_wl" (Entity.t @-> ptr void @-> ptr SampleInfo.Type.t @-> int @-> int @-> returning  ReturnValue.t)

let read_mask_wl e samples info max_samples mask = _read_mask_wl e (to_voidp @@ CArray.start samples)  (CArray.start info) max_samples mask

let _take =
  foreign "dds_take" (Entity.t @-> ptr void @-> ptr SampleInfo.Type.t @-> int @-> int @-> returning  ReturnValue.t)

let take e samples info buf_len max_samples  = _take e (to_voidp @@ CArray.start samples)  (CArray.start info) buf_len max_samples

let _take_wl =
  foreign "dds_take_wl" (Entity.t @-> ptr void @-> ptr SampleInfo.Type.t @-> int @-> returning  ReturnValue.t)

let take_wl e samples info max_samples = _take_wl e (to_voidp @@ CArray.start samples)  (CArray.start info) max_samples

let _take_mask_wl =
  foreign "dds_read_mask_wl" (Entity.t @-> ptr void @-> ptr SampleInfo.Type.t @-> int @-> int @-> returning  ReturnValue.t)

let take_mask_wl e samples info max_samples mask = _take_mask_wl e (to_voidp @@ CArray.start samples)  (CArray.start info) max_samples mask

let _return_loan =
  foreign "dds_return_loan" (Entity.t @-> ptr void @-> int32_t @-> returning ReturnValue.t)

let return_loan e samples stored_samples = _return_loan e (to_voidp @@ CArray.start samples) stored_samples


module Reader = struct
  type daf = int32

  type t = {
    eid: Entity.t;
    samples: SKeySValue.Type.t structure ptr carray;
    info: SampleInfo.Type.t structure carray;
    max_samples: int;
    listener: Listener.t structure ptr;
    mutable on_data_available : Entity.t -> unit ptr -> unit;
    mutable on_liveliness_changed: Entity.t -> LivelinessChanged.status structure -> unit ptr -> unit
  }

  type event =
      | DataAvailable of  t
      | LivelinessLost of t * LivelinessLost.status structure
      | LivelinessChanged of  t * LivelinessChanged.status structure
      | PublicationMatched of  t * PublicationMatched.status structure
      | SubscriptionMatched of t * SubscriptionMatched.status structure


  let make ?(max_samples=128) ?(policies=QosPattern.state) sub topic  =
    let qos = from_policies policies in
    let eid = create_reader sub topic qos Listener.none in
    let samples = SKeySValue.make_ptr_array max_samples in
    let info = CArray.make SampleInfo.Type.t max_samples in
    let listener = ListenerSet.make () in
    let r = { eid; samples; info; max_samples; listener; on_data_available = (fun _ _ -> ()); on_liveliness_changed = (fun _ _ _ -> ()) } in
    r

  let read_or_take_n dr n action selector =
    let rec collect_samples kvi n samples info  = match n with
      | 0 -> kvi
      | _ ->
        let idx = n - 1 in
        let s = !@(CArray.get samples idx) in
        let i = CArray.get info idx in
        let k = (SKeySValue.key s) in
        let v = (SKeySValue.value s) in
        collect_samples (((k, v), i)::kvi) (n-1) samples info
    in
    if n <= dr.max_samples then
      begin
        let read_samples = action dr.eid dr.samples dr.info dr.max_samples (StatusSelector.to_int selector) in
        let kvis = collect_samples [] (Int32.to_int read_samples) dr.samples dr.info in
        ignore (return_loan dr.eid dr.samples read_samples) ;
        kvis
      end
    else
      begin
        let info = SampleInfo.make_array n in
        let samples = SKeySValue.make_ptr_array n in
        let read_samples = action dr.eid samples info n (StatusSelector.to_int selector) in
        let kvis = collect_samples [] (Int32.to_int read_samples) samples info in
        ignore (return_loan dr.eid samples read_samples) ;
        kvis
      end

  let read_n dr n = read_or_take_n dr n read_mask_wl StatusSelector.fresh
  let take_n dr n = read_or_take_n dr n take_mask_wl StatusSelector.fresh

  let read dr = read_n dr dr.max_samples
  let take dr = take_n dr dr.max_samples

  let selective_read sel dr = read_or_take_n dr dr.max_samples read_mask_wl sel
  let selctive_take sel dr = read_or_take_n dr dr.max_samples take_mask_wl sel

  let react dr callback =
    dr.on_data_available <- (fun _ _ ->  callback (DataAvailable dr)) ;
    dr.on_liveliness_changed <- (fun _ s _ ->  callback @@ LivelinessChanged (dr, s)) ;
    lset_data_available dr.listener dr.on_data_available ;
    lset_liveliness_changed dr.listener dr.on_liveliness_changed ;
    ignore (set_listener dr.eid dr.listener)

  let deaf dr  = reset_listener dr.listener

end
