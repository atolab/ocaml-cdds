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

module Time = struct
  include Int64
  let t = int64_t
  let infinity = Int64.max_int
  let of_secs s = mul (of_int s) (of_int 1000000000)
  let of_millisec ms = mul (of_int ms) (of_int 1000000)
  let of_microsec us = mul (of_int us) (of_int 1000)
  let of_nanosec ns = ns
end

module Duration = Time

module DomainId = struct
  include Int32
  let t = int32_t
  let default = of_int 0
end


module Entity = struct
  include Int32
  let t = int32_t
end

module InstanceHandle = struct
  include Int64
  let t = int64_t
end

module SampleState = struct
  include Int32
  let t = int32_t
  let read  = of_int 0x01
  let not_read = of_int 0x02
  let all = of_int 0x03
end

module ViewState = struct
  include Int32
  let t = int32_t
  let novel = of_int 0x04
  let not_novel = of_int 0x08
  let any = of_int 0x0c
end

module InstanceState = struct
  include Int32
  let t = int32_t
  let alive = of_int 0x10
  let disposed = of_int 0x20
  let no_writers = of_int 0x40
  let any = of_int @@ 0x10 lor 0x20 lor  0x40
end

module ReturnValue = struct
  include Int32
  let t = int32_t
end


module Attachment = struct
  type t
  let t = ptr void

  let none = from_voidp void null
end

module Listener = struct
  type t
  let t : t structure typ = structure "dds_listener_t"

  let none = from_voidp t null
end


module TopicDescriptor = struct
  type t
  let t : t structure typ = structure "dds_topic_descriptor_t"

  let dds_bit_SKeyBValue_desc = foreign_value "dds_bit_SKeyBValue_desc" t
end



module SKeySValue = struct
  type t
  let t : t structure typ = structure "dds_bit_SKeySValue"
  let key  = field t "key" string
  let value = field t "value" string
  let () = seal t

  let desc = foreign_value "dds_bit_SKeySValue_desc" TopicDescriptor.t

  let make k v =
    let e = make t in
    setf e key k ;
    setf e value v;
    e

  let make_array n = CArray.make t n

  let get_key v = getf v key
  let get_value v = getf v value

  let set_key kv k = setf kv key k
  let set_value kv v = setf kv value v

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
  let sampe_state = field t "sample_state" SampleState.t
  let view_state = field t "view_state" ViewState.t
  let instance_statew = field t "instance_state" InstanceState.t
  let valid_data = field t "valid_data" bool
  let source_timestamp = field t "source_timestamp" Time.t
  let instance_handle = field t "instance_handle" InstanceHandle.t
  let publication_handle = field t "publication_handle" InstanceHandle.t
  let disposed_generation_count = field t "disposed_generation_count" uint32_t
  let no_writers_generation_count = field t "no_writers_generation_count" uint32_t
  let sample_rank = field t "sample_rank" uint32_t
  let generation_rank = field t "generation_rank" uint32_t
  let absolute_generation_rank = field t "absolute_generation_rank" uint32_t
  let reception_timestamp = field t "reception_timestamp" Time.t
  let () = seal t

  let make_array n = CArray.make t n

end
