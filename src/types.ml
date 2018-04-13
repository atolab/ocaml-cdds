open Ctypes
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

  let of_int i = Int32.of_int i
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
  let any = of_int 0x03
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

module StatusSelector = struct
  type t = {s : SampleState.t; i : InstanceState.t; v : ViewState.t}
  let make s i v = {s; i; v}
  let to_int sel =  (Int32.to_int sel.s) lor (Int32.to_int sel.i) lor (Int32.to_int sel.v)
  let of_int n = {s = Int32.of_int @@ n land 0x03; i = Int32.of_int @@ n land 0x70; v = Int32.of_int @@ n land 0x00c}

  let any = {s = SampleState.any; i = InstanceState.any; v = ViewState.any}
  let fresh = {s = SampleState.not_read; i = InstanceState.alive; v = ViewState.any}
  let disposed = {s = SampleState.any; i = InstanceState.disposed; v = ViewState.any}
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
  module Type = struct
    type t
    let t : t structure typ = structure "dds_bit_SKeySValue"
    let key  = field t "key" string
    let value = field t "value" string
    let () = seal t

    let desc = foreign_value "dds_bit_SKeySValue_desc" TopicDescriptor.t
  end

  let make k v =
    let e = make Type.t in
    setf e Type.key k ;
    setf e Type.value v;
    e

  let make_array n = CArray.make Type.t n

  let make_ptr_array n = CArray.make (ptr Type.t) n

  let key v = getf v Type.key
  let value v = getf v Type.value

  let set_key kv k = setf kv Type.key k
  let set_value kv v = setf kv Type.value v
end


module BitBytes = struct
  module Type = struct
    type t
    let t : t structure typ = structure "dds_bit_bytes"
    let _maximum = field t "_maximum" uint32_t
    let _length = field t "_length" uint32_t
    let _buffer = field t "_buffer"  (ptr char)
    let _release = field t "_release" bool
    let () = seal t
  end

  let length b = getf b Type._length

  let to_ptr b = getf b Type._buffer

  let of_array xs  =
    let s = make Type.t in
    let ulen = Unsigned.UInt32.of_int @@ CArray.length xs in
    let ptr = CArray.start xs in
    setf s Type._maximum ulen ;
    setf s Type._length ulen ;
    setf s Type._buffer ptr ;
    setf s Type._release false ;
    s

  let make len ptr =
    let s = make Type.t in
    let ulen = Unsigned.UInt32.of_int len in
    setf s Type._maximum ulen ;
    setf s Type._length ulen ;
    setf s Type._buffer ptr ;
    setf s Type._release false ;
    s

end

module SKeyBValue = struct

  module Type = struct
    type t
    let t : t structure typ = structure "dds_bit_SKeyBValue"
    let key  = field t "key" string
    let value = field t "value" BitBytes.Type.t
    let () = seal t

    let desc = foreign_value "dds_bit_SKeyBValue_desc" TopicDescriptor.t
  end

  let make k v =
    let e = make Type.t in
    setf e Type.key k ;
    setf e Type.value v;
    e

  let make_array n = CArray.make Type.t n

  let make_ptr_array n = CArray.make (ptr Type.t) n

  let key v = getf v Type.key
  let value v = getf v Type.value

  let set_key kv k = setf kv Type.key k
  let set_value kv v = setf kv Type.value v
end


module SampleInfo = struct
  module Type = struct
    type t
    let t : t structure typ = structure "dds_sample_info"
    let sample_state = field t "sample_state" SampleState.t
    let view_state = field t "view_state" ViewState.t
    let instance_state = field t "instance_state" InstanceState.t
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
  end


  let make () = make Type.t

  let make_array n = CArray.make Type.t n

  let valid_data si = getf si Type.valid_data
  let sample_state si = getf si Type.sample_state
  let view_state si = getf si Type.view_state
  let instance_state si = getf si Type.instance_state
end
