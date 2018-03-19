open Ctypes
open PosixTypes
open Cdds


let writer () =
  let dp = create_participant (Int32.of_int 0) no_qos no_listener in
  Printf.printf "The dp = %d \n" (Int32.to_int dp) ;
  let name = "KeyValue" in
  let topic = create_topic dp SKeySValue.desc name no_qos no_listener in
  Printf.printf "Created topic %d\n" @@ Int32.to_int topic ;
  let qos = create_qos () in
  let plist = ["alpha"; "beta"] in
  qset_partition qos plist ;
  let pub = create_publisher dp no_qos no_listener in
  Printf.printf "The pub = %d\n" (Int32.to_int pub) ;
  let _ = qset_durability qos (Int32.of_int 0) in
  let w = create_writer dp topic no_qos no_listener in
  Printf.printf "Created Writer %d\n" @@ Int32.to_int w ;
  let s = SKeySValue.make "ocaml" "rulez" in
  let rec loop =function
    | 0 -> ()
    | n ->
    let r = write w s in
    Printf.printf "Write %d returned  %d"  n @@ Int32.to_int r ;
    print_endline "" ;
    Unix.sleepf 0.01 ;
    loop @@ n - 1
  in loop 100000 ;

  print_endline "Works!" ;
  ignore ( delete_qos qos ) ;
  print_endline "Released QoS!"

let reader () =
  let dp = create_participant (Int32.of_int 0) no_qos no_listener in
  Printf.printf "The dp = %d \n" (Int32.to_int dp) ;
  let name = "KeyValue" in
  let topic = create_topic dp SKeySValue.desc name no_qos no_listener in
  Printf.printf "Created topic %d\n" @@ Int32.to_int topic ;
  let qos = create_qos () in
  let plist = ["alpha"; "beta"] in
  qset_partition qos plist ;
  let sub = create_subscriber dp no_qos no_listener in
  Printf.printf "The sub = %d\n" (Int32.to_int sub) ;
  let _ = qset_durability qos (Int32.of_int 0) in
  let r = create_reader dp topic no_qos no_listener in
  Printf.printf "Created Reader %d\n" @@ Int32.to_int r ;

  let max_samples = 10 in
  let info = SampleInfo.make_array max_samples in
  let samples = CArray.make (ptr SKeySValue.t) max_samples in
  for i = 0 to (max_samples -1) do
    CArray.set samples i (addr @@ make SKeySValue.t)
  done ;

  let rec loop =function
    | 0 -> ()
    | n ->
      let r = read r (to_voidp (CArray.start samples)) (CArray.start info) max_samples max_samples in
      Printf.printf "Read %d returned  %d"  n @@ Int32.to_int r ;
      if Int32.to_int r > 0 then
      begin
        let s = !@(CArray.get samples 0) in
        let k = SKeySValue.get_key s in
        let v = SKeySValue.get_value s in
        Printf.printf "\tkey = %s\n\tvalue= %s" k v ;
        print_endline "" ;
      end ;
      Unix.sleepf 0.01 ;
      loop @@ n - 1
  in loop 100000 ;

  print_endline "Works!" ;
  ignore ( delete_qos qos ) ;
  print_endline "Released QoS!"

let reader_wl () =
  let dp = create_participant (Int32.of_int 0) no_qos no_listener in
  Printf.printf "The dp = %d \n" (Int32.to_int dp) ;
  let name = "KeyValue" in
  let topic = create_topic dp SKeySValue.desc name no_qos no_listener in
  Printf.printf "Created topic %d\n" @@ Int32.to_int topic ;
  let qos = create_qos () in
  let plist = ["alpha"; "beta"] in
  qset_partition qos plist ;
  let sub = create_subscriber dp no_qos no_listener in
  Printf.printf "The sub = %d\n" (Int32.to_int sub) ;
  let _ = qset_durability qos (Int32.of_int 0) in
  let r = create_reader dp topic no_qos no_listener in
  Printf.printf "Created Reader %d\n" @@ Int32.to_int r ;

  let max_samples = 10 in
  let info = SampleInfo.make_array max_samples in
  let samples = CArray.make (ptr SKeySValue.t) max_samples in


  let rec loop =function
    | 0 -> ()
    | n ->
      let r = read_wl r (to_voidp (CArray.start samples)) (CArray.start info) max_samples in
      Printf.printf "Read %d returned  %d"  n @@ Int32.to_int r ;
      if Int32.to_int r > 0 then
        begin
          let s = !@(CArray.get samples 0) in
          let k = SKeySValue.get_key s in
          let v = SKeySValue.get_value s in
          Printf.printf "\tkey = %s\n\tvalue= %s" k v ;
          print_endline "" ;
        end ;
      ignore ( return_loan r (to_voidp (CArray.start samples)) max_samples) ;
      Unix.sleepf 0.01 ;
      loop @@ n - 1
  in loop 100000 ;

  print_endline "Works!" ;
  ignore ( delete_qos qos ) ;
  print_endline "Released QoS!"


let usage () = print_endline "USAGE:\n\t simple <pub|sub>"
let _ =
let argv = Sys.argv in
if Array.length argv < 2 then usage ()
else
match Array.get argv 1 with
| "pub" -> writer ()
| "sub" -> reader ()
| "sub-wl" -> reader_wl ()
| _ -> usage ()
