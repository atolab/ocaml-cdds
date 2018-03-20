open Ctypes
open PosixTypes
open Cdds


let writer () =
  let dp = create_participant DomainId.default Qos.none Listener.none in
  Printf.printf "The dp = %d \n" (DomainId.to_int dp) ;
  let name = "KeyValue" in
  let topic = create_topic dp SKeySValue.desc name Qos.none Listener.none in
  Printf.printf "Created topic %d\n" @@ Entity.to_int topic ;
  let qos = create_qos () in
  let plist = ["alpha"; "beta"] in
  qset_partition qos plist ;
  let pub = create_publisher dp Qos.none Listener.none in
  Printf.printf "The pub = %d\n" (Entity.to_int pub) ;
  let _ = qset_durability qos (Int32.of_int 0) in
  let w = create_writer dp topic Qos.none Listener.none in
  Printf.printf "Created Writer %d\n" @@ Int32.to_int w ;
  let s = SKeySValue.make "ocaml" "rulez" in
  let rec loop =function
    | 0 -> ()
    | n ->
    let r = write w s in
    Printf.printf "Write %d returned  %d"  n @@ Int32.to_int r ;
    print_endline "" ;
    Unix.sleepf 0.1 ;
    loop @@ n - 1
  in loop 100000 ;

  print_endline "Works!" ;
  ignore ( delete_qos qos ) ;
  print_endline "Released QoS!"

let handle_data dr _  =
  (* print_endline ">> Listener called!"; *)
  Printf.printf ">>> Reader %d\n" @@ Int32.to_int dr ;
  let max_samples = 10 in
  (* print_endline ">> Creating Array!" ; *)
  let info = SampleInfo.make_array max_samples in
  let samples = CArray.make (ptr SKeySValue.t) max_samples in
  (* print_endline ">> Reading Data!" ; *)
  let sample_num = read_wl dr samples info max_samples in
  Printf.printf "Read returned  %d\n" @@ Int32.to_int sample_num ;
  (* print_endline ">> Done reading data!" ; *)
  if Int32.to_int sample_num > 0 then
    begin
      let s = !@(CArray.get samples 0) in
      let k = SKeySValue.get_key s in
      let v = SKeySValue.get_value s in
      Printf.printf "\tkey = %s\n\tvalue= %s" k v ;
      (* print_endline "" ; *)
    end ;
  ignore ( return_loan dr samples sample_num)


let reader () =
  let dp = create_participant (Int32.of_int 0) Qos.none Listener.none in
  Printf.printf "The dp = %d \n" (Int32.to_int dp) ;
  let name = "KeyValue" in
  let topic = create_topic dp SKeySValue.desc name Qos.none Listener.none in
  Printf.printf "Created topic %d\n" @@ Int32.to_int topic ;
  let qos = create_qos () in
  let plist = ["alpha"; "beta"] in
  qset_partition qos plist ;
  let sub = create_subscriber dp Qos.none Listener.none in
  Printf.printf "The sub = %d\n" (Int32.to_int sub) ;
  let _ = qset_durability qos (Int32.of_int 0) in
  let r = create_reader dp topic Qos.none Listener.none in
  Printf.printf "Created Reader %d\n" @@ Int32.to_int r ;

  let max_samples = 10 in
  let info = SampleInfo.make_array max_samples in
  (* let samples = SKeySValue.make_array max_samples in *)
  let samples = CArray.make (ptr SKeySValue.t) max_samples in
  for i = 0 to (max_samples - 1) do
    CArray.set samples i (allocate SKeySValue.t (SKeySValue.make "" ""))
  done ;

  let rec loop =function
    | 0 -> ()
    | n ->
      let r = read r samples info max_samples max_samples in
      Printf.printf "Read %d returned  %d"  n @@ Int32.to_int r ;
      if Int32.to_int r > 0 then
      begin
        let s = !@(CArray.get samples 0) in
        let k = SKeySValue.get_key s in
        let v = SKeySValue.get_value s in
        let _ = SKeySValue.set_key s "" in
        let _ = SKeySValue.set_value s "" in
        Printf.printf "\tkey = %s\n\tvalue= %s" k v ;
        print_endline "" ;
      end ;
      Unix.sleepf 0.01 ;
      loop @@ n - 1
  in loop 100000 ;

  print_endline "Works!" ;
  ignore ( delete_qos qos ) ;
  print_endline "Released QoS!"

let reader_wlr () =
  let dp = create_participant (Int32.of_int 0) Qos.none Listener.none in
  Printf.printf "The dp = %d \n" (Int32.to_int dp) ;
  let name = "KeyValue" in
  let topic = create_topic dp SKeySValue.desc name Qos.none Listener.none in
  Printf.printf "Created topic %d\n" @@ Int32.to_int topic ;
  let qos = create_qos () in
  let plist = ["alpha"; "beta"] in
  qset_partition qos plist ;
  let sub = create_subscriber dp Qos.none Listener.none in
  Printf.printf "The sub = %d\n" (Int32.to_int sub) ;
  let _ = qset_durability qos (Int32.of_int 0) in
  let r = create_reader dp topic Qos.none Listener.none in
  Printf.printf "Created Reader %d\n" @@ Int32.to_int r ;

  let max_samples = 10 in
  let info = SampleInfo.make_array max_samples in
  let samples = CArray.make (ptr SKeySValue.t) max_samples in

  let rec loop =function
    | 0 -> ()
    | n -> handle_data r 0 ;
      let read_samples = read_wl r samples info max_samples in
      Printf.printf "Read %d returned  %d"  n @@ Int32.to_int read_samples ;
      if Int32.to_int r > 0 then
        begin
          let s = !@(CArray.get samples 0) in
          let k = SKeySValue.get_key s in
          let v = SKeySValue.get_value s in
          Printf.printf "\tkey = %s\n\tvalue= %s" k v ;
          print_endline "" ;
        end ;
      ignore ( return_loan r samples read_samples) ;
      Unix.sleep 1 ;
      loop @@ n - 1
  in loop 100000 ;

  print_endline "Works!" ;
  ignore ( delete_qos qos ) ;
  print_endline "Released QoS!"


let reader_wlr_wl () =
  let dp = create_participant (Int32.of_int 0) Qos.none Listener.none in
  Printf.printf "The dp = %d \n" (Int32.to_int dp) ;
  let name = "KeyValue" in
  let topic = create_topic dp SKeySValue.desc name Qos.none Listener.none in
  Printf.printf "Created topic %d\n" @@ Int32.to_int topic ;
  let qos = create_qos () in
  let plist = ["alpha"; "beta"] in
  qset_partition qos plist ;
  let sub = create_subscriber dp Qos.none Listener.none in
  Printf.printf "The sub = %d\n" (Int32.to_int sub) ;
  let _ = qset_durability qos (Int32.of_int 0) in
  let listener = create_listener () in
  let _ = lset_data_available listener handle_data in
  let r = create_reader dp topic Qos.none listener in
  Printf.printf "Created Reader %d\n" @@ Int32.to_int r ;

  let rec loop =function
    | 0 -> ()
    | n ->
      Unix.sleepf 1.0 ;
      print_endline "done sleeping...";
      loop @@ n - 1
  in loop 100000 ;

  print_endline "Works!" ;
  ignore ( delete_qos qos ) ;
  print_endline "Released QoS!"


let usage () = print_endline "USAGE:\n\t simple <pub | sub | sub-wlr | sub-wlr-wl>"
let _ =
let argv = Sys.argv in
if Array.length argv < 2 then usage ()
else
match Array.get argv 1 with
| "pub" -> writer ()
| "sub" -> reader ()
| "sub-wlr" -> reader_wlr ()
| "sub-wlr-wl" -> reader_wlr_wl ()
| _ -> usage ()
