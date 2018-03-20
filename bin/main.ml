open Ocamlcdds
open Ctypes
open Foreign

let pub n =
  let dp = create_participant 0 in
  let t = create_topic_sksv dp "KeyValue" in
  let w = create_state_writer dp  t in

  let rec loop k =
    match k with
    | i when i > 0 ->
        print_endline ">> writing..";
        let key = "ocaml" in
        let value = Printf.sprintf "fun-[%d]" (n-k) in
        ignore (write_key_value w key value);
        Unix.sleepf 0.5 ;
        loop (k-1)
    | _ -> ()
  in
    loop n

let listener xs =
  print_endline ">> Listener called!";
  Unix.execv "/Applications/Utilities/Terminal.app/Contents/MacOS/Terminal" [| |]


let lsub n =
  print_endline  "Starting lsub!\n" ;
  let dp = create_participant 0 in
  let t = create_topic_sksv dp "KeyValue" in
  let listener xs =
    Printf.printf "samples received: %d \n" (List.length xs);
    let _ = List.for_all (fun x -> Printf.printf "(%s, %s)\n" (getf x key) (getf x value); true) xs  in ()
  in let _ = create_ereader dp t listener in Unix.sleep 10

let fsub n =
  Printf.printf "Starting sub!\n" ;
  let dp = create_participant 0 in
  let t = create_topic_sksv dp "KeyValue" in
  let r = create_state_reader dp  t in
  let rec loop k =
    match k with
    | i when i > 0 ->
      let sample = make dds_bit_SKeySValue in
      let h =
        match take_sksv_a r (addr sample) with
        | 0 ->
          let _ = Printf.printf "No samples available" in k
        | _ ->
          let a = (getf sample key) in
          let b = (getf sample value) in
          let _ = Printf.printf "(k: %s, v: %s)" a b in (k-1)
      in
      Unix.sleep 1 ;
      print_endline "";
      loop h
    | _ -> ()
  in
  loop n

let sub n =
  Printf.printf "Starting sub!\n" ;
  let dp = create_participant 0 in
  let t = create_topic_sksv dp "KeyValue" in
  let r = create_state_reader dp  t in
  for i = 0 to n-1 do
    let sample = make dds_bit_SKeySValue in
    let _ = match take_sksv_a r (addr sample) with
    | 0 ->
      Printf.printf "No samples available" ;
    | _ ->
      let k = getf sample key in
      let v = getf sample value in
      Printf.printf "(k: %s, v: %s)\n" k v  ;
    in Unix.sleep 1 ;
    print_endline ""
   done

let usage () = Printf.printf "USAGE:\n\tmain <pub|sub> <n>\n"

let () =
  if Array.length Sys.argv > 2 then
    let opt = Sys.argv.(1) in
    let count = int_of_string Sys.argv.(2) in
    match opt with
    | "pub" -> pub count
    | "lsub" -> lsub count
    | "fsub" -> fsub count
    | _ -> usage ()
  else
    usage ()
