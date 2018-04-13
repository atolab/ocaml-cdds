open Cdds


let writer () =
  let dp = Participant.make DomainId.default in
  Printf.printf "The dp = %d \n" (DomainId.to_int dp) ;
  let name = "KeyValue" in
  let topic = Topic.make dp name in
  Printf.printf "Created topic %d\n" @@ Entity.to_int topic ;
  let pub = Publisher.make dp in
  Printf.printf "The pub = %d\n" (Entity.to_int pub) ;
  let w = Writer.make dp topic in
  Printf.printf "Created Writer %d\n" @@ Int32.to_int w ;
  let rec loop =function
    | 0 -> ()
    | n ->
      let k = "ocaml" ^ (string_of_int (n mod 10)) in
      let v = "rulez-" ^ (string_of_int n) in
    let r = Writer.write_string w k v in
    Printf.printf "Write %d returned  %d"  n @@ Int32.to_int r ;
    print_endline "" ;
    Unix.sleepf 0.01 ;
    loop @@ n - 1
  in loop 100000 ;

  print_endline "Works!" ;
  print_endline "Released QoS!"

let handle_data dr  =
  Reader.read dr  |> List.iter (fun ((k, v), _) ->
      let vs = Ctypes.CArray.to_list v in
      let s = List.fold_left (fun a b -> a ^ ", " ^ (Char.escaped (Char.chr @@ Unsigned.UInt8.to_int b) )) "" vs  in
      Printf.printf "\tkey = %s\n\tvalue= %s\n" k s)

let handle_liveliness _ =
  print_endline "liveliness changed!"

let reader () =
  let dp = Participant.make DomainId.default in
  let name = "KeyValue" in
  let topic = Topic.make dp name in
  let sub = Subscriber.make dp in
  let r = Reader.make sub topic in
  let rec loop =function
    | 0 -> ()
    | n ->
      handle_data r ;
      print_endline "-" ;
      Unix.sleepf 0.1;
      if n mod 100 = 0 then
        begin
          Gc.print_stat stdout ;
          Gc.compact () ;
          Gc.print_stat stdout ;
        end ;
      loop @@ n - 1
  in loop 100000


let reader_wl () =
  let dp = Participant.make DomainId.default in
  let name = "KeyValue" in
  let topic = Topic.make dp name  in
  let sub = Subscriber.make dp  in
  let r = Reader.make sub topic  in

  Reader.react r (fun e -> match e with
      | Reader.DataAvailable dr ->
        Reader.read dr  |> List.iter (fun ((k, v), _) ->
            let vs = Ctypes.CArray.to_list v in
            let s = List.fold_left (fun a b -> a ^ "" ^ (Char.escaped (Char.chr @@ Unsigned.UInt8.to_int b) )) "" vs  in
            Printf.printf "\tkey = %s\n\tvalue= %s\n" k s ; print_endline "<->")
      | Reader.LivelinessChanged (_, _) -> print_endline "Liveliness Changed!" ;
      | _ -> ()
    ) ;

  let rec loop =function
    | 0 -> ()
    | n ->
      Unix.sleepf 1.0 ;
      print_endline "done sleeping...";
      loop @@ n - 1
  in loop 100000


let usage () = ignore( print_endline "USAGE:\n\t simple <pub | sub | sub-wl >" )

let _ =
  let argv = Sys.argv in
  if Array.length argv < 2 then usage ()
  else
  match Array.get argv 1 with
  | "pub" -> writer ()
  | "sub" -> reader ()
  | "sub-wl" -> reader_wl ()
  | _ -> usage ()
