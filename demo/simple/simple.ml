open Cdds

let dp = Participant.make DomainId.default
let name = "KeyValue"
let topic = Topic.make dp name
let pub = Publisher.make dp
let sub = Subscriber.make dp

let writer () =
  let w = Writer.make dp topic in
  let rec loop =function
    | 0 -> ()
    | n ->
      let k = "ocaml" ^ (string_of_int (n mod 10)) in
      let v = "rulez-" ^ (string_of_int n) in
    let r = Writer.write_string w k v in
    Printf.printf "Write %d returned  %d"  n @@ Int32.to_int r ;
    print_endline "" ;
    Unix.sleepf 0.01
    ;
    loop @@ n - 1
  in loop 100000 ;

  print_endline "Works!" ;
  print_endline "Released QoS!"

let handle_liveliness _ =
  print_endline "liveliness changed!"

let sreader () =
  let r = Reader.make sub topic in
  let rec loop () =
    Reader.sread r  Duration.infinity |> List.iter (fun ((k, v), _) ->
        let s = Bytes.to_string v in
        Printf.printf "\tkey = %s\n\tvalue= %s\n" k s) ;
    print_endline "looping..." ;
    loop ()
  in loop ()


let lreader () =
  let r = Reader.make sub topic  in

  Reader.react r (fun e -> match e with
      | Reader.DataAvailable dr ->
        Reader.read dr  |> List.iter (fun ((k, v), _) ->
            let s = Bytes.to_string v in
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

let usage () = ignore( print_endline "USAGE:\n\t simple <pub | sub | sub-wl | sub-ws>" )

let _ =
  let argv = Sys.argv in
  if Array.length argv < 2 then usage ()
  else
  match Array.get argv 1 with
  | "pub" -> writer ()
  | "sub" -> sreader ()
  | "sub-wl" -> lreader ()  
  | _ -> usage ()
