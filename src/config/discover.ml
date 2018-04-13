open Base
open Stdio


module C = Configurator

let write_sexp fn sexp =
  Out_channel.write_all fn ~data:(Sexp.to_string sexp)

let on_linux = Osconfig.on_linux

let () =
  C.main ~name:"ocaml-cdds" (fun c ->
      let default : C.Pkg_config.package_conf =
        if on_linux then
          begin
            { libs   = ["-Wl,-no-as-needed"; "-lddsc"; "-lddstubs"] ;
              cflags = ["-L/usr/local/lib"] }
          end
        else
          begin
            { libs   = ["-lddsc"; "-lddstubs"] ;
              cflags = ["-L/usr/local/lib"] }
          end
      in
      let conf =
        match C.Pkg_config.get c with
        | None -> default
        | Some pc ->
          Option.value (C.Pkg_config.query pc ~package:"conf-cdds") ~default
      in

      write_sexp "c_flags.sexp"         (sexp_of_list sexp_of_string conf.cflags);
      write_sexp "c_library_flags.sexp" (sexp_of_list sexp_of_string conf.libs))
