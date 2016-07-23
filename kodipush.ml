open Lwt
open Cohttp
open Cohttp_lwt_unix
open Cmdliner

let dump uri meth headers =
  Printf.printf "Uri: %s\nMethod: %s\nHeaders\nHeaders: %s\n%!"
    uri meth headers

let server external_address filename kodi_address =
  let callback _conn req body =
    let uri = req |> Request.uri |> Uri.to_string in
    let meth = req |> Request.meth in
    let headers = req |> Request.headers in
    ( match meth with
      | `GET ->
        dump uri (meth |> Code.string_of_method) (headers |> Header.to_string);
        FileServer.get body headers filename
      | `HEAD ->
        dump uri (meth |> Code.string_of_method) (headers |> Header.to_string);
        FileServer.head body headers filename
      | _ -> Lwt.fail (Invalid_argument "unsupported") )
    >>= (fun (headers_out, status, body) -> Server.respond ~headers:headers_out ~status ~body ())
  in
  let port = 8000 in
  let url = Printf.sprintf "http://%s:%d/%s" external_address port (Uri.pct_encode (Filename.basename filename)) in
  let server_done = Lwt_mvar.create_empty () in
  Printf.printf "%s\n%!" url;
  Lwt.async (fun () ->
      Server.create ~mode:(`TCP (`Port port)) (Server.make ~callback ()) >>= fun () ->
      Lwt_mvar.put server_done ()
    );
  Lwt_unix.sleep 0.1 >>= fun () ->
  Kodi.player_open ("http://" ^ kodi_address) url >>= fun () ->
  let rec old_handler = lazy (Lwt_unix.on_signal Sys.sigint @@ fun _ ->
                              Lwt_unix.disable_signal_handler (Lazy.force old_handler);
                              Lwt.async (fun () ->
                                  Kodi.player_stop ("http://" ^ kodi_address) >>= fun () ->
                                  Lwt_mvar.put server_done ();
                                );
                             ) in
  ignore (Lazy.force old_handler);
  Lwt_mvar.take server_done

let kodi_address =
  let doc = "Address of the Kodi server" in
  Arg.(value & opt string "xbmc:8080" & info ["k"; "kodi"] ~docv:"KODI" ~doc)

let external_address =
  let doc = "External address of the server" in
  let default = CCOpt.get "" (match ExtUnixAll.getifaddrs () with (_, x)::_ -> Some x | [] -> None) in
  Arg.(value & opt string default & info ["a"; "address"] ~docv:"ADDRESS" ~doc)

let filename =
  let doc = "File to serve" in
  Arg.(required & pos ~rev:true 0 (some string) None & info [] ~docv:"FILENAME" ~doc)

let args =
  Term.(const server $ external_address $ filename $ kodi_address)

let info = Term.info "kodipush"

let () =
  match Term.eval (args, info) with
  | `Error _ -> exit 1
  | `Version -> Printf.printf "version 0"
  | `Help -> exit 2
  | `Ok other -> Lwt_main.run other
