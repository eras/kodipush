open Lwt
open Cohttp
open Cohttp_lwt_unix
open Cmdliner

let dump uri meth headers =
  Printf.printf "Uri: %s\nMethod: %s\nHeaders\nHeaders: %s\n%!"
    uri meth headers

let server filename =
  let callback _conn req body =
    let uri = req |> Request.uri |> Uri.to_string in
    let meth = req |> Request.meth in
    let headers = req |> Request.headers in
    ( match meth with
      | `GET ->
        dump uri (meth |> Code.string_of_method) (headers |> Header.to_string);
        FileServer.get body headers filename
      | _ -> Lwt.fail (Invalid_argument "unsupported") )
    >>= (fun (headers_out, status, body) -> Server.respond ~headers:headers_out ~status ~body ())
  in
  Server.create ~mode:(`TCP (`Port 8000)) (Server.make ~callback ())

let filename =
  let doc = "File to serve" in
  Arg.(required & pos ~rev:true 0 (some string) None & info [] ~doc)

let args =
  Term.(filename)

let info = Term.info "kodipush"

let () =
  match Term.eval (args, info) with
  | `Error _ -> exit 1
  | `Version -> Printf.printf "version 0"
  | `Help -> exit 2
  | `Ok other -> Lwt_main.run (server other)
