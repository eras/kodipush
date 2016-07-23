open Lwt
open Cohttp
open Cohttp_lwt_unix

let dump uri meth headers =
  Printf.printf "Uri: %s\nMethod: %s\nHeaders\nHeaders: %s\n%!"
    uri meth headers

let server =
  let callback _conn req body =
    let uri = req |> Request.uri |> Uri.to_string in
    let meth = req |> Request.meth in
    let headers = req |> Request.headers in
    ( match meth with
      | `GET ->
	dump uri (meth |> Code.string_of_method) (headers |> Header.to_string);
	FileServer.get body headers "/etc/passwd"
      | _ -> Lwt.fail (Invalid_argument "unsupported") )
    >>= (fun (headers_out, status, body) -> Server.respond ~headers:headers_out ~status ~body ())
  in
  Server.create ~mode:(`TCP (`Port 8000)) (Server.make ~callback ())

let () = ignore (Lwt_main.run server)
