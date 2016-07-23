open Lwt
open Cohttp
open Cohttp_lwt_unix

let player_open kodi_host url =
  let json =
    `Assoc [
      ("jsonrpc", `String "2.0");
      ("method", `String "Player.Open");
      ("params", `Assoc [
          ("item", `Assoc [
              ("file", `String url)
            ])
        ]);
      ("id", `Int 1)
    ]
  in

  let headers = Header.init () in
  let headers = Header.add headers "Content-Type" "application/json" in
  let body = Lwt_stream.of_list [Yojson.Basic.to_string json] |> Cohttp_lwt_body.of_stream in
  Client.post ~headers ~body (Uri.of_string (kodi_host ^ "/jsonrpc")) >>= fun (resp, body) ->
  let code = resp |> Response.status |> Code.code_of_status in
  Printf.printf "Response code: %d\n%!" code;
  Printf.printf "Headers: %s\n%!" (resp |> Response.headers |> Header.to_string);
  body |> Cohttp_lwt_body.to_string >|= fun body ->
  Printf.printf "Body: %s\n%!" body;
  ()

(* curl -H 'Content-Type: application/json' --data-binary '{ "jsonrpc": "2.0", "method": "Player.Open", "params": { "item": { "file": "'"${1}"'" } }, "id": 1 }' "${XBMC_HOST}/jsonrpc" *)
