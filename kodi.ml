open Lwt
open Cohttp
open Cohttp_lwt_unix

let jsonrpc method_ params =
  `Assoc [
    ("jsonrpc", `String "2.0");
    ("method", `String method_);
    ("params", params);
    ("id", `Int 1)
  ]

let request kodi_host method_ params =
  let json = jsonrpc method_ params in

  let headers = Header.init () in
  let headers = Header.add headers "Content-Type" "application/json" in
  let body = Lwt_stream.of_list [Yojson.Basic.to_string json] |> Cohttp_lwt_body.of_stream in
  Printf.printf "Request: %s\n%!" (Yojson.Basic.to_string json);
  Client.post ~headers ~body (Uri.of_string (kodi_host ^ "/jsonrpc")) >>= fun (resp, body) ->
  let code = resp |> Response.status |> Code.code_of_status in
  Printf.printf "Response code: %d\n%!" code;
  Printf.printf "Headers: %s\n%!" (resp |> Response.headers |> Header.to_string);
  body |> Cohttp_lwt_body.to_string >|= fun body ->
  Printf.printf "Body: %s\n%!" body;
  ()

let player_open kodi_host url =
  request kodi_host "Player.Open"  (`Assoc [
      ("item", `Assoc [
          ("file", `String url)
        ])
    ])

let player_stop kodi_host =
  request kodi_host "Player.Stop"  (`Assoc [
      ("playerid", `Int 2)])

(* curl -H 'Content-Type: application/json' --data-binary '{ "jsonrpc": "2.0", "method": "Player.Open", "params": { "item": { "file": "'"${1}"'" } }, "id": 1 }' "${XBMC_HOST}/jsonrpc" *)
