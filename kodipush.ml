open Lwt
open Cohttp
open Cohttp_lwt_unix

let body_of_file ?(range=(None, None)) filename =
  let st, push_st = Lwt_stream.create_bounded 100 in
  let body = Cohttp_lwt_body.of_stream st in
  let file = open_in filename in
  let bytes_left, range =
    let total = in_channel_length file in
    match range with
      | (None, None) ->
	(ref 0, ((0, total - 1), total))
      | (None, Some r1) ->
	seek_in file (total - r1);
        (ref r1, ((total - r1, total - 1), total))
      | (Some r0, None) ->
	seek_in file r0;
        (ref (total - r0), ((r0, total - 1), total))
      | (Some r0, Some r1) ->
        (ref (r1 - r0 + 1), ((r0, r1), total))
  in
  let _ =
    Lwt.catch (fun () ->
      let rec respond () =
	let buffer = Bytes.create (max 0 (min 4096 !bytes_left)) in
	let got =
	  if Bytes.length buffer > 0
	  then input file buffer 0 (Bytes.length buffer)
	  else 0
	in
	if got = 0
	then begin
	  push_st#close;
	  close_in file;
	  Lwt.return ()
	end
	else begin
	  bytes_left := !bytes_left - got;
	  push_st#push buffer >>=
	  respond
	end
      in respond ()
    ) (fun exn ->
      close_in file;
      return ()
    )
  in
  Lwt.return (range, body)

let range_of_headers =
  let bytes_re = Re_perl.re "^bytes=([0-9]*)-([0-9]*)$" |> Re_perl.compile in
  fun headers ->
    match Header.get headers "range" with
      | None -> (None, None)
      | Some range_hdr when Re.execp bytes_re range_hdr ->
	let subs = Re.exec bytes_re range_hdr in
	let int_of_maybe_empty_string = function
	  | "" -> None
	  | str -> Some (int_of_string str)
	in
	let range0 = Re.get subs 1 |> int_of_maybe_empty_string in
	let range1 = Re.get subs 2 |> int_of_maybe_empty_string in
	(range0, range1)
      | Some _ -> assert false
	
let get body headers uri =
  let range = range_of_headers headers in
  body |> Cohttp_lwt_body.to_string >>= fun _ ->
    let is_range = range <> (None, None) in
    let status =
      if is_range
      then `OK
      else `Partial_content
    in
    let headers = Header.init () in
    body_of_file ~range "/etc/passwd" >>= fun (range_info, body) ->
    let ((r0, r1), total) = range_info in
    let headers =
      if is_range then
	Header.add headers "Content-Range"
        (Printf.sprintf "bytes %d-%d/%d" r0 r1 total)
	(* headers *)
      else 
        headers
    in
    let headers = Header.add headers "Content-Length" (string_of_int (r1 - r0 + 1)) in
    Lwt.return (headers, status, body)

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
	get body headers uri
      | _ -> Lwt.fail (Invalid_argument "unsupported") )
    >>= (fun (headers_out, status, body) -> Server.respond ~headers:headers_out ~status ~body ())
  in
  Server.create ~mode:(`TCP (`Port 8000)) (Server.make ~callback ())

let () = ignore (Lwt_main.run server)
