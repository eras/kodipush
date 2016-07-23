val get : Cohttp_lwt_body.t -> Cohttp.Header.t -> string -> (Cohttp.Header.t * [> `OK | `Partial_content ] * Cohttp_lwt_body.t) Lwt.t
val head : Cohttp_lwt_body.t -> Cohttp.Header.t -> string -> (Cohttp.Header.t * [> `OK | `Partial_content ] * Cohttp_lwt_body.t) Lwt.t
