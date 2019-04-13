open Lwt.Infix
open H2

let () = begin
  Ssl_threads.init ();
  Ssl.init ()
end

let default_ctx = Ssl.create_context Ssl.SSLv23 Ssl.Client_context
let () = begin
  Ssl.disable_protocols default_ctx [Ssl.SSLv23];
  Ssl.set_context_alpn_protos default_ctx [ "h2" ];
  Ssl.honor_cipher_order default_ctx;
  (* Ssl.use_bug_workarounds default_ctx; *)
end

let connect ?(ctx=default_ctx) ?src ?hostname sa =
  fun fd ->
    (match src with
      | None        -> Lwt.return_unit
      | Some src_sa -> Lwt_unix.bind fd src_sa
    ) >>= fun () ->
    Lwt_unix.connect fd sa >>= fun () ->
    begin match hostname with
      | Some host ->
        let s = Lwt_ssl.embed_uninitialized_socket fd ctx in
        Ssl.set_client_SNI_hostname
          (Lwt_ssl.ssl_socket_of_uninitialized_socket s) host;
        Lwt_ssl.ssl_perform_handshake s
      | None ->
        Lwt_ssl.ssl_connect fd ctx
    end

type body =
  | Body of string
  | Response of Response.t * [`read] Body.t

let send_request ~meth ~additional_headers ?body uri =
  let open H2_lwt_unix in
  let response_handler notify_response_received response response_body =
    Lwt.wakeup_later notify_response_received (Ok (Response (response, response_body)))
  in
  let error_handler notify_response_received error =
    let error_str = match error with
    | `Malformed_response s -> s
    | `Exn exn -> Printexc.to_string exn
    | `Protocol_error -> "Protocol Error"
    | `Invalid_response_body_length r -> Format.asprintf "%a" Response.pp_hum r
    in
    Lwt.wakeup notify_response_received (Error error_str)
  in
  let host = Uri.host_with_default uri in
  Lwt_unix.getaddrinfo host "443" [Unix.(AI_FAMILY PF_INET)]
    >>= fun addresses ->
    let sa = (List.hd addresses).Unix.ai_addr in
    let fd = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in

    connect
    ~hostname:host
    sa fd >>= fun ssl_client ->

    let request_headers = Request.create
      meth
      ~scheme:"https"
      (Uri.path_and_query uri)
      ~headers:(Headers.of_list ([
        ":authority", host;
      ] @ additional_headers))
    in

    let response_received, notify_response_received = Lwt.wait () in
    let response_handler = response_handler notify_response_received in
    let error_received, notify_error_received = Lwt.wait () in
    let error_handler = error_handler notify_error_received in

    Client.SSL.create_connection
        ~client:ssl_client
        ~error_handler
        fd
    >>= fun conn ->
    let request_body =
      Client.SSL.request
        conn
        request_headers
        ~error_handler
        ~response_handler
    in
    begin match body with
    | Some body -> Body.write_string request_body body
    | None -> ()
    end;
    Body.flush request_body (fun () ->
      Body.close_writer request_body);
    Lwt.return(response_received, error_received)

let read_response response_body =
  let buf = Buffer.create 0x2000 in
  let body_read, notify_body_read = Lwt.wait () in
  let rec read_fn () =
    Body.schedule_read
      response_body
      ~on_eof:(fun () ->
        Body.close_reader response_body;
        Lwt.wakeup_later notify_body_read (Ok (Body (Buffer.contents buf))))
      ~on_read:(fun response_fragment ~off ~len ->
        let response_fragment_bytes = Bytes.create len in
        Lwt_bytes.blit_to_bytes
          response_fragment off
          response_fragment_bytes 0
          len;
        Buffer.add_bytes buf response_fragment_bytes;
        read_fn ())
  in
  read_fn ();
  body_read

let send ?(meth=`GET) ?(additional_headers=[]) ?body uri =
  send_request ~meth ~additional_headers ?body (Uri.of_string uri) >>= fun (resp, err) ->
    Lwt.choose [resp; err] >>= function
    | Ok (Response (_r, body)) ->
      Lwt.pick [read_response body; err] >|= (function
        | Ok (Body body_str) -> body_str
        | Ok (Response _) -> assert false
        | Error err_str -> err_str)
    | Ok (Body _) -> assert false
    | Error err_str -> Lwt.return err_str

let private_github_token = Unix.getenv "GH_TOKEN"
