let client_handler client_socket_address (client_in, client_out) =
  let addr_str =
    match client_socket_address with
    | Unix.ADDR_INET (inet_addr, port) ->
        Printf.sprintf "%s:%d" (Unix.string_of_inet_addr inet_addr) port
    | _ -> "unknown"
  in
  let%lwt () = Lwt_io.printlf "I got a connection from %s." addr_str in
  let%lwt () =
    Lwt_io.fprintf client_out "You are connected.\n"
  in

  (* IMPORTANT: flush so client actually receives it *)
  let%lwt () = Lwt_io.flush client_out in

  Lwt.return ()

let run_server () =
  let server () =
    let%lwt () = Lwt_io.printlf "I am the server." in
    let%lwt running_server =
      Lwt_io.establish_server_with_client_address
        (Unix.ADDR_INET (Unix.inet_addr_of_string "10.48.3.231", 5000))
        client_handler
    in
    let%lwt () = fst (Lwt.wait ()) in
    Lwt.return ()
  in
  Lwt_main.run (server ())