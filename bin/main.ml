let () = print_endline "Hello, World!"

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