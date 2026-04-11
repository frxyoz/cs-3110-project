let counter = ref 0
let client_stream, client_push = Lwt_stream.create ()

let string_of_sockaddr = function
  | Unix.ADDR_UNIX s -> s
  | ADDR_INET (ip, port) ->
      Printf.sprintf "%s:%d" (Unix.string_of_inet_addr ip) port

let client_handler client_socket_address (client_in, client_out) =
  let () = counter := !counter + 1 in
  let conn_number = !counter in
  let%lwt () =
    Lwt_io.printlf "Player %d connected from %s." conn_number
      (string_of_sockaddr client_socket_address)
  in
  client_push (Some (conn_number, client_in, client_out));
  fst (Lwt.wait ())

let rec game_loop turn =
  let%lwt player_num, client_in, client_out = Lwt_stream.next client_stream in
  let%lwt () =
    Lwt_io.fprintlf client_out
      "It's your turn! (Turn %d, Player %d). Send your move:" turn player_num
  in
  let%lwt () = Lwt_io.flush client_out in
  let%lwt move = Lwt_io.read_line client_in in
  let%lwt () = Lwt_io.printlf "Player %d played: %S" player_num move in
  client_push (Some (player_num, client_in, client_out));
  game_loop (turn + 1)

let run_server () =
  let server () =
    let%lwt () = Lwt_io.printlf "Server running on port 12345." in
    let%lwt _running_server =
      Lwt_io.establish_server_with_client_address
  (Unix.ADDR_INET (Unix.inet_addr_of_string "0.0.0.0", 12345))
  client_handler
    in
    Lwt.async (fun () -> game_loop 1);
    let rec loop () =
      let%lwt () = Lwt_unix.sleep 1000.0 in
      loop ()
    in
    loop ()
  in
  Lwt_main.run (server ())

let run_client () =
  let client () =
    let%lwt () = Lwt_io.printlf "I am a client." in
    let%lwt server_in, server_out =
      Lwt_io.open_connection
        (Unix.ADDR_INET (Unix.inet_addr_of_string "127.0.0.1", 5000))
    in
    let%lwt () = Lwt_io.printlf "I connected to the server." in
    let%lwt line = Lwt_io.read_line server_in in
    let%lwt () = Lwt_io.printlf "The server said: %S." line in
    Lwt.return ()
  in
  Lwt_main.run (client ())

let _ =
  let print_usage () =
    Printf.printf "Usage: %s <server | client>\n" Sys.argv.(0)
  in
  if Array.length Sys.argv < 2 then print_usage ()
  else
    match Sys.argv.(1) with
    | "server" -> run_server ()
    | "client" -> run_client ()
    | _ -> print_usage ()
