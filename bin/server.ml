let card_effects =
  [ (* Hearts *)
    "H2",  "a swarm of rubber ducks"
  ; "H3",  "a chocolate waterfall"
  ; "H4",  "a nuke"
  ; "H5",  "a confetti cannon"
  ; "H6",  "a herd of stampeding penguins"
  ; "H7",  "a portal to another dimension"
  ; "H8",  "an inflatable castle"
  ; "H9",  "a glitter bomb"
  ; "H10", "a time-travelling DeLorean"
  ; "HJ",  "a fire-breathing dragon"
  ; "HQ",  "a sentient taco"
  ; "HK",  "a disco ball the size of the moon"
  ; "HA",  "a choir of operatic seagulls"
    (* Diamonds *)
  ; "D2",  "a giant magnet"
  ; "D3",  "an army of militant squirrels"
  ; "D4",  "a runaway shopping trolley"
  ; "D5",  "a laser-guided banana peel"
  ; "D6",  "a weather machine set to 'chaos'"
  ; "D7",  "a self-aware Roomba"
  ; "D8",  "a mysterious fog of cinnamon"
  ; "D9",  "a trebuchet loaded with gummy bears"
  ; "D10", "a teleportation mishap"
  ; "DJ",  "a spontaneous flash mob"
  ; "DQ",  "an existential crisis in a can"
  ; "DK",  "a jetpack made of balloons"
  ; "DA",  "a stampede of caffeinated otters"
    (* Clubs *)
  ; "C2",  "a volcano of whipped cream"
  ; "C3",  "an unsolicited marching band"
  ; "C4",  "a gravity reversal (local area only)"
  ; "C5",  "a swarm of motivational bees"
  ; "C6",  "a rogue wrecking ball"
  ; "C7",  "a sentient vending machine"
  ; "C8",  "a tornado of playing cards"
  ; "C9",  "a surprise visit from a bear in a suit"
  ; "C10", "a shrink ray set to 'extra small'"
  ; "CJ",  "a moat filled with pudding"
  ; "CQ",  "a catapult of office chairs"
  ; "CK",  "a cursed accordion that plays itself"
  ; "CA",  "a flock of airborne pianos"
    (* Spades *)
  ; "S2",  "a trapdoor beneath everyone's feet"
  ; "S3",  "a robot uprising (small scale)"
  ; "S4",  "an avalanche of bubble wrap"
  ; "S5",  "a mysterious briefcase that hums"
  ; "S6",  "a black hole (tabletop size)"
  ; "S7",  "a surprise thunderstorm indoors"
  ; "S8",  "a fax machine from the future"
  ; "S9",  "a plague of googly eyes"
  ; "S10", "a philosophical argument with a parrot"
  ; "SJ",  "a surprise tax audit"
  ; "SQ",  "an interdimensional goat"
  ; "SK",  "a fog machine filled with maple syrup"
  ; "SA",  "an unstoppable mime"
  ]

  let lookup_effect code =
  List.assoc_opt (String.uppercase_ascii code) card_effects

let counter = ref 0
let client_stream, client_push = Lwt_stream.create ()
let all_clients : (int * Lwt_io.output_channel) list ref = ref []

let string_of_sockaddr = function
  | Unix.ADDR_UNIX s -> s
  | ADDR_INET (ip, port) ->
      Printf.sprintf "%s:%d" (Unix.string_of_inet_addr ip) port

let broadcast_to_all msg =
  let clients = !all_clients in
  Lwt_list.iter_p
    (fun (_num, out) ->
      let%lwt () = Lwt_io.fprintlf out "%s" msg in
      Lwt_io.flush out)
    clients

let client_handler client_socket_address (client_in, client_out) =
  let () = counter := !counter + 1 in
  let conn_number = !counter in
  let%lwt () =
    Lwt_io.printlf "Player %d connected from %s." conn_number
      (string_of_sockaddr client_socket_address)
  in
  all_clients := (conn_number, client_out) :: !all_clients;
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
  (* Check if the move is a special card code and broadcast the effect *)
  let%lwt () =
    match lookup_effect move with
    | Some card_effect ->
      let msg =
        Printf.sprintf "*** Player %d used %s! ***" player_num card_effect
      in
      let%lwt () = Lwt_io.printlf "%s" msg in
      broadcast_to_all msg
    | None -> Lwt.return ()
  in
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