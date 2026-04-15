open Lwt.Syntax (* write let* instead of let%lwt *)
open Cs_3110_project

let game_state = ref (State.make ())

(* IDs of players who have confirmed "yes" to ready — tracked as a list so
   disconnections can remove the right entry without an off-by-one *)
let ready_ids : int list ref = ref []

(* counter increments every time a client connects, used as player id*)
let counter = ref 0

(* maps player to its I/O channels for broadcasting and turn input *)
let all_clients :
    (Player.t * Lwt_io.input_channel * Lwt_io.output_channel) list ref =
  ref []

let string_of_sockaddr = function
  | Unix.ADDR_UNIX s -> s
  | ADDR_INET (ip, port) ->
      Printf.sprintf "%s:%d" (Unix.string_of_inet_addr ip) port

(* sends [msg] to every connected client *)
let broadcast_to_all msg =
  Lwt_list.iter_p
    (fun (_, _, out) ->
      let* () = Lwt_io.fprintlf out "%s" msg in
      Lwt_io.flush out)
    !all_clients

(* sends [msg] to a single output channel *)

let send_to client_out msg =
  let* () = Lwt_io.fprintlf client_out "%s" msg in
  Lwt_io.flush client_out

(* keeps the server process alive so Lwt continues accepting connections *)
let rec sleep_forever () =
  let* () = Lwt_unix.sleep 1000.0 in
  sleep_forever ()

(* drives the active game: prompts the current player for a move, broadcasts
   it, then advances the turn. exits when status reaches GameOver or Draw *)
let rec game_loop () =
  let s = !game_state in
  match s.State.status with
  | State.GameOver winner ->
      broadcast_to_all (Printf.sprintf "Game over! %s wins!" winner.Player.name)
  | State.Draw -> broadcast_to_all "Game over! It's a draw!"
  | State.Waiting ->
      Lwt_io.printlf "Bug: game_loop called before game started."
  | State.InProgress -> (
      match State.current_player s with
      | None -> Lwt.return () (* unreachable: turn is always in bounds *)
      | Some p -> (
          match
            List.find_opt
              (fun (p', _, _) -> p'.Player.id = p.Player.id)
              !all_clients
          with
          | None ->
              (* should not happen — game ends on disconnect *)
              broadcast_to_all "Game ended: a player disconnected."
          | Some (_, client_in, client_out) ->
              let* () =
                send_to client_out
                  (Printf.sprintf "Your turn %s! Enter your move:" p.Player.name)
              in
              let* move = Lwt_io.read_line client_in in
              let* () =
                broadcast_to_all
                  (Printf.sprintf "%s played: %s" p.Player.name move)
              in
              game_state := State.check_game_over !game_state;
              game_state := State.next_turn !game_state;
              game_loop ()))

(* True when all connected players (>= 2) have said "yes" and the game
   hasn't started yet. *)
let should_start_game () =
  let n_ready = List.length !ready_ids in
  let n_players = List.length !game_state.State.players in
  n_ready >= 2 && n_ready = n_players
  && !game_state.State.status = State.Waiting

(* called by Lwt for each new TCP connection; handles the full client
   lifecycle: name entry -> lobby -> game. disconnects are caught at the bottom *)
let client_handler client_socket_address (client_in, client_out) =
  Lwt.catch
    (fun () ->
      counter := !counter + 1;
      let id = !counter in
      let* () =
        Lwt_io.printlf "Client %d connected from %s." id
          (string_of_sockaddr client_socket_address)
      in

      (* Step 1: get name *)
      let* () = send_to client_out "Enter your name: " in
      let* name = Lwt_io.read_line client_in in
      let player = Player.make_player id name in

      (* Step 2: add player to game state *)
      match State.add_player player !game_state with
      | Error msg ->
          (* Game full or already started — reject and close *)
          send_to client_out ("Cannot join: " ^ msg)
      | Ok new_state ->
          game_state := new_state;
          all_clients := !all_clients @ [ (player, client_in, client_out) ];
          let* () =
            broadcast_to_all
              (Printf.sprintf "%s joined! (%d/%d players)" name
                 (List.length !game_state.State.players) State.max_players)
          in

          (* Step 3: lobby — keep asking until the player commits *)
          let rec ask_ready () =
            let* () = send_to client_out "Ready to play? (yes/no): " in
            let* answer = Lwt_io.read_line client_in in
            match String.lowercase_ascii (String.trim answer) with
            | "yes" ->
                ready_ids := id :: !ready_ids;
                let n_ready = List.length !ready_ids in
                let n_players = List.length !game_state.State.players in
                let* () =
                  broadcast_to_all
                    (Printf.sprintf "%s is ready! (%d/%d ready)" name
                       n_ready n_players)
                in
                if should_start_game () then begin
                  (* This fiber starts the game and drives the game loop *)
                  game_state := State.start_game !game_state;
                  let* () =
                    broadcast_to_all "All players ready! Game starting..."
                  in
                  game_loop ()
                end else begin
                  let remaining = n_players - n_ready in
                  let* () =
                    send_to client_out
                      (Printf.sprintf
                         "Waiting for %d more player(s) to be ready..."
                         remaining)
                  in
                  (* Hold the connection open while others decide *)
                  sleep_forever ()
                end
            | "no" ->
                let* () =
                  send_to client_out
                    "No problem — let us know when you're ready."
                in
                ask_ready ()
            | _ ->
                let* () = send_to client_out "Please type 'yes' or 'no'." in
                ask_ready ()
          in
          ask_ready ()
    )
    (* Catches I/O errors and clean disconnections *)
    (fun exn ->
      match
        List.find_opt (fun (_, _, out) -> out = client_out) !all_clients
      with
      | None ->
          (* Disconnected before providing a name *)
          Lwt_io.printlf "An anonymous client disconnected."
      | Some (p, _, _) ->
          all_clients :=
            List.filter
              (fun (pl, _, _) -> pl.Player.id <> p.Player.id)
              !all_clients;
          (match exn with
          | End_of_file -> ()
          | e ->
              Printf.eprintf "Client error: %s\n%!" (Printexc.to_string e));
          if !game_state.State.status = State.InProgress then begin
            (* Game requires all players to remain connected — end immediately *)
            game_state := { !game_state with State.status = State.Draw };
            broadcast_to_all
              (Printf.sprintf
                 "Game ended: %s disconnected. All players must remain \
                  connected for the session." p.Player.name)
          end else begin
            (* Lobby: clean up their slot and ready vote *)
            ready_ids :=
              List.filter (fun rid -> rid <> p.Player.id) !ready_ids;
            game_state := State.remove_player p.Player.id !game_state;
            let* () =
              broadcast_to_all
                (Printf.sprintf "%s left the lobby. (%d/%d players)" p.Player.name
                   (List.length !game_state.State.players) State.max_players)
            in
            if should_start_game () then begin
              game_state := State.start_game !game_state;
              let* () =
                broadcast_to_all "All players ready! Game starting..."
              in
              game_loop ()
            end else Lwt.return ()
          end)

let run_server () =
  Lwt_main.run
    begin
      let* () = Lwt_io.printlf "Server running on port 12345." in
      let* _ =
        Lwt_io.establish_server_with_client_address
          (Unix.ADDR_INET (Unix.inet_addr_of_string "0.0.0.0", 12345))
          client_handler
      in
      sleep_forever ()
    end

let run_client () =
  Lwt_main.run
    begin
      let* () = Lwt_io.printlf "Connecting to server..." in
      let* server_in, server_out =
        Lwt_io.open_connection
          (Unix.ADDR_INET (Unix.inet_addr_of_string "127.0.0.1", 12345))
      in
      (* receive loop: print anything server sends *)
      let recv () =
        let rec loop () =
          let* line = Lwt_io.read_line server_in in
          let* () = Lwt_io.printlf "%s" line in
          loop ()
        in
        loop ()
      in
      (* send loop: forward anything user types to server *)
      let send () =
        let rec loop () =
          let* line = Lwt_io.read_line Lwt_io.stdin in
          let* () = Lwt_io.fprintlf server_out "%s" line in
          let* () = Lwt_io.flush server_out in
          loop ()
        in
        loop ()
      in
      Lwt.join [ recv (); send () ]
    end
