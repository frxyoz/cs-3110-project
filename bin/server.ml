open Lwt.Syntax
open Cs_3110_project

let game_state = ref (State.make ())

(* IDs of players who have confirmed "yes" to ready *)
let ready_ids : int list ref = ref []

(* counter increments every time a client connects, used as player id *)
let counter = ref 0

(* maps player to its I/O channels for broadcasting and turn input *)
let all_clients :
    (Player.t * Lwt_io.input_channel * Lwt_io.output_channel) list ref =
  ref []

let string_of_sockaddr = function
  | Unix.ADDR_UNIX s -> s
  | ADDR_INET (ip, port) ->
      Printf.sprintf "%s:%d" (Unix.string_of_inet_addr ip) port

let broadcast_to_all msg =
  Lwt_list.iter_p
    (fun (_, _, out) ->
      let* () = Lwt_io.fprintlf out "%s" msg in
      Lwt_io.flush out)
    !all_clients

let send_to client_out msg =
  let* () = Lwt_io.fprintlf client_out "%s" msg in
  Lwt_io.flush client_out

let rec sleep_forever () =
  let* () = Lwt_unix.sleep 1000.0 in
  sleep_forever ()

(* ── Card display ── *)

let string_of_rank = function
  | Types.Num n -> string_of_int n
  | Types.Jack -> "J"
  | Types.Queen -> "Q"
  | Types.King -> "K"
  | Types.Ace -> "A"
  | Types.Joker -> "J"

let string_of_suit = function
  | Types.Hearts -> "H"
  | Types.Diamonds -> "D"
  | Types.Clubs -> "C"
  | Types.Spades -> "S"

let string_of_card (c : Types.card) =
  if c.Types.rank = Types.Joker then
    if c.Types.color = Types.Black then "BJ" else "RJ"
  else string_of_rank c.Types.rank ^ string_of_suit c.Types.suit

let string_of_hand (hand : Types.card list) =
  if hand = [] then "(empty)"
  else
    List.mapi (fun i c -> Printf.sprintf "[%d]%s" i (string_of_card c)) hand
    |> String.concat " "

(* ── Client / player lookup ── *)

let find_client_by_id id =
  List.find_opt (fun (p, _, _) -> p.Player.id = id) !all_clients

let find_player_by_name name (s : State.t) =
  let n = String.lowercase_ascii (String.trim name) in
  List.find_opt
    (fun p -> String.lowercase_ascii (String.trim p.Player.name) = n)
    s.State.players

(* ── Status display ── *)

(* Broadcasts each player's current lives to everyone. *)
let broadcast_status () =
  let line =
    !game_state.State.players
    |> List.map (fun p ->
        Printf.sprintf "%s: %d/%d%s" p.Player.name p.Player.lives
          p.Player.max_lives
          (if Player.is_alive p then "" else " [dead]"))
    |> String.concat " | "
  in
  broadcast_to_all ("Lives: " ^ line)

(* Sends each player their own hand privately. *)
let show_all_hands () =
  Lwt_list.iter_s
    (fun (p, _, out) ->
      match State.find_player p.Player.id !game_state with
      | None -> Lwt.return ()
      | Some fresh ->
          send_to out
            (Printf.sprintf "Your hand (%d): %s"
               (List.length fresh.Player.hand)
               (string_of_hand fresh.Player.hand)))
    !all_clients

(* ── Input parsing ── Accepted formats: pass play <index> [target_name] — plays
   hand[index]; target required for attacks discard <index> — discards
   hand[index] *)
let parse_action (line : string) (actor : Player.t) (s : State.t) :
    (Turn.t * int option, string) result =
  let parts =
    String.split_on_char ' ' (String.trim line)
    |> List.filter (fun w -> w <> "")
  in
  match parts with
  | [ "pass" ] -> Ok (Turn.Pass, None)
  | "play" :: idx_str :: rest -> (
      match int_of_string_opt idx_str with
      | None -> Error "Expected a number after 'play'."
      | Some i ->
          if i < 0 || i >= List.length actor.Player.hand then
            Error
              (Printf.sprintf "Index %d out of range (hand has %d cards)." i
                 (List.length actor.Player.hand))
          else
            let card = List.nth actor.Player.hand i in
            begin match rest with
            | [] -> Ok (Turn.Play card, None)
            | name :: _ -> (
                match find_player_by_name name s with
                | None -> Error (Printf.sprintf "No player named '%s'." name)
                | Some target -> Ok (Turn.Play card, Some target.Player.id))
            end)
  | [ "discard"; idx_str ] -> (
      match int_of_string_opt idx_str with
      | None -> Error "Expected a number after 'discard'."
      | Some i ->
          if i < 0 || i >= List.length actor.Player.hand then
            Error
              (Printf.sprintf "Index %d out of range (hand has %d cards)." i
                 (List.length actor.Player.hand))
          else Ok (Turn.Discard (List.nth actor.Player.hand i), None))
  | _ ->
      Error "Unknown command. Try:  pass | play <n> [target_name] | discard <n>"

(* ── Action phase ── Loops until all alive players have passed consecutively.
   passes_in_a_row resets to 0 on any non-voluntary-pass action.

   Sub-state machine (from State.pending): pending = None → prompt the current
   player normally pending = Some → prompt the target to block or take damage *)
let rec action_phase_loop passes_in_a_row =
  game_state := State.check_game_over !game_state;
  let s = !game_state in
  match s.State.status with
  | State.GameOver _ | State.Draw -> Lwt.return ()
  | State.Waiting -> Lwt.return ()
  | State.InProgress ->
      let alive_count =
        List.length (List.filter Player.is_alive s.State.players)
      in
      if passes_in_a_row >= alive_count then Lwt.return ()
      else
        let actor_id, is_response =
          match s.State.pending with
          | Some p -> (
              match p.State.target_ids with
              | target :: _ -> (target, true)
              | [] -> failwith "invalid pending state")
          | None -> (
              match State.current_player s with
              | Some p -> (p.Player.id, false)
              | None -> failwith "no current player")
        in
        let actor =
          match State.find_player actor_id s with
          | Some p -> p
          | None -> failwith "actor not found"
        in
        (* skip dead players silently *)
        if not (Player.is_alive actor) then begin
          game_state := State.next_turn !game_state;
          action_phase_loop passes_in_a_row
        end
        else
          begin match find_client_by_id actor_id with
          | None -> broadcast_to_all "Error: active player not connected."
          | Some (_, cin, cout) ->
              let others_str =
                s.State.players
                |> List.filter (fun p ->
                    p.Player.id <> actor_id && Player.is_alive p)
                |> List.map (fun p ->
                    Printf.sprintf "%s (%d hp)" p.Player.name p.Player.lives)
                |> String.concat ", "
              in
              let prompt =
                if is_response then
                  Printf.sprintf
                    "Incoming attack! Play a block card ('play <n>') or 'pass' \
                     to take the damage.\n\
                     Your hand: %s\n\
                     > "
                    (string_of_hand actor.Player.hand)
                else
                  Printf.sprintf
                    "Your turn, %s!\n\
                     Your hand: %s\n\
                     Others: %s\n\
                     Commands: play <n> [target] | discard <n> | pass\n\
                     > "
                    actor.Player.name
                    (string_of_hand actor.Player.hand)
                    others_str
              in
              let* () = send_to cout prompt in
              let* line = Lwt_io.read_line cin in
              begin match parse_action line actor s with
              | Error msg ->
                  let* () = send_to cout ("! " ^ msg) in
                  action_phase_loop passes_in_a_row
              | Ok (action, target_id) ->
                  begin match
                    Rules.resolve_action actor_id action target_id s
                  with
                  | Error msg ->
                      let* () = send_to cout ("! " ^ msg) in
                      action_phase_loop passes_in_a_row
                  | Ok (new_state, event_msg) ->
                      game_state := new_state;
                      let* () = broadcast_to_all event_msg in
                      (* Only advance the turn when no attack is pending. An
                         attack sets pending; the response clears it. *)
                      if new_state.State.pending = None then
                        game_state := State.next_turn !game_state;
                      (* Only a voluntary pass (not taking damage) counts toward
                         ending the round. *)
                      let new_passes =
                        match action with
                        | Turn.Pass when not is_response -> passes_in_a_row + 1
                        | _ -> 0
                      in
                      action_phase_loop new_passes
                  end
              end
          end

(* ── Discard phase ── For each alive player who has more cards than lives,
   prompts them to discard one card at a time until they reach their limit. *)
let rec discard_phase_loop = function
  | [] -> Lwt.return ()
  | pid :: rest -> (
      let s = !game_state in
      match State.find_player pid s with
      | None -> discard_phase_loop rest
      | Some p ->
          if List.length p.Player.hand <= p.Player.lives then
            discard_phase_loop rest
          else
            begin match find_client_by_id pid with
            | None -> discard_phase_loop rest
            | Some (_, cin, cout) ->
                let excess = List.length p.Player.hand - p.Player.lives in
                let* () =
                  send_to cout
                    (Printf.sprintf
                       "Discard phase: %d card(s) over your limit.\n\
                        Your hand: %s\n\
                        Enter index to discard: "
                       excess
                       (string_of_hand p.Player.hand))
                in
                let* line = Lwt_io.read_line cin in
                begin match int_of_string_opt (String.trim line) with
                | None ->
                    let* () = send_to cout "! Enter a number." in
                    discard_phase_loop (pid :: rest)
                | Some i ->
                    if i < 0 || i >= List.length p.Player.hand then begin
                      let* () =
                        send_to cout
                          (Printf.sprintf "! Index %d out of range." i)
                      in
                      discard_phase_loop (pid :: rest)
                    end
                    else
                      let card = List.nth p.Player.hand i in
                      let new_p = Player.remove_from_hand card p in
                      game_state :=
                        State.update_player new_p
                          {
                            !game_state with
                            State.discard = card :: !game_state.State.discard;
                          };
                      let* () =
                        broadcast_to_all
                          (Printf.sprintf "%s discarded %s." p.Player.name
                             (string_of_card card))
                      in
                      (* re-check same player — they may still have excess
                         cards *)
                      discard_phase_loop (pid :: rest)
                end
            end)

(* ── Main game loop ── Each call drives one complete round: draw → action →
   discard. *)
let rec game_loop () =
  let s = !game_state in
  match s.State.status with
  | State.GameOver winner ->
      broadcast_to_all (Printf.sprintf "Game over! %s wins!" winner.Player.name)
  | State.Draw -> broadcast_to_all "Game over! It's a draw!"
  | State.Waiting -> Lwt_io.printlf "Bug: game_loop called before game started."
  | State.InProgress ->
      game_state := State.do_draw_phase !game_state;
      let* () = broadcast_to_all "─── Draw Phase ───" in
      let* () = broadcast_status () in
      let* () = show_all_hands () in
      let* () = broadcast_to_all "─── Action Phase ───" in
      let* () = action_phase_loop 0 in
      game_state := State.check_game_over !game_state;
      begin match !game_state.State.status with
      | State.GameOver _ | State.Draw -> game_loop ()
      | _ ->
          let* () = broadcast_to_all "─── Discard Phase ───" in
          let player_ids =
            !game_state.State.players
            |> List.filter Player.is_alive
            |> List.map (fun p -> p.Player.id)
          in
          let* () = discard_phase_loop player_ids in
          let* () = broadcast_status () in
          game_loop ()
      end

(* True when all connected players (>= 2) have said "yes" and the game hasn't
   started yet. *)
let should_start_game () =
  let n_ready = List.length !ready_ids in
  let n_players = List.length !game_state.State.players in
  n_ready >= 2 && n_ready = n_players
  && !game_state.State.status = State.Waiting

(* called by Lwt for each new TCP connection; handles the full client lifecycle:
   name entry -> lobby -> game. disconnects are caught at the bottom *)
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
                 (List.length !game_state.State.players)
                 State.max_players)
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
                    (Printf.sprintf "%s is ready! (%d/%d ready)" name n_ready
                       n_players)
                in
                if should_start_game () then begin
                  game_state := State.start_game !game_state;
                  let* () =
                    broadcast_to_all "All players ready! Game starting..."
                  in
                  game_loop ()
                end
                else begin
                  let remaining = n_players - n_ready in
                  let* () =
                    send_to client_out
                      (Printf.sprintf
                         "Waiting for %d more player(s) to be ready..."
                         remaining)
                  in
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
          ask_ready ())
    (* Catches I/O errors and clean disconnections *)
    (fun exn ->
      match
        List.find_opt (fun (_, _, out) -> out = client_out) !all_clients
      with
      | None -> Lwt_io.printlf "An anonymous client disconnected."
      | Some (p, _, _) ->
          all_clients :=
            List.filter
              (fun (pl, _, _) -> pl.Player.id <> p.Player.id)
              !all_clients;
          (match exn with
          | End_of_file -> ()
          | e -> Printf.eprintf "Client error: %s\n%!" (Printexc.to_string e));
          if !game_state.State.status = State.InProgress then begin
            game_state := { !game_state with State.status = State.Draw };
            broadcast_to_all
              (Printf.sprintf
                 "Game ended: %s disconnected. All players must remain \
                  connected for the session."
                 p.Player.name)
          end
          else begin
            ready_ids := List.filter (fun rid -> rid <> p.Player.id) !ready_ids;
            game_state := State.remove_player p.Player.id !game_state;
            let* () =
              broadcast_to_all
                (Printf.sprintf "%s left the lobby. (%d/%d players)"
                   p.Player.name
                   (List.length !game_state.State.players)
                   State.max_players)
            in
            if should_start_game () then begin
              game_state := State.start_game !game_state;
              let* () =
                broadcast_to_all "All players ready! Game starting..."
              in
              game_loop ()
            end
            else Lwt.return ()
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
      let recv () =
        let rec loop () =
          let* line = Lwt_io.read_line server_in in
          let* () = Lwt_io.printlf "%s" line in
          loop ()
        in
        loop ()
      in
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
