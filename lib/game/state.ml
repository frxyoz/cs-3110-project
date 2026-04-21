open Types

let max_players = 5

type status =
  | Waiting (* waiting for more players/setup *)
  | InProgress
  | GameOver of Player.t (* winner *)
  | Draw (* no last-standing player, dies same time *)

type round =
  | Judgment
  | Action
  | Discard

type pending_attack = {
  attacker_id : int;
  target_id : int;
  damage : int;
}

type t = {
  players : Player.t list;
  deck : card list;
  discard : card list;
  status : status;
  turn : int;
  round : round option;
  (* Some during Action phase when an attack has been played and the target has
     not yet responded. None means it is the active player's turn. *)
  pending : pending_attack option;
  (* how many attacks the current player has used this round *)
  attacks_used : int;
}

let make () =
  {
    players = [];
    deck = [];
    discard = [];
    status = Waiting;
    turn = 0;
    round = None;
    pending = None;
    attacks_used = 0;
  }

let add_player (p : Player.t) (s : t) : (t, string) result =
  match s.status with
  | InProgress -> Error "game already in progress"
  | GameOver _ | Draw -> Error "game is over"
  | Waiting ->
      if List.length s.players >= max_players then Error "game is full"
      else Ok { s with players = s.players @ [ p ] }

let remove_player (id : int) (s : t) : t =
  { s with players = List.filter (fun p -> p.Player.id <> id) s.players }

let find_player (id : int) (s : t) : Player.t option =
  List.find_opt (fun p -> p.Player.id = id) s.players

let update_player (p : Player.t) (s : t) : t =
  {
    s with
    players =
      List.map (fun q -> if q.Player.id = p.Player.id then p else q) s.players;
  }

let current_player (s : t) : Player.t option = List.nth_opt s.players s.turn

(* Advances turn to the next alive player and resets the per-turn attack
   counter. *)
let next_turn (s : t) : t =
  let n = List.length s.players in
  { s with turn = (s.turn + 1) mod n; attacks_used = 0 }

let set_pending (attacker_id : int) (target_id : int) (damage : int) (s : t) : t
    =
  { s with pending = Some { attacker_id; target_id; damage } }

let clear_pending (s : t) : t = { s with pending = None }

(* Shuffle the deck, deal 7 cards to each player, and begin the game. *)
let start_game (s : t) : t =
  let shuffled = Deck.shuffle Deck.full_deck in
  let players, remaining_deck =
    List.fold_left
      (fun (ps, deck) p ->
        let cards, deck' = Deck.deal 7 deck in
        let p' = List.fold_left (fun acc c -> Player.force_add c acc) p cards in
        (ps @ [ p' ], deck'))
      ([], shuffled) s.players
  in
  {
    s with
    players;
    deck = remaining_deck;
    discard = [];
    status = InProgress;
    round = Some Action;
    pending = None;
    attacks_used = 0;
  }

let check_game_over (s : t) : t =
  let alive = List.filter Player.is_alive s.players in
  match alive with
  | [ winner ] -> { s with status = GameOver winner }
  | [] -> { s with status = Draw }
  | _ -> s

(* Reshuffle the discard pile into the deck when the deck is empty. *)
let reshuffle_if_empty (s : t) : t =
  if s.deck = [] then { s with deck = Deck.shuffle s.discard; discard = [] }
  else s

(* Draw one card for [p], reshuffling first if needed. Returns the updated
   player and state. *)
let draw_one (p : Player.t) (s : t) : Player.t * t =
  let s = reshuffle_if_empty s in
  match s.deck with
  | [] -> (p, s) (* deck exhausted even after reshuffle — no cards left *)
  | card :: rest -> (Player.force_add card p, { s with deck = rest })

(* Each alive player draws up to 2 cards, cant exceed their life total. *)
let do_draw_phase (s : t) : t =
  List.fold_left
    (fun st p ->
      if not (Player.is_alive p) then st
      else
        let to_draw =
          min 2 (max 0 (p.Player.lives - List.length p.Player.hand))
        in
        let rec draw_n n (pl, state) =
          if n = 0 then (pl, state)
          else
            let pl', state' = draw_one pl state in
            draw_n (n - 1) (pl', state')
        in
        let p', st' = draw_n to_draw (p, st) in
        update_player p' st')
    s s.players
