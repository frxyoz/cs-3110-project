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

type t = {
  players : Player.t list;
  deck : card list;
  discard : card list;
  status : status;
  turn : int;
  round : round option;
}

(* Pending attack state for an attack, waiting for attacked player's response*)
type pending_record = {
  attacker : int;
  target : int;
  card : card;
}

type pending = pending_record option

let make () =
  {
    players = [];
    deck = [];
    discard = [];
    status = Waiting;
    turn = 0;
    round = None;
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

let current_player (s : t) : Player.t option = List.nth_opt s.players s.turn

let next_turn (s : t) : t =
  let n = List.length s.players in
  { s with turn = (s.turn + 1) mod n }

(* Shuffle the deck, deal 7 cards to each player, and begin the game. *)
let start_game (s : t) : t =
  let shuffled = Deck.shuffle Deck.full_deck in

  let players, remaining_deck =
    List.fold_left
      (fun (ps, deck) p ->
        (* deal 7 from current deck *)
        let cards, deck' = Deck.deal 7 deck in

        (* give each card to player *)
        let p' =
          List.fold_left (fun acc c -> Player.add_to_hand c acc) p cards
        in
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
  }

(*Sets the attack as impending *)
let set_pending (attacker : int) (target : int) (card : card) (s : t) : pending
    =
  Some { attacker; target; card }

(* Clears the pending attack *)
let clear_pending (_ : pending) : pending = None

let check_game_over (s : t) : t =
  let alive = List.filter Player.is_alive s.players in
  match alive with
  | [ winner ] -> { s with status = GameOver winner }
  | [] -> { s with status = Draw }
  | _ -> s
