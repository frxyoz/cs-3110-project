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

(* How the pending attack can be countered *)
type block_type =
  | ByBlock (* normal attacks — respond with a Block card *)
  | ByAttack (* Chaos — respond with an Attack card *)

type pending_attack = {
  attacker_id : int;
  target_ids : int list;
  damage : int;
  block_with : block_type;
}

(* Tracks an in-flight Dead Man's Gamble waiting for partner holders to
   respond *)
type pending_dmg = {
  dmg_actor_id : int;
  played_card : Types.card;
  waiting_on : int list;
  any_triggered : bool;
}

type pending_sayno_effect =
  | Heal of int
  | TwoToMax of Types.card
  | DeadMansGamble of Types.card * int list
  | Diplomacy of (int * Types.card) list
  | Sacrifice

type pending_sayno = {
  source_id : int;
  source_card : Types.card;
  waiting_on : int list;
  resolution : pending_sayno_effect;
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
  (* Some when a Dead Man's Gamble was played and partner holders haven't
     responded yet. *)
  pending_dmg : pending_dmg option;
  (* Some when a non-attack card is waiting for Say No responses. *)
  pending_sayno : pending_sayno option;
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
    pending_dmg = None;
    pending_sayno = None;
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

let set_pending (attacker_id : int) (target_id : int) (damage : int)
    (block_with : block_type) (s : t) : t =
  {
    s with
    pending =
      Some { attacker_id; target_ids = [ target_id ]; damage; block_with };
  }

let set_pending_targets (attacker_id : int) (target_ids : int list)
    (damage : int) (block_with : block_type) (s : t) : t =
  { s with pending = Some { attacker_id; target_ids; damage; block_with } }

let clear_pending (s : t) : t =
  match s.pending with
  | None -> s
  | Some p -> (
      match p.target_ids with
      | [] -> { s with pending = None }
      | _ :: rest ->
          if rest = [] then { s with pending = None }
          else { s with pending = Some { p with target_ids = rest } })

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
    pending_dmg = None;
    pending_sayno = None;
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

let onto_discard (c : Types.card) (s : t) : t =
  { s with discard = c :: s.discard }

let apply_card (actor_id : int) (c : Types.card) (s : t) : t =
  match find_player actor_id s with
  | None -> s
  | Some actor ->
      let actor' = Player.remove_from_hand c actor in
      onto_discard c (update_player actor' s)

(* ── Dead Man's Gamble helpers ── *)

let set_pending_dmg (actor_id : int) (played_card : Types.card)
    (holders : int list) (s : t) : t =
  {
    s with
    pending_dmg =
      Some
        {
          dmg_actor_id = actor_id;
          played_card;
          waiting_on = holders;
          any_triggered = false;
        };
  }

let set_pending_sayno (source_id : int) (source_card : Types.card)
    (resolution : pending_sayno_effect) (waiting_on : int list) (s : t) : t =
  {
    s with
    pending_sayno = Some { source_id; source_card; waiting_on; resolution };
  }

let dmg_respond (responder_id : int) (played : bool) (s : t) : t =
  match s.pending_dmg with
  | None -> s
  | Some pdmg ->
      let waiting_on' =
        List.filter (fun id -> id <> responder_id) pdmg.waiting_on
      in
      {
        s with
        pending_dmg =
          Some
            {
              pdmg with
              waiting_on = waiting_on';
              any_triggered = pdmg.any_triggered || played;
            };
      }

let sayno_respond (responder_id : int) (played : bool) (s : t) : t =
  match s.pending_sayno with
  | None -> s
  | Some psay ->
      let waiting_on' =
        List.filter (fun id -> id <> responder_id) psay.waiting_on
      in
      { s with pending_sayno = Some { psay with waiting_on = waiting_on' } }

let diplomacy_join (joiner_id : int) (played_card : Types.card) (s : t) : t =
  match s.pending_sayno with
  | None -> s
  | Some psay -> (
      let waiting_on' =
        List.filter (fun id -> id <> joiner_id) psay.waiting_on
      in
      match psay.resolution with
      | Diplomacy joins ->
          {
            s with
            pending_sayno =
              Some
                {
                  psay with
                  waiting_on = waiting_on';
                  resolution = Diplomacy ((joiner_id, played_card) :: joins);
                };
          }
      | _ -> s)

(* Call once waiting_on is empty to apply the +1/-1 life to the actor. *)
let resolve_dmg (s : t) : t =
  match s.pending_dmg with
  | None -> s
  | Some pdmg -> (
      let s' = { s with pending_dmg = None } in
      match find_player pdmg.dmg_actor_id s' with
      | None -> s'
      | Some actor ->
          let delta = if pdmg.any_triggered then -1 else 1 in
          update_player (Player.modify_lives delta actor) s' |> check_game_over)

let resolve_sayno (s : t) : t =
  match s.pending_sayno with
  | None -> s
  | Some psay -> (
      let s' = { s with pending_sayno = None } in
      let remove_from_discard card state =
        let rec aux = function
          | [] -> []
          | x :: rest -> if x = card then rest else x :: aux rest
        in
        { state with discard = aux state.discard }
      in
      match psay.resolution with
      | Heal amt -> (
          match find_player psay.source_id s' with
          | None -> s'
          | Some actor ->
              update_player (Player.modify_lives amt actor) s'
              |> check_game_over)
      | TwoToMax partner -> (
          match find_player psay.source_id s' with
          | None -> s'
          | Some actor ->
              let actor' =
                Player.remove_from_hand partner actor |> Player.set_max_lives 1
              in
              update_player actor' s' |> onto_discard partner
              |> onto_discard psay.source_card)
      | DeadMansGamble (played_card, holders) ->
          if holders = [] then
            match find_player psay.source_id s' with
            | None -> s'
            | Some actor ->
                update_player (Player.modify_lives 1 actor) s'
                |> check_game_over
          else set_pending_dmg psay.source_id played_card holders s'
      | Sacrifice -> (
          match find_player psay.source_id s' with
          | None -> s'
          | Some actor ->
              let actor' =
                Player.modify_lives (-3) actor |> Player.set_max_lives 1
              in
              update_player actor' s' |> check_game_over)
      | Diplomacy joins ->
          let joiners = List.rev joins in
          (* [first_joiner; ...; last_joiner] *)
          let all_players = (psay.source_id, psay.source_card) :: joiners in
          let s'' = s' in
          let s''' =
            List.fold_left
              (fun st (id, _) ->
                match find_player id st with
                | None -> st
                | Some player -> update_player (Player.modify_lives 1 player) st)
              s'' all_players
          in
          let rec swap_cards st i =
            if i >= List.length all_players - 1 then st
            else
              let id1, card1 = List.nth all_players i in
              let id2, card2 = List.nth all_players (i + 1) in
              let st =
                match find_player id1 st with
                | None -> st
                | Some player ->
                    update_player (Player.force_add card2 player) st
              in
              let st =
                match find_player id2 st with
                | None -> st
                | Some player ->
                    update_player (Player.force_add card1 player) st
              in
              swap_cards st (i + 1)
          in
          let s'''' = swap_cards s''' 0 in
          let all_cards = List.map snd joiners in
          let s''''' =
            List.fold_left
              (fun st card -> remove_from_discard card st)
              s'''' all_cards
          in
          s''''' |> check_game_over)
