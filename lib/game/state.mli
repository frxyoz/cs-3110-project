val max_players : int

type status =
  | Waiting
  | InProgress
  | GameOver of Player.t
  | Draw

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
  deck : Types.card list;
  discard : Types.card list;
  status : status;
  turn : int;
  round : round option;
  pending : pending_attack option;
  attacks_used : int;
}

val make : unit -> t
val add_player : Player.t -> t -> (t, string) result
val remove_player : int -> t -> t
val find_player : int -> t -> Player.t option
val update_player : Player.t -> t -> t
val current_player : t -> Player.t option
val next_turn : t -> t
val set_pending : int -> int -> int -> t -> t
val clear_pending : t -> t
val start_game : t -> t
val check_game_over : t -> t
val reshuffle_if_empty : t -> t
val draw_one : Player.t -> t -> Player.t * t
val do_draw_phase : t -> t
