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

type block_type =
  | ByBlock
  | ByAttack

type pending_attack = {
  attacker_id : int;
  target_id : int;
  damage : int;
  block_with : block_type;
}

type pending_dmg = {
  dmg_actor_id : int;
  played_card : Types.card;
  waiting_on : int list;
  any_triggered : bool;
}

type t = {
  players : Player.t list;
  deck : Types.card list;
  discard : Types.card list;
  status : status;
  turn : int;
  round : round option;
  pending : pending_attack option;
  pending_dmg : pending_dmg option;
  attacks_used : int;
}

val make : unit -> t
val add_player : Player.t -> t -> (t, string) result
val remove_player : int -> t -> t
val find_player : int -> t -> Player.t option
val update_player : Player.t -> t -> t
val current_player : t -> Player.t option
val next_turn : t -> t
val set_pending : int -> int -> int -> block_type -> t -> t
val clear_pending : t -> t
val start_game : t -> t
val check_game_over : t -> t
val reshuffle_if_empty : t -> t
val draw_one : Player.t -> t -> Player.t * t
val do_draw_phase : t -> t
val onto_discard : Types.card -> t -> t
val apply_card : int -> Types.card -> t -> t
val set_pending_dmg : int -> Types.card -> int list -> t -> t
val dmg_respond : int -> bool -> t -> t
val resolve_dmg : t -> t
