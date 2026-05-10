val inbound_q : string Queue.t
val outbound_q : string Queue.t
val q_mutex : Mutex.t

type screen =
  | NameEntry
  | Lobby
  | InGame
  | GameOver of string

val current_screen : screen ref

type game_view = {
  mutable log : string list;
  mutable hand : Cs_3110_project.Types.card list;
  mutable players : string list;
  mutable status_bar : string;
  mutable prompt : string;
  mutable waiting_for_input : bool;
  mutable pending_attack : int option;
  mutable pending_choice : int option;
  mutable pending_choice_ready : bool;
  mutable in_discard_phase : bool;
  mutable lobby_connected : int;
  mutable lobby_total : int;
  mutable lobby_ready : int;
  mutable attack_incoming : bool;
  mutable attack_popup_triggered : bool;
}

val view : game_view
val parse_card : string -> Cs_3110_project.Types.card option
val string_of_card : Cs_3110_project.Types.card -> string
val view_mutex : Mutex.t
val push_inbound : string -> unit
val drain_outbound : unit -> string list
val network_thread_fn : string -> int -> unit
val update_view_from_line : string -> unit
val screen_w : int
val screen_h : int
val card_w : int
val card_h : int
val card_spacing : int
val hand_y : int
val log_x : int
val log_y : int
val log_line_h : int
val max_log_vis : int
val heart_tex : Raylib.Texture2D.t option ref
val emptyheart_tex : Raylib.Texture2D.t option ref
val attack_popup_until : float ref
val parse_health_entry : string -> (string * int * int * string list) option
val draw_log : unit -> unit
val draw_health : unit -> unit
val draw_attack_popup : unit -> unit
val card_raylib_color : Cs_3110_project.Types.card -> Raylib.Color.t
val card_description : Cs_3110_project.Types.card -> string list
val draw_tooltip : string list -> int -> int -> unit

val draw_suit_symbol :
  Cs_3110_project.Types.suit -> int -> int -> int -> Raylib.Color.t -> unit

val rank_str : Cs_3110_project.Types.card -> string
val draw_card : int -> int -> Cs_3110_project.Types.card -> bool -> unit
val is_attack_card : Cs_3110_project.Types.card -> bool
val needs_target_card : Cs_3110_project.Types.card -> bool
val draw_hand : unit -> unit
val draw_card_choice_popup : unit -> unit
val draw_target_selection : unit -> unit
val draw_action_buttons : unit -> unit
val draw_prompt : unit -> unit
val draw_game_over : string -> unit
val run_client_gui : unit -> unit
