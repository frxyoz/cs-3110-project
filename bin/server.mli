val game_state : Cs_3110_project.State.t ref
val ready_ids : int list ref
val counter : int ref

val all_clients :
  (Cs_3110_project.Player.t * Lwt_io.input_channel * Lwt_io.output_channel) list
  ref

val string_of_sockaddr : Unix.sockaddr -> string
val broadcast_to_all : string -> unit Lwt.t
val send_to : Lwt_io.output_channel -> string -> unit Lwt.t
val sleep_forever : unit -> 'a Lwt.t
val string_of_rank : Cs_3110_project.Types.rank -> string
val string_of_suit : Cs_3110_project.Types.suit -> string
val string_of_card : Cs_3110_project.Types.card -> string
val string_of_hand : Cs_3110_project.Types.card list -> string

val find_client_by_id :
  int ->
  (Cs_3110_project.Player.t * Lwt_io.input_channel * Lwt_io.output_channel)
  option

val find_player_by_name :
  string -> Cs_3110_project.State.t -> Cs_3110_project.Player.t option

val broadcast_status : unit -> unit Lwt.t
val show_all_hands : unit -> unit Lwt.t

val parse_action :
  string ->
  Cs_3110_project.Player.t ->
  Cs_3110_project.State.t ->
  (Cs_3110_project.Turn.t * int option, string) result

val action_phase_loop : int -> unit Lwt.t
val discard_phase_loop : int list -> unit Lwt.t
val game_loop : unit -> unit Lwt.t
val should_start_game : unit -> bool

val client_handler :
  Unix.sockaddr -> Lwt_io.input_channel * Lwt_io.output_channel -> unit Lwt.t

val run_server : unit -> 'a
val run_client : unit -> unit
