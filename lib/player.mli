type t = {
  id : int;
  name : string;
  lives : int;
  max_lives : int;
  hand : Types.card list;
}

val make_player : int -> string -> t
val is_alive : t -> bool
val set_lives : int -> t -> t
val set_max_lives : int -> t -> t
val modify_lives : int -> t -> t
val add_to_hand : Types.card -> t -> t
val force_add : Types.card -> t -> t
val remove_from_hand : Types.card -> t -> t
