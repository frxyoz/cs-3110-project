val special_type_of_card : Types.card -> Types.special_type option
val equipment_type_of_card : Types.card -> Types.equipment_type option
val card_type_of_card : Types.card -> Types.card_type
val effect_of_card : Types.card -> Types.card_effect

val resolve_action :
  int -> Turn.t -> int option -> State.t -> (State.t * string, string) result
