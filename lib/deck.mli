val color_of_suit : Types.suit -> Types.color
val make_card : Types.rank -> Types.suit -> Types.card
val full_deck : Types.card list
val deal : int -> Types.card list -> Types.card list * Types.card list
val shuffle : Types.card list -> Types.card list
