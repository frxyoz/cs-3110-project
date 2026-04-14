(* building & dealing a deck of cards *)

open Types

let color_of_suit = function
  | Hearts | Diamonds -> Red
  | Clubs | Spades -> Black

let make_card rank suit = { rank; suit; color = color_of_suit suit }

let full_deck =
  let suits = [ Hearts; Diamonds; Clubs; Spades ] in
  let ranks =
    [
      Num 2;
      Num 3;
      Num 4;
      Num 5;
      Num 6;
      Num 7;
      Num 8;
      Num 9;
      Num 10;
      Jack;
      Queen;
      King;
      Ace;
    ]
  in
  List.concat_map (fun s -> List.map (fun r -> make_card r s) ranks) suits

(** [deal] returns (cards given to player, remaining deck) *)
let deal (n : int) (deck : card list) : card list * card list =
  let rec take acc i = function
    | [] -> (List.rev acc, [])
    | lst when i = 0 -> (List.rev acc, lst)
    | x :: rest -> take (x :: acc) (i - 1) rest
  in
  take [] n deck
