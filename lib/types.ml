(* the card type, suit, rank, color *)

type suit =
  | Hearts
  | Diamonds
  | Clubs
  | Spades

type rank =
  | Num of int (* 2-10 *)
  | Jack
  | Queen
  | King
  | Ace

type color =
  | Red
  | Black

type card = {
  rank : rank;
  suit : suit;
  color : color;
}
