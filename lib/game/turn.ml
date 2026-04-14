(* Single player turn logic *)

open Types

type t =
  | Play of card
  | Discard of card
  | Pass
