(* a player's hand, lives, identity *)
open Types

type t = {
  id : int;
  name : string;
  lives : int;
  max_lives : int;
  hand : card list;
}

let make_player (id : int) (name : string) : t =
  { id; name; lives = 7; max_lives = 7; hand = [] }

let is_alive (p : t) : bool = p.lives >= 1

(* [set_lives lives p] sets [p]'s lives to [lives], clamped to [0, max_lives] *)
let set_lives (lives : int) (p : t) : t =
  let clamped = max 0 (min p.max_lives lives) in
  { p with lives = clamped }

(* [set_max_lives amt p] adjusts [p]'s max life cap by [amt]. current lives are
   reduced to the new cap if they would exceed it *)
let set_max_lives (amt : int) (p : t) : t =
  let new_mlives = max 0 (p.max_lives + amt) in
  let new_lives = min p.lives new_mlives in
  { p with max_lives = new_mlives; lives = new_lives }

let modify_lives (amt : int) (p : t) : t = set_lives (p.lives + amt) p

(* [add_to_hand card p] adds [card] to [p]'s hand. TODO: currently enforces hand
   limit (= lives) at all times, but rules allow temporarily exceeding it during
   the action round — fix when implementing turns *)
let add_to_hand (card : card) (p : t) : t =
  if List.length p.hand >= p.lives then p else { p with hand = card :: p.hand }

let remove_from_hand (card : card) (p : t) : t =
  let rec aux = function
    | [] -> []
    | x :: rest -> if x = card then rest else x :: aux rest
  in
  { p with hand = aux p.hand }
