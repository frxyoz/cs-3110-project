open Types

(* Card type classification by suit and rank.
   Suits: ♠ = attack, ♥ = block(2-5)/heal(6+), ♣ = special, ♦ = special
   Aces are equip cards regardless of suit. *)
let card_type_of_card (c : card) : card_type =
  match c.rank with
  | Ace -> Equipment
  | Jack | Queen | King -> Special
  | Num _ -> (
      match c.suit with
      | Spades -> BasicAttack
      | Hearts -> (
          match c.rank with
          | Num n when n <= 5 -> BasicBlock
          | _ -> BasicHeal)
      | Clubs -> Special
      | Diamonds -> Special)

(* Effect produced when a card is played.
   Attack cards deal 1 damage. Block cards negate an incoming attack.
   Heal cards restore 1 life. All other cards produce NoEffect for now. *)
let effect_of_card (c : card) : card_effect =
  match card_type_of_card c with
  | BasicAttack -> Attack 1
  | BasicBlock -> Block
  | BasicHeal -> Heal 1
  | Equipment | Special -> NoEffect
