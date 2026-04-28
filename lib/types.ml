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

type card_type =
  | BasicAttack
  | BasicBlock
  | BasicHeal
  | Equipment of equipment_type
  | Special of special_type

and equipment_type =
  | Random50 (* 50/50 — half chance attack passes *)
  | UnlimitedAttack (* No 1-attack limit *)
  | BlockHealReverse
  | Unblockable

and special_type =
  | Chaos (* 2♣: -1 life, blockable by attack *)
  | ArrowStorm (* 3♣: -1 life, blockable by block *)
  | GarbageDisposal (* 4♣: top discard → hand *)
  | LifeLock (* 5♣: shared life pool *)
  | Reduction (* 6♣: target discards all non-basics *)
  | DeadMansGamble (* 7♣/8♣: +1 life; pair → -1 life instead *)
  | TwoToMax (* 9♣/10♣: collect both → +1 max life *)
  | SayNo (* 2♦: negate a card *)
  | Reversify (* 3♦: reverse an effect *)
  | Diplomacy (* 4♦: exchange cards; gain a life *)
  | Draw2 (* 5♦: draw 2 cards *)
  | Silencer (* 6♦: judgment — black → target can't play this round *)
  | DoubleAgent (* 7♦: judgment — black → reveal hand *)
  | SummonLightning (* 8♦: judgment — black → -3 lives, hot potato if red *)
  | Reflector (* 9♦: bounce action back, -1 life *)
  | Sacrifice (* 10♦: -3 lives, +1 max lives *)
  | Steal (* Q: take card from someone's hand *)
  | Break (* J: discard a card from someone's hand; breaks equips *)
  | HealOrDoubleAttack (* K: +1 life OR attack same person twice *)

type card_effect =
  | Attack of int
  | Block
  | Heal of int
  | NoEffect
