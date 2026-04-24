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
  | Chaos (* -1 life, blockable by attack *)
  | ArrowStorm (* -1 life, blockable by block *)
  | GarbageDisposal (* Top discard → hand *)
  | Diplomacy (* Exchange cards on join *)
  | LifeLock (* Shared life pool *)
  | Reduction (* Discard non-basics *)
  | DoubleAgent (* Reveal hand if flipped *)
  | DeadMansGamble (* Pair bonus: +1 max life *)
  | Silencer (* Can't play cards this round *)
  | SummonLightning (* Judgment: 3 damage if passes *)
  | TwoToMax (* Pair bonus: +1 max life *)
  | Reflector (* Bounce action, -1 life *)
  | Steal
  | Break
  | HealOrDoubleAttack

type card_effect =
  | Attack of int
  | Block
  | Heal of int
  | NoEffect
