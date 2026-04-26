type suit =
  | Hearts
  | Diamonds
  | Clubs
  | Spades

type rank =
  | Num of int
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
  | Random50
  | UnlimitedAttack
  | BlockHealReverse
  | Unblockable

and special_type =
  | Chaos
  | ArrowStorm
  | GarbageDisposal
  | Diplomacy
  | LifeLock
  | Reduction
  | DoubleAgent
  | DeadMansGamble
  | Silencer
  | SummonLightning
  | TwoToMax
  | Reflector
  | Steal
  | Break
  | HealOrDoubleAttack

type card_effect =
  | Attack of int
  | Block
  | Heal of int
  | NoEffect
