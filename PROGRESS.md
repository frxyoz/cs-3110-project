# Progress Log

## 2026-05-02 (Olric)

- implemented equipment cards (Ace cards)
  - added `equips : equipment_type list` field to `Player.t` with `has_equip`, `add_equip`, `remove_equip` helpers
  - added `Equip of equipment_type` to `State.pending_sayno_effect`; equip cards now go through the interception window like other non-attack cards before resolving
  - **UnlimitedAttack (A♠)**: bypasses the 1-attack-per-round limit in `execute_attack`
  - **BlockHealReverse (A♥)**: playing a Heal card while under attack counts as a Block; playing a Block card on your turn acts as a Heal (+1 life)
  - **Unblockable (A♣)**: when the attacker has this equipped, the defender cannot play a block card — they must Pass (or let their 50/50 activate)
  - **Random50 (A♦)**: when the defender Passes on an attack, there is a 50% chance the equip absorbs the hit instead
  - Unblockable + Random50 interact correctly: defender cannot block but 50/50 still activates on Pass
  - `Break` and `Steal` now include equipped cards in the random selection pool (equips can be broken/stolen)
  - fixed swapped Ace of Clubs / Ace of Diamonds descriptions in the GUI client

## 2026-04-30 (Isabella)

- fixed twotomax to discard both cards when played
- implemented reflector and sacrifice
- fixed turn error. passes on special cards or passes as a response to a card are no longer counted as a turn for a player
- implemented break and steal. \*\*this must be updated when equips are implemented because the player is able to choose which equip to break or steal. as of now a random card is broken/stolen
- implemented healordouble attack. all face cards are done for now

## 2026-04-30 (Isabella)

- fixed arrowstorm, chaos, and diplomacy
- implemented reversify, garbage disposal, and draw2

## 2026-04-28 (Tran)

### Done

- Refactored `types.ml` / `types.mli`: fixed special card type mapping, added all `special_type` variants, improved readability
- Extracted `apply_card` / `onto_discard` into `State` (moved out of `Rules`), fixed `get_player` error handling
- Added `block_type` (`ByBlock` | `ByAttack`) to `State.pending_attack` so Chaos (2♣) and ArrowStorm (3♣) can require different counters
- Added `pending_dmg` to `State.t` to track in-flight Dead Man's Gamble waiting for partner holders to respond
- Implemented `resolve_action` logic in `Rules` for:
  - **ArrowStorm (3♣)** — sets `ByBlock` pending (blockable by a Block card)
  - **Chaos (2♣)** — sets `ByAttack` pending (countered by an Attack card, not a Block)
  - **TwoToMax (9♣ / 10♣)** — requires both cards in hand; auto-discards both and raises `max_lives` by 1
  - **DeadMansGamble (7♣ / 8♣)** — if no partner holders: immediate +1 life; if holders exist: sets `pending_dmg`, holders must respond with partner card (−1 life to actor) or pass (no trigger); multi-holder support with `any_triggered` flag
  - **SayNo (2♦)** — opens a response window that negates eligible cards
  - **Diplomacy (4♦)** — lets other players join with a card to gain 1 life, then resolves the exchange
  - **Draw2 (5♦)** — opens a Say No window after the draw effect is played
- Implemented `resolve_action` for **Jokers**: Black Joker (AOE −1) and Red Joker (AOE +1)
- Added 153 unit tests (all passing) covering Player, Deck, State, and the above Rules logic

### What is NOT yet implemented (pick up from here)

- IMPLEMENT ACES NEXT! - other things are dependent on this. scroll down for more in depth description of ace functions
- or IMPLEMENT JUDGEMENT!
- have the first person who joins start the game
- more cohesive testing w bisect
- documentation for each function
- `resolve_action` for remaining **diamonds specials**: Silencer (6♦), DoubleAgent (7♦), SummonLightning (8♦). These all require judgement
- `resolve_action` for **clubs 4–6**: LifeLock (5♣), Reduction (6♣)(aces should be implemented before reduction)
- **Equipment resolve**: equipping Aces, UnlimitedAttack bypassing `attacks_used`, BlockHealReverse swapping block↔heal, Unblockable preventing instant response, Random50 50/50 flip
- **Judgment phase** logic: flip top card, red/black determines if card goes through
- **Draw phase** tests (`do_draw_phase` exists but is untested)
- **Discard phase** logic and tests
- **Turn rotation** and round-start logic (who goes first, rotating each round)
- **Life Lock** interaction: tracking linked players, breaking when they attack each other
- **Server / game loop** wiring: nothing connects `resolve_action` to actual player I/O yet

ace functions:
implement equipment cards. when an equipment card is played it can be intercepted. otherwise once the card is equipped, the player has the ability of the card gives until the card gets removed. This means the effect of the card may last for multiple rounds. the card may get removed if someone steals or breaks it. There are 4 equip types. when UnimitedAttack is equipped, the player does not have the 1 attack per round limit. The player may choose to attack multiple times in a round without getting an error. when BlockHealReverse is played the player may use blocks as heals and heals as blocks interchangeably. When 50/50 is equipped, if the player with the card equipped gets attack they may choose to block as normal or pass, however if they pass there is only a 50% chance that the attack goes through and there is a 50% chance that the equip blocks the attack. when unblockable is played, if the equipped player attacks someone, the target does not have the option to block the attack will always go through unless the player has 50/50 equipped then there is a 50% chance the attack goes through.
