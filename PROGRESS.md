# Progress Log

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
- `resolve_action` for remaining **diamonds specials**: Reversify (3♦), Silencer (6♦), DoubleAgent (7♦), SummonLightning (8♦), Reflector (9♦), Sacrifice (10♦)
- `resolve_action` for **clubs 4–6**: GarbageDisposal (4♣), LifeLock (5♣), Reduction (6♣)
- `resolve_action` for **face cards**: Jack (Break), Queen (Steal), King (Heal or Double Attack)
- **Equipment resolve**: equipping Aces, UnlimitedAttack bypassing `attacks_used`, BlockHealReverse swapping block↔heal, Unblockable preventing instant response, Random50 50/50 flip
- **Judgment phase** logic: flip top card, red/black determines if card goes through
- **Draw phase** tests (`do_draw_phase` exists but is untested)
- **Discard phase** logic and tests
- **Turn rotation** and round-start logic (who goes first, rotating each round)
- **Life Lock** interaction: tracking linked players, breaking when they attack each other
- **Server / game loop** wiring: nothing connects `resolve_action` to actual player I/O yet
