# Super Complicated Card Game — Rules

## Goal
Be the last player standing. When you run out of lives, you are eliminated.

---

## Setup
- One draw pile, one discard pile.
- Each player starts with **7 lives**, **7 cards**, and a **max life cap of 7**.
- The maximum number of cards you can hold equals your current life count.
- If the draw pile runs out, reshuffle the discard pile to form a new draw pile.

---

## Game Structure (each round has 4 phases)

### Phase 1 — Draw
Every player draws up to **2 cards**, but cannot exceed their current life count.
> Example: if you have 5 lives, you can hold at most 5 cards. If you have 4, you draw up to 2 (up to your cap of 5 is fine).

### Phase 2 — Judgment
Some cards require a **judgment** before their effect applies. Judgment works like this:
1. Flip the top card of the draw pile.
2. **Black card** (clubs/spades) = judgment **passes** → effect triggers.
3. **Red card** (hearts/diamonds) = judgment **fails** → no effect.
4. The flipped card goes to the discard pile.

Cards that require judgment: **Silencer**, **Double Agent**, **Summon Lightning**, and the **50/50 equip** (see Equips section).

### Phase 3 — Action
Players take turns in order (who goes first rotates each round). Each turn, a player may take **one action**:
1. **Play a card** (use its effect)
2. **Discard a card** (remove it from your hand with no effect)
3. **Pass** (do nothing)

The round ends when **no one can play anything** (all players pass consecutively).

**Attack limit:** Each player may use **one attack per round**, unless they have the Unlimited Attack equip.

**Out-of-turn responses:** If a card played by another player affects you directly (e.g. an attack), you may instantly respond **without spending your turn**. Responses are limited — see each card's description.

**Exceeding hand size:** During the action phase you may temporarily hold more cards than your life count. This is allowed.

### Phase 4 — Discard
After the action round, discard down to your life count. You choose which cards to keep.

---

## Ready State
A player signals they are **ready** to advance to the next round. Once all players are ready, the round ends and Phase 1 begins again.

---

## Card Types

### Basic Cards
| Card | Effect |
|------|--------|
| **Attack** (number ♠) | Deal 1 damage to target. Target may block or take the hit. |
| **Block** (2–5 ♥) | Cancel an incoming attack. Can only be played as a response to an attack. |
| **Heal** (6–10 ♥) | Gain 1 life (up to your max). |

### Equip Cards (Aces)
Equips are permanent until removed. You may only have each equip once.

| Card | Equip | Effect |
|------|-------|--------|
| A♣ | **50/50** | When you are attacked and choose to pass, judgment is triggered. Black = you take the damage. Red = the attack is blocked automatically. |
| A♠ | **Unlimited Attack** | The 1-attack-per-round limit does not apply to you. |
| A♥ | **Block/Heal Reverse** | You may use Block cards as Heals (and Heal cards as Blocks) at any time. |
| A♦ | **Unblockable Attacks** | Players cannot immediately respond to block your attacks. They must take the damage or rely on 50/50. |

To equip: play the Ace card. Other players may Say No to prevent you from equipping it.

### Special Cards
All special cards have the **Say No window** open when they are played (see Say No section), **except** attacks, Summon Lightning, and AoE Jokers.

---

## Special Card Reference

### ♣ Clubs (all require a target unless noted)

| Card | Name | Effect |
|------|------|--------|
| 2♣ | **Chaos** | Deal 1 damage to **all** players (including yourself). Each player can block by playing an **Attack** card (not a Block card). No target needed. |
| 3♣ | **Arrow Storm** | Deal 1 damage to all **other** players. Each can block normally with a Block card. No target needed. |
| 4♣ | **Garbage Disposal** | Take the **top card of the discard pile** into your hand. |
| 5♣ | **Life Lock** | Target another player. You two are now **life-locked**: any life gained or lost by one is also gained or lost by the other. The lock breaks if either of you attacks the other — when it breaks, **both** players take 1 damage. |
| 6♣ | **Reduction** | Target another player. They must immediately discard **all non-basic cards** (everything except number ♠ attacks, ♥ blocks, and ♥ heals). Aces, J, Q, K, Jokers, and all ♣/♦ cards are removed. |
| 7♣ / 8♣ | **Dead Man's Gamble** | Play one half of the pair. If any other player holds the **matching card** (7♣ ↔ 8♣) and plays it in response, the original player **loses 1 life**. If no one responds, the original player **gains 1 life**. The response window is open like a Say No window. |
| 9♣ / 10♣ | **Two to Max** | You must hold **both** cards (9♣ and 10♣) in your hand. Play one; the other is automatically consumed. Your **maximum life cap increases by 1**. Other players may Say No to stop this. |

### ♦ Diamonds

| Card | Name | Effect |
|------|------|--------|
| 2♦ | **Say No** | Played as a **response** to cancel another player's non-attack card. Does **not** work on attacks or Summon Lightning. |
| 3♦ | **Reversify** | Played as a **response** to a non-attack card. The card's effect is **cancelled**, the Reversify player takes the source card into their hand. |
| 4♦ | **Diplomacy** | Open a response window. Any player may respond by playing any card to **join** the diplomacy. All participants (you + joiners) each gain 1 life and **exchange cards cyclically** (each receives the card played by the next joiner; you receive the last joiner's card). Players may Say No to opt out. |
| 5♦ | **Draw 2** | Draw 2 cards immediately. You may discard down to your life count at end of round as normal. |
| 6♦ | **Silencer** | Target another player. Opens a Say No window first. If not cancelled: judgment is triggered. **Black = target cannot play any cards for the rest of this round.** Red = no effect. |
| 7♦ | **Double Agent** | Target another player. Opens a Say No window. If not cancelled: judgment is triggered. **Black = the target must reveal their entire hand to you.** Red = no effect. |
| 8♦ | **Summon Lightning** | Target another player. **No Say No window — cannot be cancelled.** Judgment is triggered immediately. **Black = target loses 3 lives.** Red = the lightning card is passed to the next alive player (hot potato). That player now holds it and must trigger judgment again when they play it. |
| 9♦ | **Reflector** | Played as a **response** to a non-attack card. The card's effect goes through normally, but the Reflector player also **loses 1 life**. |
| 10♦ | **Sacrifice** | Lose 3 lives. Your **maximum life cap increases by 1**. Other players may Say No. |

### Face Cards (all suits behave the same)

| Card | Name | Effect |
|------|------|--------|
| J (all suits) | **Break** | Target another player. Randomly discard one card from their hand or remove one of their equips. Can break equipped Aces. Other players may Say No. |
| Q (all suits) | **Steal** | Target another player. Randomly take one card from their hand or one of their equips into your own hand. Other players may Say No. |
| K (all suits) | **Heal or Double Attack** | Choose: **heal yourself by 1 life**, OR spend an Attack card from your hand to **attack the same target twice** (dealing 2 damage total). The double-attack is a single pending action the target must respond to once. Other players may Say No to either use. |

### Jokers

| Card | Effect |
|------|--------|
| Black Joker (♣/♠) | All players lose 1 life immediately. No response window. |
| Red Joker (♥/♦) | All players gain 1 life immediately. No response window. |

---

## Say No Window
When most non-attack, non-Lightning special cards are played, a **Say No window** opens before the effect resolves. In this window, **each other player** may respond with one of:

| Response | Effect |
|----------|--------|
| **Say No (2♦)** | Cancel the card entirely. The source card is discarded. |
| **Reversify (3♦)** | Cancel the card AND take the source card into your hand. |
| **Reflector (9♦)** | The effect still resolves, but you take 1 life of damage. |
| **Pass** | You have no objection; remove yourself from the waiting list. |

Once all other players have responded (or passed), the effect resolves — unless it was cancelled by Say No or Reversify.

**Say No does NOT work on:**
- Regular attacks
- Summon Lightning (8♦)

---

## Attack Sequence
1. Player A plays an **Attack** card (or Chaos/Arrow Storm/King double-attack) targeting Player B.
2. If Player A has **Unblockable** equipped, Player B cannot block.
3. Player B may respond:
   - **Play a Block card** → attack is cancelled.
   - **Pass** → take the damage (−1 life). If Player B has **50/50**, judgment fires: red = blocked, black = damage lands.
4. If Player B has a **life-lock partner** (Life Lock), and they take damage, their partner takes the same damage simultaneously.

---

## Life Lock Details
- Playing **5♣** on a target creates a life-lock pair between you and them.
- While locked: life gains and losses affect **both** players equally.
- If either locked player attacks the other: the **lock breaks immediately**, and **both** take 1 damage (in addition to the normal attack being replaced by the lock-break).
- The lock persists until broken by a mutual attack or one player is eliminated.

---

## Summon Lightning (Hot Potato) Details
- Play 8♦, target a player. No Say No.
- Flip judgment card:
  - **Black**: target loses 3 lives. Lightning card goes to discard. Done.
  - **Red**: the 8♦ card is placed in the **next alive player's hand**. On their turn, they must eventually play it, triggering judgment again. This repeats until the lightning strikes (black) or only one player remains.

---

## Win Condition
The last player with at least 1 life wins. If multiple players reach 0 lives simultaneously, the game is a **draw**.
