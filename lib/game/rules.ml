open Types

(* Determine the special_type based on card rank/suit *)
let special_type_of_card (c : card) : special_type option =
  match (c.rank, c.suit) with
  | Num 2, Clubs -> Some Chaos
  | Num 3, Clubs -> Some ArrowStorm
  | Num 4, Clubs -> Some GarbageDisposal
  | Num 5, Clubs -> Some LifeLock
  | Num 6, Clubs -> Some Reduction
  | Num 7, Clubs -> Some DeadMansGamble
  | Num 8, Clubs -> Some DeadMansGamble
  | Num 9, Clubs -> Some TwoToMax
  | Num 10, Clubs -> Some TwoToMax
  | Num 2, Diamonds -> Some SayNo
  | Num 3, Diamonds -> Some Reversify
  | Num 4, Diamonds -> Some Diplomacy
  | Num 5, Diamonds -> Some Draw2
  | Num 6, Diamonds -> Some Silencer
  | Num 7, Diamonds -> Some DoubleAgent
  | Num 8, Diamonds -> Some SummonLightning
  | Num 9, Diamonds -> Some Reflector
  | Num 10, Diamonds -> Some Sacrifice
  | Jack, _ -> Some Break
  | Queen, _ -> Some Steal
  | King, _ -> Some HealOrDoubleAttack
  | _ -> None

(* Determine the equipment_type based on the Ace of each suit *)
let equipment_type_of_card (c : card) : equipment_type option =
  match c.rank with
  | Ace -> (
      match c.suit with
      | Spades -> Some UnlimitedAttack
      | Hearts -> Some BlockHealReverse
      | Clubs -> Some Unblockable
      | Diamonds -> Some Random50)
  | _ -> None

(* Card type classification by suit and rank. Suits: ♠ = attack, ♥ =
   block(2-5)/heal(6+), ♣ = special, ♦ = special. Aces are equip cards
   regardless of suit. *)
let card_type_of_card (c : card) : card_type =
  match equipment_type_of_card c with
  | Some eq_type -> Equipment eq_type
  | None -> (
      match special_type_of_card c with
      | Some sp_type -> Special sp_type
      | None -> (
          match c.suit with
          | Spades -> BasicAttack
          | Hearts -> (
              match c.rank with
              | Num n when n <= 5 -> BasicBlock
              | _ -> BasicHeal)
          | Clubs | Diamonds -> BasicAttack (* fallback *)))

(* Effect produced when a card is played. Chaos is handled separately in
   resolve_action (needs ByAttack pending). ArrowStorm is a normal attack. *)
let effect_of_card (c : card) : card_effect =
  match card_type_of_card c with
  | BasicAttack -> Attack 1
  | BasicBlock -> Block
  | BasicHeal -> Heal 1
  | Special ArrowStorm -> Attack 1
  | Equipment _ | Special _ -> NoEffect

(* Returns the partner TwoToMax card: 9♣ ↔ 10♣ *)
let other_twotomax_card (c : card) : card =
  match c.rank with
  | Num 9 -> { c with rank = Num 10 }
  | Num 10 -> { c with rank = Num 9 }
  | _ -> assert false

(* Returns the partner Dead Man's Gamble card: 7♣ ↔ 8♣ *)
let dmg_partner_card (c : card) : card =
  match c.rank with
  | Num 7 -> { c with rank = Num 8 }
  | Num 8 -> { c with rank = Num 7 }
  | _ -> assert false

(* True when [eff] is a valid counter for the given [block_with] requirement. *)
let can_counter (eff : card_effect) (bw : State.block_type) : bool =
  match bw with
  | State.ByBlock -> eff = Block
  | State.ByAttack -> (
      match eff with
      | Attack _ -> true
      | _ -> false)

let counter_name : State.block_type -> string = function
  | State.ByBlock -> "Block"
  | State.ByAttack -> "Attack"

(* Apply [f] to player [id] in [s]. No-op if the player is not found. *)
let modify_actor (id : int) (f : Player.t -> Player.t) (s : State.t) : State.t =
  match State.find_player id s with
  | None -> s
  | Some a -> State.update_player (f a) s

(* Check the attack limit and target, then create a pending attack. [msg tid]
   formats the success message given the resolved target id. *)
let execute_attack (actor_id : int) (c : card) (target_id : int option)
    (dmg : int) (block_with : State.block_type) (msg : int -> string)
    (s : State.t) : (State.t * string, string) result =
  if s.State.attacks_used >= 1 then
    Error "You have already used your one attack this round."
  else
    match target_id with
    | None -> Error "An attack requires a target."
    | Some tid -> (
        match State.find_player tid s with
        | None -> Error "Target player not found."
        | Some _ ->
            let s' =
              State.apply_card actor_id c s
              |> State.set_pending actor_id tid dmg block_with
              |> fun st ->
              { st with State.attacks_used = st.State.attacks_used + 1 }
            in
            Ok (s', msg tid))

(* resolve_action actor_id action target_id state Resolves one player action and
   returns (new_state, message) or an error.

   Priority order for pending state checks: 1. pending_dmg — out-of-turn DMG
   response window 2. pending — attack response (target must block or pass) 3.
   None — normal turn *)
let resolve_action (actor_id : int) (action : Turn.t) (target_id : int option)
    (s : State.t) : (State.t * string, string) result =
  let ( let* ) = Result.bind in
  let get_player id =
    match State.find_player id s with
    | Some pl -> Ok pl
    | None -> Error (Printf.sprintf "player %d not found" id)
  in
  (* Once all waiting_on players have responded, apply the life change. *)
  let finalize_dmg msg s' =
    match s'.State.pending_dmg with
    | Some p when p.State.waiting_on = [] ->
        let s'' = State.resolve_dmg s' in
        let outcome = if p.State.any_triggered then "lost" else "gained" in
        Ok
          ( s'',
            Printf.sprintf "%s Dead Man's Gamble resolved: player %d %s 1 life."
              msg p.State.dmg_actor_id outcome )
    | _ -> Ok (s', msg)
  in
  (* ── Dead Man's Gamble pending: out-of-turn response window ── *)
  match s.State.pending_dmg with
  | Some pdmg when List.mem actor_id pdmg.State.waiting_on -> (
      let partner_card = dmg_partner_card pdmg.State.played_card in
      match action with
      | Turn.Play c when c = partner_card ->
          State.apply_card actor_id c s
          |> State.dmg_respond actor_id true
          |> finalize_dmg
               (Printf.sprintf "Player %d responded to Dead Man's Gamble!"
                  actor_id)
      | Turn.Play _ ->
          Error "You can only play your Dead Man's Gamble card or pass."
      | Turn.Pass ->
          State.dmg_respond actor_id false s
          |> finalize_dmg
               (Printf.sprintf "Player %d passed on Dead Man's Gamble." actor_id)
      | Turn.Discard _ ->
          Error "Cannot discard during a Dead Man's Gamble response.")
  | Some _ -> Error "Waiting for Dead Man's Gamble responses."
  | None -> (
      (* ── Pending attack: only the designated target may respond ── *)
      match s.State.pending with
      | Some p when p.State.target_id = actor_id -> (
          match action with
          | Turn.Play c ->
              if can_counter (effect_of_card c) p.State.block_with then
                let s' = State.apply_card actor_id c s |> State.clear_pending in
                Ok (s', Printf.sprintf "Player %d blocked the attack!" actor_id)
              else
                Error
                  (Printf.sprintf
                     "You can only play a %s card in response to this attack, \
                      or Pass to take the damage."
                     (counter_name p.State.block_with))
          | Turn.Pass ->
              let* target = get_player actor_id in
              let target' = Player.modify_lives (-p.State.damage) target in
              let s' =
                s
                |> State.update_player target'
                |> State.clear_pending |> State.check_game_over
              in
              Ok
                ( s',
                  Printf.sprintf
                    "Player %d took %d damage! (%d lives remaining)" actor_id
                    p.State.damage target'.Player.lives )
          | Turn.Discard _ ->
              Error "Cannot discard while an attack is pending — block or pass."
          )
      | Some p ->
          Error
            (Printf.sprintf "Waiting for player %d to respond to the attack."
               p.State.target_id)
      (* ── Normal turn ── *)
      | None -> (
          match action with
          | Turn.Play c -> (
              match card_type_of_card c with
              | Special TwoToMax ->
                  let* actor = get_player actor_id in
                  let partner = other_twotomax_card c in
                  if not (List.mem partner actor.Player.hand) then
                    Error
                      "You need both TwoToMax cards (9♣ and 10♣) in hand to \
                       use this effect."
                  else
                    let s' =
                      State.apply_card actor_id c s
                      |> State.apply_card actor_id partner
                      |> modify_actor actor_id (Player.set_max_lives 1)
                    in
                    Ok
                      ( s',
                        Printf.sprintf
                          "Player %d collected both TwoToMax cards — max lives \
                           raised to %d!"
                          actor_id
                          (actor.Player.max_lives + 1) )
              | Special DeadMansGamble ->
                  let partner = dmg_partner_card c in
                  let holders =
                    List.filter_map
                      (fun p ->
                        if
                          p.Player.id <> actor_id
                          && List.mem partner p.Player.hand
                        then Some p.Player.id
                        else None)
                      s.State.players
                  in
                  let s' = State.apply_card actor_id c s in
                  if holders = [] then
                    Ok
                      ( modify_actor actor_id (Player.modify_lives 1) s',
                        Printf.sprintf
                          "Player %d played Dead Man's Gamble and gained 1 \
                           life! (no one holds the partner card)"
                          actor_id )
                  else
                    Ok
                      ( State.set_pending_dmg actor_id c holders s',
                        Printf.sprintf
                          "Player %d played Dead Man's Gamble! Waiting for \
                           responses from: %s."
                          actor_id
                          (String.concat ", " (List.map string_of_int holders))
                      )
              | Special Chaos ->
                  execute_attack actor_id c target_id 1 State.ByAttack
                    (fun tid ->
                      Printf.sprintf
                        "Player %d played Chaos on player %d! Player %d must \
                         respond with an Attack card or pass."
                        actor_id tid tid)
                    s
              | _ -> (
                  match effect_of_card c with
                  | Attack dmg ->
                      execute_attack actor_id c target_id dmg State.ByBlock
                        (fun tid ->
                          Printf.sprintf
                            "Player %d attacked player %d! Player %d must \
                             block or pass."
                            actor_id tid tid)
                        s
                  | Heal amt ->
                      let* actor = get_player actor_id in
                      let actor' =
                        Player.remove_from_hand c actor
                        |> Player.modify_lives amt
                      in
                      let s' =
                        s |> State.update_player actor' |> State.onto_discard c
                      in
                      Ok
                        ( s',
                          Printf.sprintf
                            "Player %d healed! (%d lives remaining)" actor_id
                            actor'.Player.lives )
                  | Block -> Error "No attack is pending — nothing to block."
                  | NoEffect -> Error "That card has no effect yet."))
          | Turn.Discard c ->
              Ok
                ( State.apply_card actor_id c s,
                  Printf.sprintf "Player %d discarded a card." actor_id )
          | Turn.Pass -> Ok (s, Printf.sprintf "Player %d passed." actor_id)))
