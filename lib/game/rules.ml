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
  | Joker, _ -> if c.color = Black then Some BlackJoker else Some RedJoker
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

(* Effect produced when a card is played. *)
let effect_of_card (c : card) : card_effect =
  match card_type_of_card c with
  | BasicAttack -> Attack 1
  | BasicBlock -> Block
  | BasicHeal -> Heal 1
  | Special ArrowStorm -> Attack 1
  | Equipment _ | Special _ -> NoEffect

let apply_aoe_life (delta : int) (s : State.t) : State.t =
  List.fold_left
    (fun st p ->
      if Player.is_alive p then
        match State.find_player p.Player.id st with
        | None -> st
        | Some player ->
            State.update_player (Player.modify_lives delta player) st
      else st)
    s s.State.players

let apply_aoe_life_except (actor_id : int) (delta : int) (s : State.t) : State.t
    =
  List.fold_left
    (fun st p ->
      if p.Player.id <> actor_id && Player.is_alive p then
        match State.find_player p.Player.id st with
        | None -> st
        | Some player ->
            State.update_player (Player.modify_lives delta player) st
      else st)
    s s.State.players

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

let alive_other_players (actor_id : int) (s : State.t) : int list =
  List.filter_map
    (fun p ->
      if p.Player.id <> actor_id && Player.is_alive p then Some p.Player.id
      else None)
    s.State.players

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

let current_pending_target (p : State.pending_attack) : int option =
  match p.target_ids with
  | [] -> None
  | target :: _ -> Some target

let resolve_action (actor_id : int) (action : Turn.t) (target_id : int option)
    (s : State.t) : (State.t * string, string) result =
  let ( let* ) = Result.bind in
  let get_player id =
    match State.find_player id s with
    | Some pl -> Ok pl
    | None -> Error (Printf.sprintf "player %d not found" id)
  in
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
  let remove_from_discard card (state : State.t) =
    let rec aux = function
      | [] -> []
      | x :: rest -> if x = card then rest else x :: aux rest
    in
    { state with discard = aux state.discard }
  in
  let start_sayno c resolution msg =
    let waiting_on = alive_other_players actor_id s in
    let s' =
      State.apply_card actor_id c s
      |> State.set_pending_sayno actor_id c resolution waiting_on
    in
    match s'.State.pending_sayno with
    | Some p when p.State.waiting_on = [] -> Ok (State.resolve_sayno s', msg)
    | _ -> Ok (s', msg)
  in
  let rec handle_normal_turn () =
    match action with
    | Turn.Play c -> (
        match card_type_of_card c with
        | Special SayNo -> Error "Say No can only be played as a response."
        | Special Reversify ->
            Error "Reversify can only be played as a response."
        | Special Chaos ->
            if target_id <> None then Error "Chaos does not take a target."
            else
              let targets = alive_other_players actor_id s @ [ actor_id ] in
              let s' =
                State.apply_card actor_id c s
                |> State.set_pending_targets actor_id targets 1 State.ByAttack
                |> fun st ->
                { st with State.attacks_used = st.State.attacks_used + 1 }
              in
              Ok
                ( s',
                  Printf.sprintf
                    "Player %d played Chaos: all players may block with an \
                     Attack or take 1 damage."
                    actor_id )
        | Special ArrowStorm ->
            if target_id <> None then Error "ArrowStorm does not take a target."
            else
              let targets = alive_other_players actor_id s in
              if targets = [] then Error "ArrowStorm has no valid targets."
              else
                let s' =
                  State.apply_card actor_id c s
                  |> State.set_pending_targets actor_id targets 1 State.ByBlock
                  |> fun st ->
                  { st with State.attacks_used = st.State.attacks_used + 1 }
                in
                Ok
                  ( s',
                    Printf.sprintf
                      "Player %d played Arrowstorm: all other players may \
                       block or take 1 damage."
                      actor_id )
        | Special GarbageDisposal -> (
            match s.State.discard with
            | [] -> Error "No cards in discard pile to take."
            | top :: _ -> (
                let s' = remove_from_discard top s in
                let s'' = State.apply_card actor_id c s' in
                match State.find_player actor_id s'' with
                | None ->
                    Ok
                      ( s'',
                        Printf.sprintf "Player %d played Garbage Disposal."
                          actor_id )
                | Some player ->
                    let player' = Player.force_add top player in
                    let s''' = State.update_player player' s'' in
                    Ok
                      ( s''',
                        Printf.sprintf
                          "Player %d took the top card from the discard pile!"
                          actor_id )))
        | Special TwoToMax ->
            let* actor = get_player actor_id in
            let partner = other_twotomax_card c in
            if not (List.mem partner actor.Player.hand) then
              Error
                "You need both TwoToMax cards (9♣ and 10♣) in hand to use this \
                 effect."
            else
              start_sayno c (TwoToMax partner)
                (Printf.sprintf
                   "Player %d played TwoToMax. Waiting for Say No responses."
                   actor_id)
        | Special DeadMansGamble ->
            let holders =
              List.filter_map
                (fun p ->
                  if
                    p.Player.id <> actor_id
                    && List.mem (dmg_partner_card c) p.Player.hand
                  then Some p.Player.id
                  else None)
                s.State.players
            in
            start_sayno c
              (DeadMansGamble (c, holders))
              (Printf.sprintf
                 "Player %d played Dead Man's Gamble. Waiting for Say No \
                  responses."
                 actor_id)
        | Special Diplomacy ->
            start_sayno c (Diplomacy [])
              (Printf.sprintf
                 "Player %d played Diplomacy. Waiting for responses." actor_id)
        | Special BlackJoker ->
            let s' =
              State.apply_card actor_id c s
              |> apply_aoe_life (-1) |> State.onto_discard c
              |> State.check_game_over
            in
            Ok
              ( s',
                Printf.sprintf
                  "Player %d played the black joker: all players lose 1 life."
                  actor_id )
        | Special RedJoker ->
            let s' =
              State.apply_card actor_id c s
              |> apply_aoe_life 1 |> State.onto_discard c
              |> State.check_game_over
            in
            Ok
              ( s',
                Printf.sprintf
                  "Player %d played the red joker: all players gain 1 life."
                  actor_id )
        | _ -> (
            match effect_of_card c with
            | Attack dmg ->
                execute_attack actor_id c target_id dmg State.ByBlock
                  (fun tid ->
                    Printf.sprintf
                      "Player %d attacked player %d! Player %d must block or \
                       pass."
                      actor_id tid tid)
                  s
            | Heal amt ->
                start_sayno c (Heal amt)
                  (Printf.sprintf
                     "Player %d played a heal. Waiting for Say No responses."
                     actor_id)
            | Block -> Error "No attack is pending — nothing to block."
            | NoEffect -> Error "That card has no effect yet."))
    | Turn.Discard c ->
        Ok
          ( State.apply_card actor_id c s,
            Printf.sprintf "Player %d discarded a card." actor_id )
    | Turn.Pass -> Ok (s, Printf.sprintf "Player %d passed." actor_id)
  in
  let rec handle_sayno_window () =
    match s.State.pending_sayno with
    | Some p when p.State.waiting_on = [] ->
        Ok (State.resolve_sayno s, "Pending Say No resolved.")
    | Some p when List.mem actor_id p.State.waiting_on -> (
        match action with
        | Turn.Play c ->
            if card_type_of_card c = Special Reversify then
              let s' =
                State.apply_card actor_id c s |> fun st ->
                { st with State.pending_sayno = None }
              in
              let s'' =
                match State.find_player actor_id s' with
                | None -> s'
                | Some responder ->
                    let s'' = remove_from_discard p.State.source_card s' in
                    State.update_player
                      (Player.force_add p.State.source_card responder)
                      s''
              in
              Ok
                ( s'',
                  Printf.sprintf
                    "Player %d reversed the action and took the card!" actor_id
                )
            else if
              p.State.resolution = Diplomacy []
              ||
              match p.State.resolution with
              | Diplomacy _ -> true
              | _ -> false
            then
              if card_type_of_card c = Special SayNo then
                let s' =
                  State.apply_card actor_id c s |> fun st ->
                  { st with State.pending_sayno = None }
                in
                Ok (s', Printf.sprintf "Player %d said no!" actor_id)
              else
                let s' =
                  State.apply_card actor_id c s
                  |> State.diplomacy_join actor_id c
                in
                match s'.State.pending_sayno with
                | Some p when p.State.waiting_on = [] ->
                    Ok
                      ( State.resolve_sayno s',
                        Printf.sprintf
                          "Player %d joined Diplomacy and the window closed."
                          actor_id )
                | _ ->
                    Ok
                      (s', Printf.sprintf "Player %d joined Diplomacy." actor_id)
            else if card_type_of_card c = Special SayNo then
              let s' =
                State.apply_card actor_id c s |> fun st ->
                { st with State.pending_sayno = None }
              in
              Ok (s', Printf.sprintf "Player %d said no!" actor_id)
            else
              Error
                "You can only play Say No, Reversify, or pass during this \
                 response."
        | Turn.Pass -> (
            let s' = State.sayno_respond actor_id false s in
            match s'.State.pending_sayno with
            | Some p when p.State.waiting_on = [] ->
                Ok
                  ( State.resolve_sayno s',
                    Printf.sprintf "Player %d passed on Say No." actor_id )
            | _ -> Ok (s', Printf.sprintf "Player %d passed on Say No." actor_id)
            )
        | Turn.Discard _ -> Error "Cannot discard during a Say No response.")
    | Some p ->
        Error
          (Printf.sprintf "Waiting for player %d to respond to Say No."
             p.State.source_id)
    | None -> handle_attack_or_turn ()
  and handle_attack_or_turn () =
    match s.State.pending with
    | Some p -> (
        match current_pending_target p with
        | Some target when target = actor_id -> (
            match action with
            | Turn.Play c ->
                if can_counter (effect_of_card c) p.State.block_with then
                  let s' =
                    State.apply_card actor_id c s |> State.clear_pending
                  in
                  Ok
                    (s', Printf.sprintf "Player %d blocked the attack!" actor_id)
                else
                  Error
                    (Printf.sprintf
                       "You can only play a %s card in response to this \
                        attack, or Pass to take the damage."
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
                Error
                  "Cannot discard while an attack is pending — block or pass.")
        | Some target ->
            Error
              (Printf.sprintf "Waiting for player %d to respond to the attack."
                 target)
        | None -> Error "Invalid pending attack target list.")
    | None -> handle_normal_turn ()
  in
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
  | None -> handle_sayno_window ()
