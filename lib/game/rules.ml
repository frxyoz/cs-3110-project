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
   block(2-5)/heal(6+), ♣ = special, ♦ = special Aces are equip cards regardless
   of suit. *)
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

(* Effect produced when a card is played. Attack cards deal 1 damage. Block
   cards negate an incoming attack. Heal cards restore 1 life. All other cards
   produce NoEffect for now. *)
let effect_of_card (c : card) : card_effect =
  match card_type_of_card c with
  | BasicAttack -> Attack 1
  | BasicBlock -> Block
  | BasicHeal -> Heal 1
  | Equipment _ | Special _ -> NoEffect

(* Remove the first occurrence of a card from a list. *)
let remove_card (c : card) (hand : card list) : card list =
  let rec aux acc = function
    | [] -> List.rev acc
    | h :: t -> if h = c then List.rev acc @ t else aux (h :: acc) t
  in
  aux [] hand

(* resolve_action actor_id action target_id state Resolves one player action and
   returns (new_state, message) or an error.

   The sub-state machine for Action phase: state.pending = None → it is actor's
   normal turn (Play/Discard/Pass) state.pending = Some → an attack is live;
   only the target may respond with a Block or by taking the damage (Pass) *)
let resolve_action (actor_id : int) (action : Turn.t) (target_id : int option)
    (s : State.t) : (State.t * string, string) result =
  let get_player id =
    match State.find_player id s with
    | Some pl -> pl
    | None -> failwith (Printf.sprintf "player %d not found" id)
  in
  let spend c pl = { pl with Player.hand = remove_card c pl.Player.hand } in
  let onto_discard c st = { st with State.discard = c :: st.State.discard } in
  let apply_card id c st =
    let actor = get_player id in
    st |> State.update_player (spend c actor) |> onto_discard c
  in
  match s.State.pending with
  (* ── Pending attack: only the designated target may respond ── *)
  | Some p when p.State.target_id = actor_id -> (
      match action with
      | Turn.Play c -> (
          match effect_of_card c with
          | Block ->
              let s' = apply_card actor_id c s |> State.clear_pending in
              Ok (s', Printf.sprintf "Player %d blocked the attack!" actor_id)
          | _ ->
              Error
                "You can only play a Block card in response to an attack, or \
                 Pass to take the damage.")
      | Turn.Pass ->
          let target = get_player actor_id in
          let target' = Player.modify_lives (-p.State.damage) target in
          let s' =
            s
            |> State.update_player target'
            |> State.clear_pending |> State.check_game_over
          in
          Ok
            ( s',
              Printf.sprintf "Player %d took %d damage! (%d lives remaining)"
                actor_id p.State.damage target'.Player.lives )
      | Turn.Discard _ ->
          Error "Cannot discard while an attack is pending — block or pass.")
  | Some p ->
      Error
        (Printf.sprintf "Waiting for player %d to respond to the attack."
           p.State.target_id)
  (* ── Normal turn ── *)
  | None -> (
      match action with
      | Turn.Play c ->
          begin match effect_of_card c with
          | Attack dmg ->
              if s.State.attacks_used >= 1 then
                Error "You have already used your one attack this round."
              else
                begin match target_id with
                | None -> Error "An attack requires a target."
                | Some tid ->
                    if State.find_player tid s = None then
                      Error "Target player not found."
                    else
                      let s' =
                        apply_card actor_id c s
                        |> State.set_pending actor_id tid dmg
                        |> fun st ->
                        {
                          st with
                          State.attacks_used = st.State.attacks_used + 1;
                        }
                      in
                      Ok
                        ( s',
                          Printf.sprintf
                            "Player %d attacked player %d! Player %d must \
                             block or pass."
                            actor_id tid tid )
                end
          | Heal amt ->
              let actor' =
                get_player actor_id |> spend c |> Player.modify_lives amt
              in
              let s' = s |> State.update_player actor' |> onto_discard c in
              Ok
                ( s',
                  Printf.sprintf "Player %d healed! (%d lives remaining)"
                    actor_id actor'.Player.lives )
          | Block -> Error "No attack is pending — nothing to block."
          | NoEffect -> Error "That card has no effect yet."
          end
      | Turn.Discard c ->
          let s' = apply_card actor_id c s in
          Ok (s', Printf.sprintf "Player %d discarded a card." actor_id)
      | Turn.Pass -> Ok (s, Printf.sprintf "Player %d passed." actor_id))
