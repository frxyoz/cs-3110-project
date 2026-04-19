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

(* Remove the first occurrence of a card from a list. *)
let remove_card (c : card) (hand : card list) : card list =
  let rec aux acc = function
    | [] -> List.rev acc
    | h :: t -> if h = c then List.rev acc @ t else aux (h :: acc) t
  in
  aux [] hand

(* resolve_action actor_id action target_id state
   Resolves one player action and returns (new_state, message) or an error.

   The sub-state machine for Action phase:
     state.pending = None  → it is actor's normal turn (Play/Discard/Pass)
     state.pending = Some  → an attack is live; only the target may respond
                             with a Block or by taking the damage (Pass) *)
let resolve_action (actor_id : int) (action : Turn.t) (target_id : int option)
    (s : State.t) : (State.t * string, string) result =
  (* Helpers to keep the match arms readable. *)
  let get_player id =
    match State.find_player id s with
    | Some pl -> pl
    | None -> failwith (Printf.sprintf "player %d not found" id)
  in
  let spend c pl = { pl with Player.hand = remove_card c pl.Player.hand } in
  let onto_discard c st = { st with State.discard = c :: st.State.discard } in
  match s.State.pending with
  (* ── Pending attack: only the designated target may respond ── *)
  | Some p when p.State.target_id = actor_id -> (
      match action with
      | Turn.Play c -> (
          match effect_of_card c with
          | Block ->
              let actor = get_player actor_id in
              let s' =
                s
                |> State.update_player (spend c actor)
                |> onto_discard c
                |> State.clear_pending
              in
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
            |> State.clear_pending
            |> State.check_game_over
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
          let actor = get_player actor_id in
          (* Wrap in begin..end so the inner match doesn't swallow the outer arms. *)
          begin
            match effect_of_card c with
            | Attack dmg ->
                if s.State.attacks_used >= 1 then
                  Error "You have already used your one attack this round."
                else begin
                  match target_id with
                  | None -> Error "An attack requires a target."
                  | Some tid ->
                      if State.find_player tid s = None then
                        Error "Target player not found."
                      else
                        let s' =
                          s
                          |> State.update_player (spend c actor)
                          |> onto_discard c
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
                let actor' = Player.modify_lives amt actor in
                let s' =
                  s
                  |> State.update_player (spend c actor')
                  |> onto_discard c
                in
                Ok
                  ( s',
                    Printf.sprintf "Player %d healed! (%d lives remaining)"
                      actor_id actor'.Player.lives )
            | Block -> Error "No attack is pending — nothing to block."
            | NoEffect -> Error "That card has no effect yet."
          end
      | Turn.Discard c ->
          let actor = get_player actor_id in
          let s' =
            s
            |> State.update_player (spend c actor)
            |> onto_discard c
          in
          Ok (s', Printf.sprintf "Player %d discarded a card." actor_id)
      | Turn.Pass -> Ok (s, Printf.sprintf "Player %d passed." actor_id))
