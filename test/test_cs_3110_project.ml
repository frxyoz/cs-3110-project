open OUnit2
open Cs_3110_project

(* ── helpers ── *)

let add_or_fail p s =
  match State.add_player p s with
  | Ok s' -> s'
  | Error m -> assert_failure m

let ok_or_fail = function
  | Ok x -> x
  | Error m -> assert_failure ("expected Ok, got Error: " ^ m)

let mk_card r s =
  let color =
    match s with
    | Types.Hearts | Types.Diamonds -> Types.Red
    | _ -> Types.Black
  in
  { Types.rank = r; suit = s; color }

(* Replace a player's hand without going through add_to_hand limits. *)
let set_hand pid cards s =
  match State.find_player pid s with
  | None -> s
  | Some p -> State.update_player { p with Player.hand = cards } s

let set_player_lives pid n s =
  match State.find_player pid s with
  | None -> s
  | Some p -> State.update_player (Player.set_lives n p) s

let get_player pid s =
  match State.find_player pid s with
  | Some p -> p
  | None -> assert_failure (Printf.sprintf "player %d not found" pid)

(* Two-player game, started, with controlled hands. *)
let two_player_game () =
  let s = State.make () in
  let s = add_or_fail (Player.make_player 1 "Alice") s in
  let s = add_or_fail (Player.make_player 2 "Bob") s in
  State.start_game s

(* Place a pending attack on the state directly. *)
let with_pending aid tid dmg s =
  {
    s with
    State.pending =
      Some { State.attacker_id = aid; target_id = tid; damage = dmg };
  }

(* ── common cards ── *)

(* basic *)
let atk = mk_card (Types.Num 2) Types.Spades (* BasicAttack *)
let blk = mk_card (Types.Num 2) Types.Hearts (* BasicBlock *)
let blk5 = mk_card (Types.Num 5) Types.Hearts (* BasicBlock — highest block rank *)
let heal = mk_card (Types.Num 6) Types.Hearts (* BasicHeal — lowest heal rank *)
let heal10 = mk_card (Types.Num 10) Types.Hearts (* BasicHeal *)

(* clubs specials *)
let club2 = mk_card (Types.Num 2) Types.Clubs
let club3 = mk_card (Types.Num 3) Types.Clubs
let club4 = mk_card (Types.Num 4) Types.Clubs
let club5 = mk_card (Types.Num 5) Types.Clubs
let club6 = mk_card (Types.Num 6) Types.Clubs
let club7 = mk_card (Types.Num 7) Types.Clubs
let club8 = mk_card (Types.Num 8) Types.Clubs
let club9 = mk_card (Types.Num 9) Types.Clubs
let club10 = mk_card (Types.Num 10) Types.Clubs

(* diamonds specials *)
let diam2 = mk_card (Types.Num 2) Types.Diamonds
let diam3 = mk_card (Types.Num 3) Types.Diamonds
let diam4 = mk_card (Types.Num 4) Types.Diamonds
let diam5 = mk_card (Types.Num 5) Types.Diamonds
let diam6 = mk_card (Types.Num 6) Types.Diamonds
let diam7 = mk_card (Types.Num 7) Types.Diamonds
let diam8 = mk_card (Types.Num 8) Types.Diamonds
let diam9 = mk_card (Types.Num 9) Types.Diamonds
let diam10 = mk_card (Types.Num 10) Types.Diamonds

(* face cards — suit shouldn't affect classification *)
let jack = mk_card Types.Jack Types.Spades
let queen = mk_card Types.Queen Types.Hearts
let king = mk_card Types.King Types.Clubs

(* aces — equips *)
let ace_s = mk_card Types.Ace Types.Spades
let ace_h = mk_card Types.Ace Types.Hearts
let ace_c = mk_card Types.Ace Types.Clubs
let ace_d = mk_card Types.Ace Types.Diamonds

let tests =
  "test suite"
  >::: [
         (* ── Player ── *)
         ( "make player starts with 7 lives" >:: fun _ ->
           let p = Player.make_player 1 "Alice" in
           assert_equal ~printer:string_of_int 7 p.Player.lives );
         ( "make player starts with max lives 7" >:: fun _ ->
           let p = Player.make_player 1 "Alice" in
           assert_equal ~printer:string_of_int 7 p.Player.max_lives );
         ( "make player starts with empty hand" >:: fun _ ->
           let p = Player.make_player 1 "Alice" in
           assert_equal ~printer:string_of_int 0 (List.length p.Player.hand) );
         (* set_lives *)
         ( "set lives updates to given value" >:: fun _ ->
           let p = Player.make_player 1 "Alice" in
           let p' = Player.set_lives 5 p in
           assert_equal ~printer:string_of_int 5 p'.Player.lives );
         ( "set lives clamps negative to zero" >:: fun _ ->
           let p = Player.make_player 1 "Alice" in
           let p' = Player.set_lives (-3) p in
           assert_equal ~printer:string_of_int 0 p'.Player.lives );
         ( "set lives cannot exceed max lives" >:: fun _ ->
           let p = Player.make_player 1 "Alice" in
           let p' = Player.set_lives 99 p in
           assert_equal ~printer:string_of_int 7 p'.Player.lives );
         (* modify_lives *)
         ( "modify lives increases by amount" >:: fun _ ->
           let p = Player.make_player 1 "Alice" in
           let p' = Player.set_lives 4 p |> Player.modify_lives 2 in
           assert_equal ~printer:string_of_int 6 p'.Player.lives );
         ( "modify lives decreases by amount" >:: fun _ ->
           let p = Player.make_player 1 "Alice" in
           let p' = Player.modify_lives (-3) p in
           assert_equal ~printer:string_of_int 4 p'.Player.lives );
         (* is_alive *)
         ( "is alive true when lives above zero" >:: fun _ ->
           let p = Player.make_player 1 "Alice" in
           assert_equal ~printer:string_of_bool true (Player.is_alive p) );
         ( "is alive false when lives zero" >:: fun _ ->
           let p = Player.make_player 1 "Alice" |> Player.set_lives 0 in
           assert_equal ~printer:string_of_bool false (Player.is_alive p) );
         (* add_to_hand *)
         ( "add to hand increases hand size" >:: fun _ ->
           let p = Player.make_player 1 "Alice" in
           let card = List.hd Deck.full_deck in
           let p' = Player.add_to_hand card p in
           assert_equal ~printer:string_of_int 1 (List.length p'.Player.hand) );
         ( "add to hand blocked at lives cap" >:: fun _ ->
           let p = Player.make_player 1 "Alice" |> Player.set_lives 1 in
           let c1 = List.nth Deck.full_deck 0 in
           let c2 = List.nth Deck.full_deck 1 in
           let p' = Player.add_to_hand c1 p |> Player.add_to_hand c2 in
           assert_equal ~printer:string_of_int 1 (List.length p'.Player.hand) );
         (* remove_from_hand *)
         ( "remove from hand decreases hand size" >:: fun _ ->
           let p = Player.make_player 1 "Alice" in
           let card = List.hd Deck.full_deck in
           let p' = Player.add_to_hand card p |> Player.remove_from_hand card in
           assert_equal ~printer:string_of_int 0 (List.length p'.Player.hand) );
         ( "remove from hand no crash if card absent" >:: fun _ ->
           let p = Player.make_player 1 "Alice" in
           let card = List.hd Deck.full_deck in
           let p' = Player.remove_from_hand card p in
           assert_equal ~printer:string_of_int 0 (List.length p'.Player.hand) );
         ( "remove from hand only removes first occurrence" >:: fun _ ->
           let p = Player.make_player 1 "Alice" in
           let card = List.nth Deck.full_deck 0 in
           let p =
             { p with Player.hand = [ card; card; card ] }
           in
           let p' = Player.remove_from_hand card p in
           assert_equal ~printer:string_of_int 2 (List.length p'.Player.hand) );
         (* ── Deck ── *)
         ( "full deck contains 52 cards" >:: fun _ ->
           assert_equal ~printer:string_of_int 52 (List.length Deck.full_deck)
         );
         ( "deal splits into dealt and remaining" >:: fun _ ->
           let dealt, remaining = Deck.deal 7 Deck.full_deck in
           assert_equal ~printer:string_of_int 7 (List.length dealt);
           assert_equal ~printer:string_of_int 45 (List.length remaining) );
         ( "deal returns all cards when n exceeds deck" >:: fun _ ->
           let dealt, remaining = Deck.deal 60 Deck.full_deck in
           assert_equal ~printer:string_of_int 52 (List.length dealt);
           assert_equal ~printer:string_of_int 0 (List.length remaining) );
         ( "shuffle preserves deck size" >:: fun _ ->
           let shuffled = Deck.shuffle Deck.full_deck in
           assert_equal ~printer:string_of_int 52 (List.length shuffled) );
         (* ── State ── *)
         ( "initial state has no players" >:: fun _ ->
           let s = State.make () in
           assert_equal ~printer:string_of_int 0 (List.length s.State.players)
         );
         ( "add player succeeds in waiting phase" >:: fun _ ->
           let s = State.make () in
           let p = Player.make_player 1 "Alice" in
           match State.add_player p s with
           | Error _ -> assert_failure "expected Ok"
           | Ok s' ->
               assert_equal ~printer:string_of_int 1
                 (List.length s'.State.players) );
         ( "add player rejected when game in progress" >:: fun _ ->
           let s = State.make () in
           let p = Player.make_player 1 "Alice" in
           let s' = add_or_fail p s in
           let s_started = State.start_game s' in
           let p2 = Player.make_player 2 "Bob" in
           match State.add_player p2 s_started with
           | Error _ -> ()
           | Ok _ -> assert_failure "expected Error" );
         ( "start game deals 7 cards to each player" >:: fun _ ->
           let s = State.make () in
           let p = Player.make_player 1 "Alice" in
           let s' = add_or_fail p s in
           let s_started = State.start_game s' in
           let alice = List.hd s_started.State.players in
           assert_equal ~printer:string_of_int 7 (List.length alice.Player.hand)
         );
         ( "start game removes dealt cards from deck" >:: fun _ ->
           let s = State.make () in
           let p = Player.make_player 1 "Alice" in
           let s' = add_or_fail p s in
           let s_started = State.start_game s' in
           assert_equal ~printer:string_of_int 45
             (List.length s_started.State.deck) );
         ( "next turn increments turn index" >:: fun _ ->
           let s = State.make () in
           let p1 = Player.make_player 1 "Alice" in
           let p2 = Player.make_player 2 "Bob" in
           let s' = add_or_fail p1 s in
           let s'' = add_or_fail p2 s' in
           let s_next = State.next_turn s'' in
           assert_equal ~printer:string_of_int 1 s_next.State.turn );
         ( "next turn wraps back to first player" >:: fun _ ->
           let s = State.make () in
           let p1 = Player.make_player 1 "Alice" in
           let p2 = Player.make_player 2 "Bob" in
           let s' = add_or_fail p1 s in
           let s'' = add_or_fail p2 s' in
           let s_wrapped = State.next_turn s'' |> State.next_turn in
           assert_equal ~printer:string_of_int 0 s_wrapped.State.turn );
         (* onto_discard *)
         ( "onto_discard adds card to discard pile" >:: fun _ ->
           let s = State.make () in
           let s' = State.onto_discard atk s in
           assert_equal ~printer:string_of_int 1 (List.length s'.State.discard)
         );
         ( "onto_discard puts card at front of discard" >:: fun _ ->
           let s = State.make () in
           let s' = State.onto_discard atk s |> State.onto_discard blk in
           assert_equal blk (List.hd s'.State.discard) );
         ( "onto_discard does not change players or deck" >:: fun _ ->
           let s = two_player_game () in
           let s' = State.onto_discard atk s in
           assert_equal (List.length s.State.players)
             (List.length s'.State.players);
           assert_equal (List.length s.State.deck) (List.length s'.State.deck)
         );
         (* apply_card *)
         ( "apply_card removes card from player hand" >:: fun _ ->
           let s = two_player_game () |> set_hand 1 [ atk; blk ] in
           let s' = State.apply_card 1 atk s in
           let p = get_player 1 s' in
           assert_equal ~printer:string_of_int 1 (List.length p.Player.hand) );
         ( "apply_card adds card to discard" >:: fun _ ->
           let s = two_player_game () |> set_hand 1 [ atk ] in
           let s' = State.apply_card 1 atk s in
           assert_equal atk (List.hd s'.State.discard) );
         ( "apply_card with unknown player id is a no-op" >:: fun _ ->
           let s = two_player_game () in
           let s' = State.apply_card 99 atk s in
           assert_equal s s' );
         (* ── Rules: special_type_of_card — clubs ── *)
         ( "2♣ is Chaos" >:: fun _ ->
           assert_equal (Some Types.Chaos) (Rules.special_type_of_card club2) );
         ( "3♣ is ArrowStorm" >:: fun _ ->
           assert_equal (Some Types.ArrowStorm)
             (Rules.special_type_of_card club3) );
         ( "4♣ is GarbageDisposal" >:: fun _ ->
           assert_equal (Some Types.GarbageDisposal)
             (Rules.special_type_of_card club4) );
         ( "5♣ is LifeLock" >:: fun _ ->
           assert_equal (Some Types.LifeLock) (Rules.special_type_of_card club5)
         );
         ( "6♣ is Reduction" >:: fun _ ->
           assert_equal (Some Types.Reduction)
             (Rules.special_type_of_card club6) );
         ( "7♣ is DeadMansGamble" >:: fun _ ->
           assert_equal (Some Types.DeadMansGamble)
             (Rules.special_type_of_card club7) );
         ( "8♣ is DeadMansGamble" >:: fun _ ->
           assert_equal (Some Types.DeadMansGamble)
             (Rules.special_type_of_card club8) );
         ( "9♣ is TwoToMax" >:: fun _ ->
           assert_equal (Some Types.TwoToMax)
             (Rules.special_type_of_card club9) );
         ( "10♣ is TwoToMax" >:: fun _ ->
           assert_equal (Some Types.TwoToMax)
             (Rules.special_type_of_card club10) );
         (* ── Rules: special_type_of_card — diamonds ── *)
         ( "2♦ is SayNo" >:: fun _ ->
           assert_equal (Some Types.SayNo) (Rules.special_type_of_card diam2) );
         ( "3♦ is Reversify" >:: fun _ ->
           assert_equal (Some Types.Reversify)
             (Rules.special_type_of_card diam3) );
         ( "4♦ is Diplomacy" >:: fun _ ->
           assert_equal (Some Types.Diplomacy)
             (Rules.special_type_of_card diam4) );
         ( "5♦ is Draw2" >:: fun _ ->
           assert_equal (Some Types.Draw2) (Rules.special_type_of_card diam5) );
         ( "6♦ is Silencer" >:: fun _ ->
           assert_equal (Some Types.Silencer)
             (Rules.special_type_of_card diam6) );
         ( "7♦ is DoubleAgent" >:: fun _ ->
           assert_equal (Some Types.DoubleAgent)
             (Rules.special_type_of_card diam7) );
         ( "8♦ is SummonLightning" >:: fun _ ->
           assert_equal (Some Types.SummonLightning)
             (Rules.special_type_of_card diam8) );
         ( "9♦ is Reflector" >:: fun _ ->
           assert_equal (Some Types.Reflector)
             (Rules.special_type_of_card diam9) );
         ( "10♦ is Sacrifice" >:: fun _ ->
           assert_equal (Some Types.Sacrifice)
             (Rules.special_type_of_card diam10) );
         (* ── Rules: special_type_of_card — face cards ── *)
         ( "Jack is Break regardless of suit" >:: fun _ ->
           assert_equal (Some Types.Break) (Rules.special_type_of_card jack) );
         ( "Queen is Steal regardless of suit" >:: fun _ ->
           assert_equal (Some Types.Steal) (Rules.special_type_of_card queen) );
         ( "King is HealOrDoubleAttack regardless of suit" >:: fun _ ->
           assert_equal (Some Types.HealOrDoubleAttack)
             (Rules.special_type_of_card king) );
         (* ── Rules: special_type_of_card — non-specials return None ── *)
         ( "Ace returns None from special_type_of_card" >:: fun _ ->
           assert_equal None (Rules.special_type_of_card ace_s) );
         ( "Spades number returns None from special_type_of_card" >:: fun _ ->
           assert_equal None
             (Rules.special_type_of_card (mk_card (Types.Num 5) Types.Spades))
         );
         ( "Hearts number returns None from special_type_of_card" >:: fun _ ->
           assert_equal None
             (Rules.special_type_of_card (mk_card (Types.Num 5) Types.Hearts))
         );
         (* ── Rules: equipment_type_of_card ── *)
         ( "Ace♠ is UnlimitedAttack" >:: fun _ ->
           assert_equal (Some Types.UnlimitedAttack)
             (Rules.equipment_type_of_card ace_s) );
         ( "Ace♥ is BlockHealReverse" >:: fun _ ->
           assert_equal (Some Types.BlockHealReverse)
             (Rules.equipment_type_of_card ace_h) );
         ( "Ace♣ is Unblockable" >:: fun _ ->
           assert_equal (Some Types.Unblockable)
             (Rules.equipment_type_of_card ace_c) );
         ( "Ace♦ is Random50" >:: fun _ ->
           assert_equal (Some Types.Random50)
             (Rules.equipment_type_of_card ace_d) );
         ( "non-Ace returns None from equipment_type_of_card" >:: fun _ ->
           assert_equal None (Rules.equipment_type_of_card atk) );
         ( "King returns None from equipment_type_of_card" >:: fun _ ->
           assert_equal None (Rules.equipment_type_of_card king) );
         (* ── Rules: card_type_of_card ── *)
         ( "Spades number is BasicAttack" >:: fun _ ->
           assert_equal Types.BasicAttack (Rules.card_type_of_card atk) );
         ( "Hearts 2-5 is BasicBlock" >:: fun _ ->
           assert_equal Types.BasicBlock (Rules.card_type_of_card blk);
           assert_equal Types.BasicBlock (Rules.card_type_of_card blk5) );
         ( "Hearts 6+ is BasicHeal" >:: fun _ ->
           assert_equal Types.BasicHeal (Rules.card_type_of_card heal);
           assert_equal Types.BasicHeal (Rules.card_type_of_card heal10) );
         ( "Ace is Equipment" >:: fun _ ->
           assert_equal
             (Types.Equipment Types.UnlimitedAttack)
             (Rules.card_type_of_card ace_s) );
         ( "Clubs number is Special" >:: fun _ ->
           assert_equal (Types.Special Types.Chaos) (Rules.card_type_of_card club2)
         );
         ( "Diamonds number is Special" >:: fun _ ->
           assert_equal
             (Types.Special Types.Reversify)
             (Rules.card_type_of_card diam3) );
         ( "Jack is Special Break" >:: fun _ ->
           assert_equal (Types.Special Types.Break) (Rules.card_type_of_card jack)
         );
         ( "Queen is Special Steal" >:: fun _ ->
           assert_equal (Types.Special Types.Steal) (Rules.card_type_of_card queen)
         );
         ( "King is Special HealOrDoubleAttack" >:: fun _ ->
           assert_equal
             (Types.Special Types.HealOrDoubleAttack)
             (Rules.card_type_of_card king) );
         (* ── Rules: effect_of_card ── *)
         ( "BasicAttack produces Attack 1" >:: fun _ ->
           assert_equal (Types.Attack 1) (Rules.effect_of_card atk) );
         ( "BasicBlock produces Block" >:: fun _ ->
           assert_equal Types.Block (Rules.effect_of_card blk) );
         ( "Hearts 5 produces Block" >:: fun _ ->
           assert_equal Types.Block (Rules.effect_of_card blk5) );
         ( "BasicHeal produces Heal 1" >:: fun _ ->
           assert_equal (Types.Heal 1) (Rules.effect_of_card heal) );
         ( "Equipment produces NoEffect" >:: fun _ ->
           assert_equal Types.NoEffect (Rules.effect_of_card ace_s) );
         ( "Special produces NoEffect" >:: fun _ ->
           assert_equal Types.NoEffect (Rules.effect_of_card club2) );
         ( "Jack produces NoEffect" >:: fun _ ->
           assert_equal Types.NoEffect (Rules.effect_of_card jack) );
         (* ── Rules: resolve_action — normal turn ── *)
         ( "play attack sets pending" >:: fun _ ->
           let s = two_player_game () |> set_hand 1 [ atk ] in
           let s', _ = ok_or_fail (Rules.resolve_action 1 (Turn.Play atk) (Some 2) s) in
           assert_equal
             (Some { State.attacker_id = 1; target_id = 2; damage = 1 })
             s'.State.pending );
         ( "play attack increments attacks_used" >:: fun _ ->
           let s = two_player_game () |> set_hand 1 [ atk ] in
           let s', _ = ok_or_fail (Rules.resolve_action 1 (Turn.Play atk) (Some 2) s) in
           assert_equal ~printer:string_of_int 1 s'.State.attacks_used );
         ( "play attack removes card from hand" >:: fun _ ->
           let s = two_player_game () |> set_hand 1 [ atk ] in
           let s', _ = ok_or_fail (Rules.resolve_action 1 (Turn.Play atk) (Some 2) s) in
           let p1 = get_player 1 s' in
           assert_equal ~printer:string_of_int 0 (List.length p1.Player.hand) );
         ( "play attack without target returns error" >:: fun _ ->
           let s = two_player_game () |> set_hand 1 [ atk ] in
           match Rules.resolve_action 1 (Turn.Play atk) None s with
           | Error _ -> ()
           | Ok _ -> assert_failure "expected Error" );
         ( "play attack with unknown target returns error" >:: fun _ ->
           let s = two_player_game () |> set_hand 1 [ atk ] in
           match Rules.resolve_action 1 (Turn.Play atk) (Some 99) s with
           | Error _ -> ()
           | Ok _ -> assert_failure "expected Error" );
         ( "play second attack in same round returns error" >:: fun _ ->
           let s = two_player_game () |> set_hand 1 [ atk ] in
           let s = { s with State.attacks_used = 1 } in
           match Rules.resolve_action 1 (Turn.Play atk) (Some 2) s with
           | Error _ -> ()
           | Ok _ -> assert_failure "expected Error" );
         ( "play heal increases actor lives" >:: fun _ ->
           let s =
             two_player_game () |> set_hand 1 [ heal ] |> set_player_lives 1 5
           in
           let s', _ = ok_or_fail (Rules.resolve_action 1 (Turn.Play heal) None s) in
           let p1 = get_player 1 s' in
           assert_equal ~printer:string_of_int 6 p1.Player.lives );
         ( "play heal discards the card" >:: fun _ ->
           let s = two_player_game () |> set_hand 1 [ heal ] in
           let s', _ = ok_or_fail (Rules.resolve_action 1 (Turn.Play heal) None s) in
           assert_equal heal (List.hd s'.State.discard) );
         ( "play heal removes card from hand" >:: fun _ ->
           let s = two_player_game () |> set_hand 1 [ heal ] in
           let s', _ = ok_or_fail (Rules.resolve_action 1 (Turn.Play heal) None s) in
           let p1 = get_player 1 s' in
           assert_equal ~printer:string_of_int 0 (List.length p1.Player.hand) );
         ( "play block with no pending returns error" >:: fun _ ->
           let s = two_player_game () |> set_hand 1 [ blk ] in
           match Rules.resolve_action 1 (Turn.Play blk) None s with
           | Error _ -> ()
           | Ok _ -> assert_failure "expected Error" );
         ( "play NoEffect card returns error" >:: fun _ ->
           let s = two_player_game () |> set_hand 1 [ club2 ] in
           match Rules.resolve_action 1 (Turn.Play club2) None s with
           | Error _ -> ()
           | Ok _ -> assert_failure "expected Error" );
         ( "discard removes card from hand" >:: fun _ ->
           let s = two_player_game () |> set_hand 1 [ atk; blk ] in
           let s', _ =
             ok_or_fail (Rules.resolve_action 1 (Turn.Discard atk) None s)
           in
           let p1 = get_player 1 s' in
           assert_equal ~printer:string_of_int 1 (List.length p1.Player.hand) );
         ( "discard puts card on discard pile" >:: fun _ ->
           let s = two_player_game () |> set_hand 1 [ atk ] in
           let s', _ =
             ok_or_fail (Rules.resolve_action 1 (Turn.Discard atk) None s)
           in
           assert_equal atk (List.hd s'.State.discard) );
         ( "pass does not change game state" >:: fun _ ->
           let s = two_player_game () in
           let s', _ = ok_or_fail (Rules.resolve_action 1 Turn.Pass None s) in
           assert_equal s.State.players s'.State.players;
           assert_equal s.State.discard s'.State.discard );
         (* ── Rules: resolve_action — pending attack ── *)
         ( "non-target cannot act while attack is pending" >:: fun _ ->
           let s =
             two_player_game () |> with_pending 1 2 1
           in
           match Rules.resolve_action 1 Turn.Pass None s with
           | Error _ -> ()
           | Ok _ -> assert_failure "expected Error" );
         ( "target can block pending attack" >:: fun _ ->
           let s =
             two_player_game ()
             |> set_hand 2 [ blk ]
             |> with_pending 1 2 1
           in
           let s', _ =
             ok_or_fail (Rules.resolve_action 2 (Turn.Play blk) None s)
           in
           assert_equal None s'.State.pending );
         ( "blocking clears the pending attack" >:: fun _ ->
           let s =
             two_player_game ()
             |> set_hand 2 [ blk ]
             |> with_pending 1 2 1
           in
           let s', _ =
             ok_or_fail (Rules.resolve_action 2 (Turn.Play blk) None s)
           in
           assert_equal None s'.State.pending );
         ( "blocking discards the block card" >:: fun _ ->
           let s =
             two_player_game ()
             |> set_hand 2 [ blk ]
             |> with_pending 1 2 1
           in
           let s', _ =
             ok_or_fail (Rules.resolve_action 2 (Turn.Play blk) None s)
           in
           assert_equal blk (List.hd s'.State.discard) );
         ( "target passing takes damage" >:: fun _ ->
           let s =
             two_player_game ()
             |> set_player_lives 2 5
             |> with_pending 1 2 1
           in
           let s', _ = ok_or_fail (Rules.resolve_action 2 Turn.Pass None s) in
           let p2 = get_player 2 s' in
           assert_equal ~printer:string_of_int 4 p2.Player.lives );
         ( "target passing clears pending" >:: fun _ ->
           let s = two_player_game () |> with_pending 1 2 1 in
           let s', _ = ok_or_fail (Rules.resolve_action 2 Turn.Pass None s) in
           assert_equal None s'.State.pending );
         ( "target passing to 0 lives triggers game over" >:: fun _ ->
           let s =
             two_player_game ()
             |> set_player_lives 2 1
             |> with_pending 1 2 1
           in
           let s', _ = ok_or_fail (Rules.resolve_action 2 Turn.Pass None s) in
           match s'.State.status with
           | State.GameOver w -> assert_equal 1 w.Player.id
           | _ -> assert_failure "expected GameOver" );
         ( "target cannot discard while attack is pending" >:: fun _ ->
           let s =
             two_player_game ()
             |> set_hand 2 [ atk ]
             |> with_pending 1 2 1
           in
           match Rules.resolve_action 2 (Turn.Discard atk) None s with
           | Error _ -> ()
           | Ok _ -> assert_failure "expected Error" );
         ( "target playing non-block returns error" >:: fun _ ->
           let s =
             two_player_game ()
             |> set_hand 2 [ heal ]
             |> with_pending 1 2 1
           in
           match Rules.resolve_action 2 (Turn.Play heal) None s with
           | Error _ -> ()
           | Ok _ -> assert_failure "expected Error" );
       ]

let _ = run_test_tt_main tests
