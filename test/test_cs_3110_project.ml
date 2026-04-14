open OUnit2
open Cs_3110_project

let add_or_fail p s =
  match State.add_player p s with
  | Ok s' -> s'
  | Error m -> assert_failure m

let tests =
  "test suite"
  >::: [
         (* make_player *)
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
       ]

let _ = run_test_tt_main tests
