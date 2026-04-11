type suit = Hearts | Diamonds | Clubs | Spades

type rank =
  | Num of int  (* 2-10 *)
  | Jack
  | Queen
  | King
  | Ace

type color = Red | Black

type card = {
  rank : rank;
  suit : suit;
  color : color;
}

let color_of_suit = function
  | Hearts | Diamonds -> Red
  | Clubs | Spades -> Black

let make_card rank suit =
  { rank; suit; color = color_of_suit suit }

let full_deck =
  let suits = [Hearts; Diamonds; Clubs; Spades] in
  let ranks = [Num 2; Num 3; Num 4; Num 5; Num 6; Num 7; Num 8; Num 9; Num 10;
               Jack; Queen; King; Ace] in
  List.concat_map (fun s -> List.map (fun r -> make_card r s) ranks) suits

let () =
  let print_usage () =
    Printf.printf "Usage: %s <server | client>\n" Sys.argv.(0)
  in
  if Array.length Sys.argv < 2 then print_usage ()
  else
    match Sys.argv.(1) with
    | "server" -> Server.run_server ()
    | "client" -> Server.run_client ()
    | _ -> print_usage ()