(* client.ml — Raylib GUI client for the card game

   Architecture: - Main thread: Raylib game loop — renders state, handles mouse
   clicks - Background thread: Lwt TCP loop — reads from server, writes player
   commands

   Communication between threads via two thread-safe queues: inbound_q : string
   Queue.t — lines received from server, read by main thread outbound_q : string
   Queue.t — commands to send, written by main thread *)

open Raylib
open Cs_3110_project

(* ── Shared state between threads ── *)

(* Mutex-protected queues for cross-thread messaging *)
let inbound_q : string Queue.t = Queue.create ()
let outbound_q : string Queue.t = Queue.create ()
let q_mutex = Mutex.create ()

(* Which screen we are currently showing *)
type screen =
  | NameEntry (* typing your name before connecting *)
  | Lobby (* connected, waiting for ready votes *)
  | InGame (* game is running *)

let current_screen : screen ref = ref NameEntry

(* The rendered view of the game. Main thread reads this each frame. Network
   thread writes to it when it parses a server message. *)
type game_view = {
  mutable log : string list;
  mutable hand : Types.card list; (* actual card values, not strings *)
  mutable players : string list;
  mutable status_bar : string;
  mutable prompt : string;
  mutable waiting_for_input : bool;
  mutable pending_attack : int option;
}

let view : game_view =
  {
    log = [];
    hand = [];
    players = [];
    status_bar = "";
    prompt = "";
    waiting_for_input = false;
    pending_attack = None;
  }

(* ── Card parsing ── *)

(* Parse a card token like "7S", "AH", "10D", "JC" into a Types.card. Returns
   None if the token is malformed. *)
let parse_card token =
  let n = String.length token in
  if n < 2 then None
  else
    let suit_char = token.[n - 1] in
    let rank_str = String.sub token 0 (n - 1) in
    let suit_opt =
      match suit_char with
      | 'H' -> Some Types.Hearts
      | 'D' -> Some Types.Diamonds
      | 'C' -> Some Types.Clubs
      | 'S' -> Some Types.Spades
      | _ -> None
    in
    let rank_opt =
      match rank_str with
      | "A" -> Some Types.Ace
      | "K" -> Some Types.King
      | "Q" -> Some Types.Queen
      | "J" -> Some Types.Jack
      | s -> (
          match int_of_string_opt s with
          | Some n when n >= 2 && n <= 10 -> Some (Types.Num n)
          | _ -> None)
    in
    match (suit_opt, rank_opt) with
    | Some suit, Some rank ->
        let color =
          match suit with
          | Types.Hearts | Types.Diamonds -> Types.Red
          | Types.Clubs | Types.Spades -> Types.Black
        in
        Some { Types.rank; suit; color }
    | _ -> None

(* Render a Types.card back to a short display string like "7S", "AH" *)
let string_of_card (c : Types.card) =
  let rank_str =
    match c.Types.rank with
    | Types.Num n -> string_of_int n
    | Types.Jack -> "J"
    | Types.Queen -> "Q"
    | Types.King -> "K"
    | Types.Ace -> "A"
  in
  let suit_str =
    match c.Types.suit with
    | Types.Hearts -> "H"
    | Types.Diamonds -> "D"
    | Types.Clubs -> "C"
    | Types.Spades -> "S"
  in
  rank_str ^ suit_str

let view_mutex = Mutex.create ()

(* ── Network thread ── *)

(* Push a line received from the server onto the inbound queue. The main thread
   drains this queue each frame and updates [view]. *)
let push_inbound line =
  Mutex.lock q_mutex;
  Queue.push line inbound_q;
  Mutex.unlock q_mutex

(* Pull all pending outbound commands and return them. *)
let drain_outbound () =
  Mutex.lock q_mutex;
  let cmds = Queue.fold (fun acc s -> s :: acc) [] outbound_q in
  Queue.clear outbound_q;
  Mutex.unlock q_mutex;
  List.rev cmds

(* The Lwt loop that owns the TCP connection. Runs in a background thread via
   [Thread.create]. *)
let network_thread_fn host port =
  Lwt_main.run
    begin
      let open Lwt.Syntax in
      let addr = Unix.ADDR_INET (Unix.inet_addr_of_string host, port) in
      let* server_in, server_out = Lwt_io.open_connection addr in

      (* Receive loop: read one line at a time, push to inbound queue *)
      let recv_loop () =
        let rec loop () =
          let* line = Lwt_io.read_line server_in in
          push_inbound line;
          loop ()
        in
        loop ()
      in

      (* Send loop: poll outbound queue every 50 ms, flush anything queued *)
      let send_loop () =
        let rec loop () =
          let cmds = drain_outbound () in
          let* () =
            Lwt_list.iter_s
              (fun cmd ->
                let* () = Lwt_io.fprintlf server_out "%s" cmd in
                Lwt_io.flush server_out)
              cmds
          in
          let* () = Lwt_unix.sleep 0.05 in
          loop ()
        in
        loop ()
      in

      Lwt.join [ recv_loop (); send_loop () ]
    end

(* ── Parsing server messages into view updates ── *)

let update_view_from_line line =
  Mutex.lock view_mutex;
  (* Append to log (keep last 20 lines) *)
  view.log <-
    line
    ::
    (if List.length view.log >= 20 then
       List.filteri (fun i _ -> i < 19) view.log
     else view.log);
  (* Parse hand lines: "Your hand (3): [0]7S [1]AH [2]KD" *)
  if String.length line > 10 && String.sub line 0 9 = "Your hand" then
    begin match String.index_opt line ':' with
    | None -> ()
    | Some i ->
        let rest =
          String.trim (String.sub line (i + 1) (String.length line - i - 1))
        in
        let tokens =
          String.split_on_char ' ' rest |> List.filter (fun s -> s <> "")
        in
        (* Strip "[n]" prefix, parse into Types.card, drop any malformed
           tokens *)
        let cards =
          List.filter_map
            (fun tok ->
              let stripped =
                match String.index_opt tok ']' with
                | None -> tok
                | Some j -> String.sub tok (j + 1) (String.length tok - j - 1)
              in
              parse_card stripped)
            tokens
        in
        view.hand <- cards;
        current_screen := InGame
    end;
  (* Parse status bar: "Lives: Alice: 7/7 | Bob: 5/7" *)
  if String.length line > 6 && String.sub line 0 6 = "Lives:" then
    view.status_bar <- String.trim (String.sub line 6 (String.length line - 6));
  (* Server sends "Enter your name: " immediately on connect -> move to Lobby so
     the player knows they're connected while we wait for more messages *)
  if String.length line >= 16 && String.sub line 0 16 = "Enter your name:" then
    current_screen := Lobby;
  (* "Ready to play?" prompt -> definitely in lobby *)
  if String.length line >= 14 && String.sub line 0 14 = "Ready to play?" then
    current_screen := Lobby;
  (* Game is starting *)
  if line = "All players ready! Game starting..." then current_screen := InGame;
  (* Detect action-phase prompts — the prompt block contains "> " *)
  let contains_prompt s =
    let needle = "> " in
    let nl = String.length needle in
    let sl = String.length s in
    if sl < nl then false
    else
      let found = ref false in
      for i = 0 to sl - nl do
        if String.sub s i nl = needle then found := true
      done;
      !found
  in
  if contains_prompt line then begin
    view.prompt <- line;
    view.waiting_for_input <- true;
    view.pending_attack <- None;
    (* Parse "Others: Alice (7 hp), Bob (5 hp)" from the prompt block. The
       prompt is one big string with \n embedded. *)
    let sublines = String.split_on_char '\n' line in
    List.iter
      (fun sl ->
        let sl = String.trim sl in
        if String.length sl > 7 && String.sub sl 0 7 = "Others:" then begin
          let rest = String.trim (String.sub sl 7 (String.length sl - 7)) in
          (* Each entry is "Name (N hp)" separated by ", " *)
          let entries = String.split_on_char ',' rest in
          view.players <-
            List.filter_map
              (fun entry ->
                let entry = String.trim entry in
                match String.index_opt entry ' ' with
                | None -> if entry = "" then None else Some entry
                | Some i -> Some (String.sub entry 0 i))
              entries
        end)
      sublines
  end;
  Mutex.unlock view_mutex

(* ── Raylib rendering ── *)

(* Layout constants *)
let screen_w = 1024
let screen_h = 768
let card_w = 80
let card_h = 110
let card_spacing = 10
let hand_y = screen_h - card_h - 40
let log_x = 20
let log_y = 20
let log_line_h = 18
let max_log_vis = 20

(* Draw the command log on the left side *)
let draw_log () =
  Mutex.lock view_mutex;
  let lines = view.log in
  Mutex.unlock view_mutex;
  let visible = List.filteri (fun i _ -> i < max_log_vis) lines in
  List.iteri
    (fun i line ->
      let y = log_y + (i * log_line_h) in
      draw_text line log_x y 14 (Color.create 200 200 180 255))
    visible

(* Draw status bar at top right *)
let draw_status () =
  Mutex.lock view_mutex;
  let s = view.status_bar in
  Mutex.unlock view_mutex;
  draw_text s ((screen_w / 2) - 200) 20 16 (Color.create 180 220 180 255)

let card_raylib_color (c : Types.card) =
  match c.Types.color with
  | Types.Red -> Color.create 220 50 50 255
  | Types.Black -> Color.create 20 20 20 255

let card_description (c : Types.card) : string list =
  match c.Types.rank with
  | Types.Ace -> (
      match c.Types.suit with
      | Types.Clubs ->
          [
            "EQUIP: 50/50";
            "Half chance an attack";
            "goes through (black=-1 life)";
          ]
      | Types.Spades ->
          [
            "EQUIP: Unlimited attack";
            "The 1 attack/round limit";
            "does not apply to you";
          ]
      | Types.Hearts ->
          [
            "EQUIP: Block/heal reverse";
            "Use blocks as heals";
            "and heals as blocks";
          ]
      | Types.Diamonds ->
          [
            "EQUIP: Unblockable attacks";
            "Cannot immediately respond";
            "to attacks from you";
          ])
  | Types.Jack ->
      [ "Break"; "Take someone's card + discard"; "Can break equips" ]
  | Types.Queen -> [ "Steal"; "Take someone's card"; "+ add to your hand" ]
  | Types.King -> [ "Heal / Double attack"; "(on the same person)" ]
  | Types.Num n -> (
      match c.Types.suit with
      | Types.Spades -> [ "Attack"; "-1 life" ]
      | Types.Hearts ->
          if n <= 5 then [ "Block"; "Attack prevented" ]
          else [ "Heal"; "+1 life" ]
      | Types.Clubs -> (
          match n with
          | 2 -> [ "Chaos"; "-1 life, blockable by an attack" ]
          | 3 -> [ "Arrow storm"; "-1 life, blockable by a block" ]
          | 4 -> [ "Garbage disposal"; "Top discard gets added to your hand" ]
          | 5 ->
              [
                "Life lock";
                "Gain/lose hearts together";
                "Break by attacking each other";
                "(both take life)";
              ]
          | 6 -> [ "Reduction"; "Discard all cards but basics" ]
          | 7 | 8 ->
              [
                "Dead man's gamble";
                "+1 life";
                "If other DMG is played: -1 life";
              ]
          | 9 | 10 ->
              [
                "2 to max"; "Get both cards to add"; "another heart to your max";
              ]
          | _ -> [ "Special" ])
      | Types.Diamonds -> (
          match n with
          | 2 -> [ "Say no"; "Doesn't work on lightning"; "or attacks" ]
          | 3 -> [ "Reversify" ]
          | 4 ->
              [
                "Diplomacy";
                "Exchange cards with others";
                "who join to gain a heart";
              ]
          | 5 ->
              [
                "Draw 2";
                "Draw 2 cards";
                "Discard to life amount at end of round";
              ]
          | 6 ->
              [
                "Silencer";
                "If blue is flipped,";
                "you cannot play any cards in the round";
              ]
          | 7 ->
              [
                "Double agent";
                "If blue is flipped, show your cards";
                "to the person who placed double agent";
              ]
          | 8 ->
              [
                "Summon lightning";
                "If judgement passes: -3 lives";
                "Otherwise passed to next player";
                "(like hot potato!)";
              ]
          | 9 -> [ "Reflector"; "Reflect the action,"; "but take a life" ]
          | 10 -> [ "Sacrifice"; "-3 lives"; "+1 max heart level" ]
          | _ -> [ "Special" ]))

let draw_tooltip lines card_x card_y =
  let font_size = 13 in
  let line_h = 17 in
  let pad = 8 in
  let max_w =
    List.fold_left
      (fun acc line -> max acc (measure_text line font_size))
      0 lines
  in
  let tw = max_w + (pad * 2) in
  let th = (List.length lines * line_h) + (pad * 2) in
  let tx = card_x + (card_w / 2) - (tw / 2) in
  let ty = card_y - th - 6 in
  let tx = max 4 (min tx (screen_w - tw - 4)) in
  let ty = max 4 ty in
  draw_rectangle tx ty tw th (Color.create 30 30 30 230);
  draw_rectangle_lines tx ty tw th (Color.create 200 180 100 255);
  List.iteri
    (fun i line ->
      draw_text line (tx + pad)
        (ty + pad + (i * line_h))
        font_size
        (Color.create 240 230 200 255))
    lines

(* Draw a suit symbol centered at (cx, cy) with the given size using primitives.
   Avoids relying on Raylib's default ASCII-only bitmap font for Unicode
   glyphs. *)
let draw_suit_symbol suit cx cy size col =
  let h = size in
  let w = size in
  match suit with
  | Types.Hearts ->
      let r = h * 3 / 10 in
      draw_circle (cx - r + 1) (cy - (h / 6)) (float_of_int r) col;
      draw_circle (cx + r - 1) (cy - (h / 6)) (float_of_int r) col;
      draw_triangle
        (Vector2.create
           (float_of_int (cx + (w / 2)))
           (float_of_int (cy - (h / 6))))
        (Vector2.create
           (float_of_int (cx - (w / 2)))
           (float_of_int (cy - (h / 6))))
        (Vector2.create (float_of_int cx) (float_of_int (cy + (h / 2))))
        col
  | Types.Diamonds ->
      let hw = w / 2 in
      let hh = h / 2 in
      draw_triangle
        (Vector2.create (float_of_int cx) (float_of_int (cy - hh)))
        (Vector2.create (float_of_int (cx + hw)) (float_of_int cy))
        (Vector2.create (float_of_int (cx - hw)) (float_of_int cy))
        col;
      draw_triangle
        (Vector2.create (float_of_int cx) (float_of_int (cy + hh)))
        (Vector2.create (float_of_int (cx - hw)) (float_of_int cy))
        (Vector2.create (float_of_int (cx + hw)) (float_of_int cy))
        col
  | Types.Spades ->
      let r = h * 3 / 10 in
      draw_circle (cx - r + 1) (cy + (h / 6)) (float_of_int r) col;
      draw_circle (cx + r - 1) (cy + (h / 6)) (float_of_int r) col;
      draw_triangle
        (Vector2.create
           (float_of_int (cx - (w / 2)))
           (float_of_int (cy + (h / 6))))
        (Vector2.create
           (float_of_int (cx + (w / 2)))
           (float_of_int (cy + (h / 6))))
        (Vector2.create (float_of_int cx) (float_of_int (cy - (h / 2))))
        col;
      draw_rectangle (cx - 2) (cy + (h / 3)) 4 (h / 5) col
  | Types.Clubs ->
      let r = h / 4 in
      draw_circle cx (cy - (r / 2)) (float_of_int r) col;
      draw_circle (cx - r) (cy + (r / 2)) (float_of_int r) col;
      draw_circle (cx + r) (cy + (r / 2)) (float_of_int r) col;
      draw_rectangle (cx - 2) (cy + r) 4 (r + 3) col

let rank_str (c : Types.card) =
  match c.Types.rank with
  | Types.Num n -> string_of_int n
  | Types.Jack -> "J"
  | Types.Queen -> "Q"
  | Types.King -> "K"
  | Types.Ace -> "A"

let draw_card x y (c : Types.card) hovered =
  let bg =
    if hovered then Color.create 255 255 220 255
    else Color.create 245 245 235 255
  in
  draw_rectangle x y card_w card_h bg;
  draw_rectangle_lines x y card_w card_h (Color.create 100 100 80 255);
  if hovered then
    draw_rectangle_lines (x + 1) (y + 1) (card_w - 2) (card_h - 2)
      (Color.create 200 160 0 255);
  let col = card_raylib_color c in
  (* Top-left: rank label *)
  draw_text (rank_str c) (x + 5) (y + 5) 18 col;
  (* Small suit icon just below the rank in the corner *)
  draw_suit_symbol c.Types.suit (x + 12) (y + 32) 14 col;
  (* Large suit icon centered on the card face *)
  draw_suit_symbol c.Types.suit (x + (card_w / 2)) (y + (card_h / 2)) 36 col

(* Use Rules.card_type_of_card — no more string guessing *)
let is_attack_card (c : Types.card) =
  Rules.card_type_of_card c = Types.BasicAttack

(* Draw the player's hand along the bottom *)
let draw_hand () =
  Mutex.lock view_mutex;
  let hand = view.hand in
  let waiting = view.waiting_for_input in
  let pending = view.pending_attack in
  Mutex.unlock view_mutex;
  let interactive = waiting && pending = None in
  let mx = get_mouse_x () in
  let my = get_mouse_y () in
  let hovered_info = ref None in
  List.iteri
    (fun i card ->
      let x = 20 + (i * (card_w + card_spacing)) in
      let base_y = if waiting then hand_y else hand_y + 20 in
      let hovered =
        interactive && mx >= x
        && mx <= x + card_w
        && my >= base_y
        && my <= base_y + card_h
      in
      let y = if hovered then base_y - 12 else base_y in
      draw_card x y card hovered;
      if hovered then hovered_info := Some (card, x, y);
      if hovered && is_mouse_button_pressed MouseButton.Left then
        begin if is_attack_card card then begin
          Mutex.lock view_mutex;
          view.pending_attack <- Some i;
          Mutex.unlock view_mutex
        end
        else begin
          Mutex.lock q_mutex;
          Queue.push (Printf.sprintf "play %d" i) outbound_q;
          Mutex.unlock q_mutex;
          Mutex.lock view_mutex;
          view.waiting_for_input <- false;
          Mutex.unlock view_mutex
        end
        end)
    hand;
  match !hovered_info with
  | None -> ()
  | Some (card, cx, cy) -> draw_tooltip (card_description card) cx cy

(* Draw target selection overlay when an attack card has been clicked *)
let draw_target_selection () =
  Mutex.lock view_mutex;
  let pending = view.pending_attack in
  let players = view.players in
  Mutex.unlock view_mutex;
  match pending with
  | None -> ()
  | Some card_idx ->
      (* Dim overlay *)
      draw_rectangle 0 0 screen_w screen_h (Color.create 0 0 0 120);
      draw_text "Choose a target:"
        ((screen_w / 2) - 100)
        ((screen_h / 2) - 80)
        24
        (Color.create 240 220 150 255);
      let mx = get_mouse_x () in
      let my = get_mouse_y () in
      List.iteri
        (fun i name ->
          let btn_x = (screen_w / 2) - 100 in
          let btn_y = (screen_h / 2) - 30 + (i * 55) in
          let hovered =
            mx >= btn_x && mx <= btn_x + 200 && my >= btn_y && my <= btn_y + 44
          in
          draw_rectangle btn_x btn_y 200 44
            (if hovered then Color.create 180 50 50 255
             else Color.create 130 30 30 255);
          draw_text name (btn_x + 10) (btn_y + 12) 20 Color.white;
          if hovered && is_mouse_button_pressed MouseButton.Left then begin
            Mutex.lock q_mutex;
            Queue.push (Printf.sprintf "play %d %s" card_idx name) outbound_q;
            Mutex.unlock q_mutex;
            Mutex.lock view_mutex;
            view.waiting_for_input <- false;
            view.pending_attack <- None;
            Mutex.unlock view_mutex
          end)
        players;
      (* Cancel button *)
      let cancel_x = (screen_w / 2) - 60 in
      let cancel_y = (screen_h / 2) - 30 + (List.length players * 55) + 10 in
      let hover_cancel =
        mx >= cancel_x
        && mx <= cancel_x + 120
        && my >= cancel_y
        && my <= cancel_y + 36
      in
      draw_rectangle cancel_x cancel_y 120 36
        (if hover_cancel then Color.create 80 80 80 255
         else Color.create 50 50 50 255);
      draw_text "Cancel" (cancel_x + 28) (cancel_y + 10) 18 Color.white;
      if hover_cancel && is_mouse_button_pressed MouseButton.Left then begin
        Mutex.lock view_mutex;
        view.pending_attack <- None;
        Mutex.unlock view_mutex
      end

(* Draw Pass and Discard buttons when it's our turn *)
let draw_action_buttons () =
  Mutex.lock view_mutex;
  let waiting = view.waiting_for_input in
  Mutex.unlock view_mutex;
  if not waiting then ()
  else begin
    (* Pass button *)
    let pass_x = screen_w - 160 in
    let pass_y = screen_h - 60 in
    let mx = get_mouse_x () in
    let my = get_mouse_y () in
    let hover_pass =
      mx >= pass_x && mx <= pass_x + 130 && my >= pass_y && my <= pass_y + 40
    in
    draw_rectangle pass_x pass_y 130 40
      (if hover_pass then Color.create 80 80 200 255
       else Color.create 50 50 150 255);
    draw_text "Pass" (pass_x + 45) (pass_y + 12) 18 Color.white;
    if hover_pass && is_mouse_button_pressed MouseButton.Left then begin
      Mutex.lock q_mutex;
      Queue.push "pass" outbound_q;
      Mutex.unlock q_mutex;
      Mutex.lock view_mutex;
      view.waiting_for_input <- false;
      Mutex.unlock view_mutex
    end
  end

(* Draw current prompt at the bottom center *)
let draw_prompt () =
  Mutex.lock view_mutex;
  let p = view.prompt in
  Mutex.unlock view_mutex;
  if p <> "" then
    draw_text p
      ((screen_w / 2) - 200)
      (screen_h - 30) 14
      (Color.create 220 200 100 255)

(* ── Main entry point ── *)

let run_client_gui () =
  init_window screen_w screen_h "Card Game";
  set_target_fps 60;

  (* Name entry state — typed before the network thread starts *)
  let name_buf = Buffer.create 32 in
  let name_submitted = ref false in

  (* ── Name entry loop — runs before network connects ── *)
  while (not (window_should_close ())) && not !name_submitted do
    let rec collect_keys () =
      let code = get_char_pressed () in
      if code <> Uchar.of_int 0 then begin
        let n = Uchar.to_int code in
        if n >= 32 && n < 127 && Buffer.length name_buf < 24 then
          Buffer.add_char name_buf (Char.chr n);
        collect_keys ()
      end
    in
    collect_keys ();
    (* Also handle Enter via key press (for terminals that send KEY_ENTER) *)
    if is_key_pressed Key.Enter && Buffer.length name_buf > 0 then
      name_submitted := true;
    (* Backspace *)
    if is_key_pressed Key.Backspace && Buffer.length name_buf > 0 then begin
      let s = Buffer.contents name_buf in
      Buffer.clear name_buf;
      Buffer.add_string name_buf (String.sub s 0 (String.length s - 1))
    end;

    begin_drawing ();
    clear_background (Color.create 34 85 34 255);

    (* Title *)
    draw_text "Card Game"
      ((screen_w / 2) - 80)
      ((screen_h / 2) - 120)
      48
      (Color.create 240 220 150 255);

    (* Input box *)
    let box_x = (screen_w / 2) - 160 in
    let box_y = (screen_h / 2) - 20 in
    draw_text "Enter your name:" box_x (box_y - 30) 20
      (Color.create 200 200 180 255);
    draw_rectangle box_x box_y 320 44 (Color.create 245 245 235 255);
    draw_rectangle_lines box_x box_y 320 44 (Color.create 200 160 0 255);
    draw_text (Buffer.contents name_buf) (box_x + 10) (box_y + 12) 22
      (Color.create 20 20 20 255);
    (* Blinking cursor *)
    if int_of_float (get_time ()) mod 2 = 0 then
      draw_text "_"
        (box_x + 10 + measure_text (Buffer.contents name_buf) 22)
        (box_y + 12) 22
        (Color.create 100 100 80 255);

    (* Hint *)
    draw_text "Press Enter to connect"
      ((screen_w / 2) - 110)
      (box_y + 60) 16
      (Color.create 160 160 140 255);

    end_drawing ()
  done;

  if window_should_close () then (
    close_window ();
    exit 0);

  (* ── Start network thread now that we have a name ── *)
  let player_name = Buffer.contents name_buf in
  let _net_thread =
    Thread.create
      (fun () ->
        (* The network thread sends the name as the very first outbound message.
           The server reads one line immediately on connect and treats it as the
           name. *)
        Mutex.lock q_mutex;
        Queue.push player_name outbound_q;
        Mutex.unlock q_mutex;
        network_thread_fn "127.0.0.1" 12345)
      ()
  in

  current_screen := Lobby;

  (* ── Lobby / in-game loop ── *)
  while not (window_should_close ()) do
    (* Drain inbound queue and update view *)
    Mutex.lock q_mutex;
    let lines = Queue.fold (fun acc s -> s :: acc) [] inbound_q in
    Queue.clear inbound_q;
    Mutex.unlock q_mutex;
    List.iter update_view_from_line (List.rev lines);

    begin_drawing ();
    clear_background (Color.create 34 85 34 255);

    (match !current_screen with
    | NameEntry -> () (* unreachable here *)
    | Lobby ->
        (* Show the server log and a ready button *)
        draw_log ();
        draw_text "Waiting for players..."
          ((screen_w / 2) - 120)
          ((screen_h / 2) - 60)
          24
          (Color.create 240 220 150 255);

        (* Ready button *)
        let btn_x = (screen_w / 2) - 80 in
        let btn_y = screen_h / 2 in
        let mx = get_mouse_x () in
        let my = get_mouse_y () in
        let hovered =
          mx >= btn_x && mx <= btn_x + 160 && my >= btn_y && my <= btn_y + 50
        in
        draw_rectangle btn_x btn_y 160 50
          (if hovered then Color.create 60 180 60 255
           else Color.create 40 140 40 255);
        draw_text "Ready!" (btn_x + 45) (btn_y + 15) 22 Color.white;
        if hovered && is_mouse_button_pressed MouseButton.Left then begin
          Mutex.lock q_mutex;
          Queue.push "yes" outbound_q;
          Mutex.unlock q_mutex
        end;

        (* Also show "not ready" option *)
        let no_x = (screen_w / 2) - 60 in
        let no_y = btn_y + 65 in
        draw_text "Not ready yet" no_x no_y 14 (Color.create 160 160 140 255)
    | InGame ->
        draw_log ();
        draw_status ();
        draw_hand ();
        draw_action_buttons ();
        draw_prompt ();
        draw_target_selection ());

    end_drawing ()
  done;

  close_window ()
