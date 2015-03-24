open Lwt.Infix

type player = Player1 | Player2 [@@deriving show]
let player_of_string = function
  | "player1" -> Player1
  | "player2" -> Player2
  | e         -> failwith (":: [ERROR] Unknown player name: " ^ e)

type settings = {
  time_bank: int;
  time_per_move: int;
  hands_per_level: int;
  starting_stack: int;
  player: player;
} [@@deriving show]

let read_split () =
  Lwt_io.read_line Lwt_io.stdin >|= Str.split (Str.regexp " ")

let read_settings () =
  let next_setting () = read_split () >|= (fun s -> List.nth s 2) in
  let%lwt time_bank       = next_setting () >|= int_of_string in
  let%lwt time_per_move   = next_setting () >|= int_of_string in
  let%lwt hands_per_level = next_setting () >|= int_of_string in
  let%lwt starting_stack  = next_setting () >|= int_of_string in
  let%lwt player          = next_setting () >|= player_of_string in
  Lwt.return {time_bank; time_per_move; hands_per_level; starting_stack; player}
       
type suit = Spade | Heart | Diamond | Club [@@deriving show, eq, ord]
type card = {
  rank: int;
  suit: suit;
} [@@deriving show, eq, ord]

let card_of_string s =
  let rank = match String.sub s 0 1 with
    | "T" -> 10 | "J" -> 11 | "Q" -> 12
    | "K" -> 13 | "A" -> 14 | i   -> int_of_string i in
  let suit = match String.sub s 1 2 with
    | "s" -> Spade   | "h" -> Heart | "d" -> Diamond
    | "c" -> Club    | x   -> failwith (String.concat "" ["no such suit: "; x; " in card: "; s]) in
  { rank; suit }

type round = {
  id: int;
  small_blind: int;
  big_blind: int;
  on_button: player;
  mutable table: card list;
  mutable max_win_pot: int;
  mutable amount_to_call: int;
} [@@deriving show]

let read_round () =
  let next_input () = read_split () >|= (fun s -> List.nth s 2) in
  let%lwt id          = next_input () >|= int_of_string in
  let%lwt small_blind = next_input () >|= int_of_string in
  let%lwt big_blind   = next_input () >|= int_of_string in
  let%lwt on_button   = next_input () >|= player_of_string in
  Lwt.return { id; small_blind; big_blind; on_button; table=[]; max_win_pot=0; amount_to_call=0 }

type hand = card * card [@@deriving show]

type bot_status = {
  mutable stack: int;
  mutable hand: hand option;
} [@@deriving show]

type poker_hand =
  | High_card       of card list
  | One_pair        of int * card list
  | Two_pair        of int * int * card list
  | Drill           of int * card list
  | Straight        of int
  | Flush           of card list
  | Full_house      of int * int
  | Poker           of int * card
  | Straight_flush  of int
  | Royal_flush     of int
        [@@ deriving show, eq, ord]

let rec on_action () =
  let%lwt line = Lwt_io.read_line Lwt_io.stdin in
  (match Str.split (Str.regexp " ") line with
   | "Action" :: _ -> ignore (Lwt_io.printl "check 0")
   | l -> ());
  on_action ()

(* let () = *)
(*   Lwt_main.run ( *)
(*     let%lwt settings = read_settings () in *)
(*     Lwt_io.eprintl (show_settings settings) >> *)
(*     let%lwt round = read_round () in *)
(*     Lwt_io.eprintl (show_round round) >> *)
(*     on_action () *)
(*   ) *)
