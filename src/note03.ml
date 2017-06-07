(* Accessing lists *)
let rec sum lst =
  match lst with
  | [] -> 0
  | h :: t -> h + sum t

let rec sum xs =
  match xs with
  | [] -> 0
  | x :: xs' -> x + sum xs'

(* let rec count lst = *)
(*   match lst with *)
(*   | [] -> 0 *)
(*   | h::t -> 1 + count t;; *)

let rec length xs =
  match xs with
  | [] -> 0
  | _ :: xs' -> 1 + length xs'

let rec append lst1 lst2 =
  match lst1 with
  | [] -> lst2
  | h :: t -> h :: (append t lst2) (* this append just like built-in operator @ *)

let empty lst =
  match lst with
  | [] -> true
  | h :: t -> false

let empty lst =
  match lst with
  | [] -> true
  | _ -> false

let empty lst = lst = []

(* Mutating lists *)
let inc_first lst =
  match lst with
  | [] -> []
  | h :: t -> (h + 1) :: t;;

match 1 :: [] with
  | [] -> false
  | h :: t -> (h >= 1) && (length t = 0);;

let rec sum lst =
    match lst with
    | h :: t -> h + sum t
    | [] -> 0

(* Tail recursion *)
let rec sum (l: int list): int =
  match l with
  | [] -> 0
  | x :: xs -> x + sum xs

let rec sum_plus_acc (acc: int) (l: int list) =
  match l with
  | [] -> acc
  | x :: xs -> sum_plus_acc (acc + x) xs

let sum_tr: int list -> int = sum_plus_acc 0

(* Bonus syntax *)
let rec sum = function
  | [] -> 0
  | h :: t -> h + sum t
