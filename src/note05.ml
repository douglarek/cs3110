(* Type synonyms *)
type point = float * float
type vector = float list
type matrix = float list list

let getx : point -> float =
  fun (x, _) -> x

let pt : point = (1., 2.)

let floatpair : float * float = (1., 3.)

let one = getx pt

let one' = getx floatpair

(* Variants *)
type day = Sun | Mon | Tue | Wed | Thu | Fri | Sat

type ptype = TNormal | TFire | TWater

type peff = ENormal | ENotVery | Esuper

type shape =
  | Point of point
  | Circle of point * float
  | Rect of point * point

let pi = 4. *. atan 1.317

let area = function
  | Point _ -> 0.0
  | Circle (_, r) -> pi *. (r ** 2.0)
  | Rect ((x1, y1), (x2, y2)) ->
    let w = x2 -. x1 in
    let h = y2 -. y1 in
    w *. h

let center = function
  | Point p -> p
  | Circle (p, _) -> p
  | Rect ((x1, y1), (x2, y2))->
    ((x2 -. x1) /. 2.0, (y2 -. y1) /. 2.0)

type string_or_int =
  | String of string
  | Int of int

type string_or_int_list = string_or_int list

let rec sum : string_or_int list -> int = function
  | [] -> 0
  | (String s) :: t -> int_of_string s + sum t
  | (Int i) :: t -> i + sum t

let three = sum [String "1"; Int 2]

type t = Left of int | Right of int

let x = Left 1

let double_right : t -> int = function
  | Left i -> i
  | Right i -> 2 * i


(* Catch-all cases *)

type color = Blue | Red

let string_of_color = function
  | Blue -> "blue"
  | Red -> "red"


(* Recursive variants *)

type intlist = Nil | Cons of int * intlist

let lst3 = Cons (3, Nil)

let lst123 = Cons (1, Cons (2, lst3))

let rec sum : intlist -> int = function
  | Nil -> 0
  | Cons (h, t) -> h + sum t

let rec length : intlist -> int = function
  | Nil -> 0
  | Cons (h, t) -> 1 + length t

let empty : intlist -> bool = function
  | Nil -> true
  | Cons _ -> false

(* Parameterized variants *)

type 'a mylist = Nil | Cons of 'a * 'a mylist

let lst3 = Cons (3, Nil)  (* similar to [3] *)
let lst_hi = Cons ("hi", Nil)  (* similar to ["hi"] *)

let rec length : 'a mylist -> int = function
  | Nil -> 0
  | Cons (_, t) -> 1 + length t

let empty : 'a mylist -> bool = function
  | Nil -> true
  | Cons _ -> false

type ('a, 'b) pair = {first: 'a; second: 'b}

let x = {first=2; second="hello"}

(* Exception semantics *)
exception A
exception B;;

(* let x = raise A in raise B;; *)

match List.hd [] with
  | [] -> "empty"
  | h :: t -> "nonempty"
  | exception (Failure s) -> s


(* Case study: Trees *)

(* type 'a tree = *)
(*   | Leaf *)
(*   | Node of 'a * 'a tree * 'a tree;; *)
type 'a tree =
  | Leaf
  | Node of 'a node
and 'a node = {
  value: 'a;
  left: 'a tree;
  right: 'a tree;
}

let t =
  Node {
    value= 2;
    left = Node {value=1; left=Leaf; right=Leaf};
    right = Node {value=3; left=Leaf; right=Leaf}
  }

let rec mem x = function
  | Leaf -> false
  | Node {value; left; right} -> value = x || mem x left || mem x right

let preorder_lin t =
  let rec pre_acc acc = function
    | Leaf -> acc
    | Node {value; left; right} -> value :: (pre_acc (pre_acc acc right) left)
  in pre_acc []

(* Case study: Natural numbers *)
type nat = Zero | Succ of nat

let zero  = Zero
let one   = Succ Zero
let two   = Succ one
let three = Succ two
let four  = Succ three

let iszero (n : nat) : bool =
  match n with
  | Zero    -> true
  | Succ(m) -> false

let pred (n : nat) : nat =
  match n with
  | Zero    -> failwith "pred Zero is undefined"
  | Succ(m) -> m

let rec add (n1 : nat) (n2 : nat) : nat =
  match n1 with
  | Zero -> n2
  | Succ(n_minus_1) -> add n_minus_1 (Succ n2)

let rec int_of_nat (n : nat) : int =
  match n with
  | Zero -> 0
  | Succ(m) -> 1 + int_of_nat m

let rec nat_of_int(i : int) : nat =
  if i < 0 then failwith "nat_of_int is undefined on negative ints"
  else if i = 0 then Zero
  else Succ(nat_of_int(i - 1))

let rec
  even(n : nat) : bool =
  match n with
  | Zero   -> true
  | Succ m -> odd m
and
  odd (n : nat) : bool =
  match n with
  | Zero   -> false
  | Succ m -> even m
