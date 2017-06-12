(* Let expressions *)
let x = 42;;

let x = 42 in x + 1;;

(* (let x = 42) + 1;; *)

(let x = 42 in x) + 1

(* Variants *)

type day = Sun | Mon | Tue | Wed | Thu | Fri | Sat

let d : day = Tue

let int_of_day = function
  | Sun -> 1
  | Mon -> 2
  | Tue -> 3
  | Wed -> 4
  | Thu -> 5
  | Fri -> 6
  | Sat -> 7

type ptype = TNormal | TFire | TWater

type peff = ENormal | ENotVery | ESuper

(* Records *)
type mon = {name : string; hp : int; ptype : ptype}

let c = {name = "Charmander"; hp = 39; ptype = TFire};;

c.hp;;

match c with
| {name = n; hp = h; ptype = t} -> h;;

match c with
| {name; hp; ptype} -> hp;;

match (1, 2, 3) with
| (x, y, z) -> x + y + z;;

(* Several ways to get a Pokemon's hit points *)
(* OK *)
let get_hp m =
  match m with
  | {name = n; hp = h; ptype = t} -> h

(* better *)
let get_hp m =
  match m with
  | {name = _; hp = h; ptype = _} -> h

(* better *)
let get_hp m =
  match m with
  | {name; hp; ptype} -> hp

(* better *)
let get_hp m =
  match m with
  | {hp} -> hp

(* best *)
let get_hp m = m.hp

(* Several ways to get the 3rd component of a tuple *)

(* OK *)
let thrd t =
  match t with
  | (x, y, z) -> z

(* good *)
let thrd t =
  let (x, y, z) = t in z

(* better *)
let thrd t =
  let (_, _, z) = t in z

(* best *)
let thrd (_, _, z) = z
