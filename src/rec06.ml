(* Higher-order functions *)
(* twice, no arguments *)
let double x = 2*x
let square x = x*x
let twice f x = f (f x)
let quad = twice double (* int -> int *)
let fourth = twice square

(* mystery operator 1 *)
let ($) f x = f x (* square $ 2 + 2 -> 16; square 2 + 2 -> 6 *)

(* mystery operator 2 *)
let (@@) f g x = x |> g |> f

(* repeat *)
let rec repeat f n x =
  if n < 0 then
    failwith "n must be positive"
  else if n = 0 then
    x
  else repeat f (n -1) (f x)

(* Map, fold, and filter *)
(* product *)
let product_left = List.fold_left ( *. ) 1.0
let product_right lst = List.fold_right ( *. ) lst 1.0

(* sum_cube_odd *)
let sum_cube_odd n =
  let lst = let open Note06 in 0--n in
  lst
  |> List.filter (fun x -> x mod 2 <> 0)
  |> List.map (fun x -> x * x * x)
  |> List.fold_left (+) 0

(* Three ways *)
(* exists *)
let rec exists_rec p = function
  | [] -> false
  | h :: t -> p h || exists_rec p t

let exists_fold p =
  List.fold_left (fun acc elt -> acc || p elt) false

let exists_lib p =
  List.exists p

(* Currying *)
(* library uncurried *)
let uncurried_append lst1 lst2 = List.append lst1 lst2
let uncurried_compare c1 c2 = Char.compare c1 c2
let uncurried_max a b = max a b

(* uncurry *)
let uncurry f (a, b) =  f a b

(* curry *)
let curry f a b = f (a, b)

(* Additional exercises *)
(* map composition *)
let double_map f g lst =
  lst
  |> List.map (f @@ g)

(* more list fun *)
let elements_filter = List.filter (fun x -> String.length(x) > 3)
let elements_add = List.map (fun x -> x +. 1.0)
let join_strings strs sep = List.fold_left (fun acc elt -> if acc = "" then elt else acc ^ sep ^ elt) "" strs

(* tree map *)
type 'a tree =
  | Leaf
  | Node of 'a * 'a tree * 'a tree

let rec tree_map f = function
  | Leaf -> Leaf
  | Node (x, l, r) -> Node (f x, tree_map f l, tree_map f r)

let add1 = tree_map (fun x -> x + 1)
(* let a = Node(10, Node (2, Leaf, Leaf), Leaf);; *)
(* add1 a *)

(* association list keys *)
let keys lst = let (a, _) = List.split lst in
  a
  |> List.sort_uniq compare
(* a better way *)
let keys lst = lst |> List.split |> fst |> List.sort_uniq compare

(* Challenge exercises: Matrices *) (* TODO:  *)
