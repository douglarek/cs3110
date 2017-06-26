(* Higher-order functions *)
let double x = 2 * x
let square x = x * x
let quad x = double (double x)
let fourth x = square (square x)

let twice f x = f (f x)

let quad x = twice double x
let fourth x = twice square x

(* The Abstraction Principle *)
(* Other basic higher-order functions *)

(* Apply *)
let apply f x = f x

(* Pipeline *)
let pipeline x f = f x
(* let (|>) = pipeline *)
let x = 5 |> double (* 10 *)

(* Compose *)
let compose f g x = f (g x)
let square_then_double = compose double square
let x = square_then_double 1
let y = square_then_double 2

(* Both *)
let both f g x = (f x, g x)
let ds = both double square
let p = ds 3

(* Cond *)
let cond p f g x =
  if p x then
    f x
  else
    g x

(* Map *)
let rec add1 = function
  | [] -> []
  | h :: t -> (h + 1) :: (add1 t)

let rec concat3110  = function
  | [] -> []
  | h :: t -> (h ^ "3100") :: (concat3110 t)

let rec map f = function
  | [] -> []
  | h :: t -> (f h) :: (map f t)

let add1 = map (fun x -> x + 1)
let concat3110 = map (fun x -> x ^ "3110")

(* Filter *)

let rec events = function
  | [] -> []
  | h :: t -> if even h then h :: (events t) else events t
and even n = n mod 2 = 0

let rec filter f = function
  | [] -> []
  | h :: t -> if f h then h :: (filter f t) else filter f t

let events = filter even

(* Fold right *)
let rec sum  = function
  | [] -> 0
  | h :: t -> h + sum t

let rec concat = function
  | [] -> ""
  | h :: t -> h ^ concat t

let rec sum' init = function
  | [] -> init
  | h :: t -> h + sum' init t

let sum = sum' 0

let rec concat' init = function
  | [] -> init
  | h :: t -> h ^ concat' init t

let concat = concat' ""

let rec combine op init = function
  | [] -> init
  | h :: t -> op h (combine op init t)

let sum = combine (+) 0

let concat = combine (^) ""

let rec fold_right op lst init = match lst with
  | [] -> init
  | h :: t -> op h (fold_right op t init)

let sum lst = fold_right (+) lst 0
let concat lst = fold_right (^) lst ""

(* Fold left *)
let rec fold_left op acc = function
  | [] -> acc
  | h :: t -> fold_left op (op acc h) t

let rec sum' acc = function
  | [] -> acc
  | h :: t -> sum' (acc + h) t

let sum = sum' 0

let sum = List.fold_left (+) 0
let concat = List.fold_left (^) ""

(* Fold left vs fold right *)
let (--) i j =
  let rec from  i j l =
    if i > j then l
    else from i (j - 1) (j :: l)
  in from i j [];;

List.fold_left (-) 0 (List.rev (0--1_000_000));;
(* List.fold_right (-) (List.rev (0--1_000_000)) 0;; *) (* Stack overflow *)

(* Folding is powerful*)
let length l = List.fold_left (fun a _ -> a + 1) 0 l
let rev l = List.fold_left (fun a x -> x :: a) [] l
let map f l = List.fold_right (fun x a -> (f x) :: a) l []
let filter f l = List.fold_right (fun x a -> if f x then x :: a else a) l []

(* Pipelining *)
let sum_sq n =
  0--n
  |> List.map square
  |> sum

let sum_sq n =
  0--n
  |> List.rev_map square
  |> sum
