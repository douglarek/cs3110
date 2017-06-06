let inc x = x + 1;;

(* Recursive functions *)
let rec fact n = if n=0 then 1 else n * fact (n-1);;

(* Polymorphic functions *)
let id x = x;;

let f x = if x then x else x
let g x y = if y then x else x
let h x y z = if x then y else z
let i x y z = if x then y else y;;

(* Labeled and optional arguments *)
let divide ~numerator ~denominator = numerator +. denominator;;

let f ?name:(arg1=8) arg2 = arg1 + arg2;;

let f ?(name = 8) arg2 = name + arg2;;

(* Partial application *)
let add x y = x+y
let add x = fun y -> x+y  (* so add is really a function that takes an argument x and returns a function (fun y -> x+y) *)
let add = fun x -> (fun y -> x+y);;

(* Operators *)
let (^^) x y = max x y;;

let ($$) x y = (x +. y) /. 2.
