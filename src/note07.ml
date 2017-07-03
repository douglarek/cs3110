(* Modules *)
(* Structures and modules *)
(* module ListStack = struct *)
(*   let empty = [] *)
(*   let is_empty s = (s = []) *)

(*   let push x s = x :: s *)

(*   let peek = function *)
(*     | [] -> failwith "Empty" *)
(*     | x::_ -> x *)

(*   let pop = function *)
(*     | [] -> failwith "Empty" *)
(*     | _::xs -> xs *)
(* end *)

(* Opening a module *)
(* module M = struct *)
(*   let x = 42 *)
(*   type t = bool *)
(*   exception E *)
(*   module N = struct *)
(*     let y = 0 *)
(*   end *)
(* end *)

(* let x = M.x *)
(* type t = M.t *)
(* type exn += E = M.E *)
(* module N = M.N *)

(* Opening a module in a limited scope *)
let s = "BigRed"
let s' = s |> String.trim |> String.lowercase_ascii
let s' = String.(s |> trim |> lowercase_ascii)

(* Signatures and module types *)
(* module type Stack = sig *)
(*   type 'a stack *)
(*   val empty    : 'a stack *)
(*   val is_empty : 'a stack -> bool *)
(*   val push     : 'a -> 'a stack -> 'a stack *)
(*   val peek     : 'a stack -> 'a *)
(*   val pop      : 'a stack -> 'a stack *)
(* end *)

(* module ListStack : Stack = struct *)
(*   type 'a stack = 'a list *)
(*   let empty = [] *)
(*   let is_empty s = (s = []) *)

(*   let push x s = x :: s *)

(*   let peek = function *)
(*     | [] -> failwith "Empty" *)
(*     | x::_ -> x *)

(*   let pop = function *)
(*     | [] -> failwith "Empty" *)
(*     | _::xs -> xs *)
(* end *)

(* module MyStack : Stack = struct *)
(*   type 'a stack = *)
(*     | Empty *)
(*     | Entry of 'a * 'a stack *)

(*   let empty = Empty *)
(*   let is_empty s = s = Empty *)
(*   let push x s = Entry(x, s) *)
(*   let peek = function *)
(*     | Empty -> failwith "Empty" *)
(*     | Entry(x, _) -> x *)
(*   let pop = function *)
(*     | Empty -> failwith "Empty" *)
(*     | Entry(_, s) -> s *)
(* end *)

module type Stack = sig
  type 'a stack
  val empty    : 'a stack
  val is_empty : 'a stack -> bool
  val push     : 'a -> 'a stack -> 'a stack
  val peek     : 'a stack -> 'a
  val pop      : 'a stack -> 'a stack
  val format : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a stack -> unit
end

module ListStack : Stack = struct
  type 'a stack = 'a list
  let empty = []
  let is_empty s = (s = [])

  let push x s = x :: s

  let peek = function
    | [] -> failwith "Empty"
    | x :: _ -> x

  let pop = function
    | [] -> failwith "Empty"
    | _ :: xs -> xs

  let format fmt_elt fmt s =
    Format.fprintf fmt "[";
    (* List.iter (fun elt -> Format.fprintf fmt "%a; " fmt_elt elt) s; *)
    List.iter (fun elt -> Format.fprintf fmt "" ) s;
    Format.fprintf fmt "]"
end

(* Example: Arithmetic *)
module type Arith = sig
  type t
  val zero  : t
  val one   : t
  val (+)   : t -> t -> t
  val ( * ) : t -> t -> t
  val (~-)  : t -> t
  val to_string : t -> string
end

module Ints : (Arith with type t = int) = struct
  type t = int
  let zero = 0
  let one = 1
  let (+) = Pervasives.(+)
  let ( * ) = Pervasives.( * )
  let (~-) = Pervasives.(~-)
  let to_string = string_of_int
end

module IntsAbstracted : Arith = Ints

module IntsExposed : (Arith with type t = int) = Ints
