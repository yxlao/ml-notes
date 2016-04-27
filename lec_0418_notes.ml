(*
type expr =
  "value" == 4.0, 2.9, ...
  "operation" == "over two exprs"
*)

(* type op = PlusOp | MinusOp | TimesOf | DivOp *)

type expr = Value   of float
          | PlusOp  of expr * expr
          | MinusOp of expr * expr
          | TimesOp of expr * expr
          | DivOp   of expr * expr

(* 4.0 * 2.9 *)
let e0 = Value 4.0
let e1 = Value 2.9
let e2 = PlusOp (e0, e1)

(* 3.78 * 5.92 *)
let e0' = Value 3.78
let e1' = Value 5.92
let e2' = PlusOp (e0', e1')

let e3 = TimesOp (e2, e2')


let rec eval (e:expr) : float = match e with
  | Value   v -> v
  | PlusOp  (e1, e2) -> (eval e1) +. (eval e2)
  | MinusOp (e1, e2) -> (eval e1) -. (eval e2)
  | TimesOp (e1, e2) -> (eval e1) *. (eval e2)
  | DivOp   (e1, e2) -> (eval e1) /. (eval e2)

(* Higher order functions *)

(*
Tail recursion
> The last thing that the function does is the recursive call.
> The function does not use the returned value of the recursive call in anyway.
> If the last thing is dummy ops, not counted as tail recursion.

Ocaml will convert tail recursion to a for loop
*)

(* original *)
let rec fac n = match n with
  | 0 -> 1
  | n -> n * fac (n-1)

(* let fac n = helper 1 n *)
let fac n =
  let rec helper pocket n = match n with
    | 0 -> pocket
    | n -> helper (pocket*n) (n-1)
  in helper 1 n

(* original *)
let rec foo i j =
   if i >= j then []
   else i::(foo (i+1) j)

let foo i j =
  let rec helper pocket i j =
    if i >= j then
      List.rev pocket
    else
      helper (i :: pocket) (i+1) j
  in
    helper [] i j
