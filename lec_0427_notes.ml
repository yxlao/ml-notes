(* phone book: why need to save the old value *)

(* ocaml *)
(* whenever a function is called, always return the same answer *)
(* static lexical scoping, static scoping
   the definition of x that is most recent use point of the program *)

let x = 0
let y = x + 1
let f a = (x, y)
let x = 100
let res = f [] (* (0,1) *)

(*
# python
x = 0
y = x + 1
def f(a): return (x, y)
x = 100
print f(123) # (100, 1)
*)

let f x = 1
let f x = if x < 2 then 1 else (x * f(x-1))
let res = f 5

let rec funnyList = 1 :: funnyList;;
let rec length xs = match xs with
  | [] -> 1
  | h::t -> 1 + length t
length funnyList (* stack overflow *)
