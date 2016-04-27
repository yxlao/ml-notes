
type attrib =
  | Name    of string
  | Age     of int
  | DOB     of int * int * int
  | Address of string
  | Height  of float
  | Alive   of bool
  | Phone   of int * int
  | Email   of string
  | Dummy

type bob =
  | Rage

type mega = Bob of bob | Attrib of attrib

let megaString a = match a with
  | Age i         -> Printf.sprintf "age  = %d" i
  | Name n        -> Printf.sprintf "name = %s" n
  | DOB (d, m, y) -> Printf.sprintf "day = %d, month = %d, year = %d" d m y
  | _             -> "i am lazyyyy!"

let welcome a =
  match a with
  | Name s -> s
  | Age  i -> i
  | _      -> "go away"


let _ = welcome (Name "Ranjit")


type nat = Zero
         | PlusOne of nat

let x0 = Zero
let x1 = PlusOne x0
let x2 = PlusOne x1
let x3 = PlusOne x2

let cook x =
  let x    = clean x in
  let x    = peel  x in
  let x    = chop x  in
  let x    = boil x  in
  let dish = fry x   in
  dish

let rec factorial n = match n with
  | 1 -> 1
  | n -> n * factorial (n-1)

let rec toInt n = match n with
  | Zero         -> 0
  | PlusOne m    -> 1 + toInt m

let rec foo n =
  if n <= 0 then Zero else PlusOne(foo(n-1))

foo 2
  ===> PlusOne( foo 1 )
  ===> PlusOne( PlusOne (foo 0))
  ===> PlusOne( PlusOne Zero )

let rec toNat n = match n with
  | 0 -> Zero
  | n -> PlusOne (toNat (n-1))


add Zero Zero                     = Zero
add Zero (PlusOne Zero)           = PlusOne Zero
add Zero (PlusOne (PlusOne Zero)) = PlusOne (PlusOne Zero)

add          (PlusOne Zero)  m =          PlusOne m
add (PlusOne (PlusOne Zero)) m = PlusOne (PlusOne m)

add Zero        m = m
add (PlusOne n) m = PlusOne (add n m)

let rec add n m = match n with
  | Zero          -> m
  | PlusOne thing -> PlusOne (add thing m)

let rec times n m = match n with
  | Zero -> Zero
  | PlusOne thing -> add (times thing m) m (* (thing + 1) * m = thing * m + m *)

let rec minus n m = match (n, m) with (* don't have negative *)
  | (n, Zero) -> n
  | ((PlusOne, n), (PlusOne m)) -> minus n m

(* lists are recursive types *)
type intList = Nil
             | Cons of int * intList

let emptyList = Nil (* [] *)
let emptyList = Cons (1, (Cons (2, Nil)));;

type 'a myList = Nil
               | Cons of 'a * 'a myList

let l = Nil
let l = Cons (1, (Cons (2, Nil)));;
let l = Cons ("one", (Cons ("dos", Nil)));;

(* list functions *)
length Nil = 0
length (Cons (_, Nil)) = 1
length (Cons (_, (Cons (_, Nil)))) = 2
length (Cons, (_, (Cons (_, (Cons (_, Nil)))))) = 3

let rec length xs = match  xs with
  | Nil -> 0
  | (Cons (_, m)) -> 1 + length m

(* function append two lists *)
appnd Nil m = m
append (Cons (h, t)) m = ?
append (Cons (1, Cons (2, Nil))) (Cons (3, Nil)) = Cons (1, Cons (2, Cons (3, Nil)))

append Nil ys = ys
append (Cons (1, Cons (2, Nil))) m = Cons (1, Cons (2, m))

let rec append xs ys = match xs with
  | Nil -> ys
  | Cons (x, tail) -> Cons (x, append tail ys)

(* append takes time propotional to the size of the first list *)
