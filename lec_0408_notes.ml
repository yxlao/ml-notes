let isEven x   = x mod 2 = 0

let lt x y = x < y

let negate f x = not (f x)

let negate f   = fun x -> not (f x)

let isOdd = negate isEven
               (* = fun x -> not (isEven x) *)

let is5gte = negate is5lt


filter isEven [1;2;3;4;5;6] = [2; 4; 6]

filter (fun y -> y >= 5) [1;2;3;4;5;6] = [5;6]

let rec filter cond l = match l with
  | []      -> []
  | (x::xs) -> let rest = filter cond xs in
               if cond x then x :: rest else rest

partition isEven [1;2;3;4;5;6] = ([2;4;6], [1;3;5])

partition (fun y -> y >= 5) [1;2;3;4;5;6] = ([5; 6], [1; 2; 3; 4])

let partition cond l = (filter cond l, filter (negate cond) l)


let rec sort l = match l with
  | []     -> []
  | h :: t -> let (smalls, bigs)  = partition (fun y -> y <= h) t in
              (sort smalls) @ (h :: (sort bigs))



if c
  then 1 :: [2;3;4;5]
  else      [2;3;4;5]


(* 1 *)
(if c then [1] else []) @ [2;3;4;5]

(* 2 *)
let tail = [2;3;4;5] in
if c then 1 :: tail else tail
i

(* one of type *)
type attrib =
  | Name of string (* tagged Name, is global *)
  | Age of int
  | DOB of int*int*int
  | Address of string
  | Height of float
  | Alive of bool
  | Phone of int*int
  | Email of string
  | Dummy (* Dummy of () *)

type bob =
  | Rage


type meag = Bob of bob | Attrib of attrib

[Dummy; Name "Mickey"; Age 56];;


let attribString a = match a with
  | Name n -> Printf.sprintf "name = %s" n
  | Age i -> Printf.sprintf "age = %d" i
  | _      -> 100 (* doesn't work, all branch value must be the same type *)

if true then 1 else "dummy" (* error, different types *)

let attribString a = match a with
  | Rage -> Printf.sprintf "RAGE" (* can not come from two types *)
  | Name n -> Printf.sprintf "name = %s" n
  | Age i -> Printf.sprintf "age = %d" i
  | _      -> "i am lazy!"

let attribString a = match a with
  | Rage -> Printf.sprintf "RAGE" (* can not come from two types *)
  | Name n -> Printf.sprintf "name = %s" n
  | Age i -> Printf.sprintf "age = %d" i
  | Age i -> Printf.sprintf "age = %d" i (* unused warning *)
  | _      -> "i am lazy!"

let megaString a = match a with
  | Bob    (Rage)   -> Printf.sprintf "RAGE" (* can not come from two types *)
  | Attrib (Name n) -> Printf.sprintf "name = %s" n
  | Attrib (Age i)  -> Printf.sprintf "age = %d" i
  | _               -> "i am lazy!"

let welcome a = match a with | Name s -> s
  in welcome (Name "Ranjit")

(* *)
type mystery = () (* "unit": tuple of length zero *)
type one_int = (int)
type int_int = (int * int)
type int_int_int = (int * int * int)

(* 'a -> 'a = <fun> *)
let aFuncPloy x = x;;

(* <A> A aFunc(x:A) {return x;} // java *)
let aFuncInt (x:int) : int = x;;
let aFuncInt (x:int) = x;; (* qeuivalent *)

(* mystery func *)
let mysteryFun (x:mystery) = x;; (* can only called with mysteryFun () *)

(* recursive *)
type nat = Zero
         | PlusOne of nat

let x0 = Zero
let x1 = PlusOne x0
let x2 = PlusOne x1
let x3 = PlusOne x2

(* nat -> int *)
(*
toInt Zero = 0
toInt (PlusOne Zero) = 1
toInt (PlusOne (PlusOne Zero)) = 2
toInt (PlusOne (PlusOne (PlusOne Zero))) = 3
*)
let rec toInt n = match n with
  | Zero      -> 0
  | PlusOne m -> 1 + toInt m

let rec factorial n = match n with
  | 1 -> 1
  | n -> n * factorial (n-1)

(* don't want recursive *)
let cook x =
  let x = clean x in
  let x = peel x in (* the RHS x is the same as LHS x in the previous line *)
  let x = chop x in
  let x = boil x in
  let dish = fry x in
  dish

let rect toNat n = match n with
  | 0 -> Zero
  | n -> PlusOne (toNat (n-1))

(* add Zero Zero = Zero
add Zero (PlusOne Zero) = PlusOne Zero
add Zero (PlusOne (PlusOne Zero)) = PlusOne (PlusOne Zero)
add (PlusOne Zero) (PlusOne (PlusOne Zero)) = PlusOne (PlusOne (PlusOne Zero)) *)

(* let rec add x y = match (x, y) with
 | Zero, Zero -> Zero
 | Zero, m -> m
 | m, Zero -> m *)

(* add (PluseOne (PluseOne Zero)) m = PluseOne (PluseOne m) *)

add Zero m = m
let rec add n m = match n with
  | Zero -> m
  | PluseOne n -> PluseOne (add n m)
