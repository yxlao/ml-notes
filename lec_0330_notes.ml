(* tuple *)
(2+2, 7>8);;

(* tuple can contain different types *)
let y = ("cat", 12, 9.8);;

(* head tail matching, h is element, t is list *)
let (h::t) = [1;2;3;4] ;;

(* ways to define a list *)
let myList = [1;2;3;4] ;;

(* list concat with empty list *)
( 1 :: 2 :: 3 :: 4 :: []);;

(* matching *)
let bob = 
  match myList with
    | []     -> 0
    | (h::t) -> h
;;

let greet name = 
  if name = "ranjit" then
    "boss!"
  else
    "something rude"
;;

let greet name = 
  if (name <= "ranjit") then
    "boss!"
  else
    "something rude"
;;

let greet name = 
  match name with
    | "ranjit" -> "boss!"
    | _        -> "something else"
;;

let greet name = 
  match (name <= "ranjit") with
    | true -> "boss!"
    | _    -> "something else"
;;

(*if cond then true else false -> cond*)

(* What is 

   if e1 then e2 else e3

   as a pattern match?

   match e1 with
   | true  -> e2
   | false -> e3

*)

(* What is the value of `bob`? 
   a. 0
   b. 1
   c. type error
   d. syntax error
   e. []
*)


(* fun x_param -> e_body *)

(* var inc = function(x){ return x + 1;} *)

(* define a function without name *)
fun x -> x + 1;; (* all function one input one output *)

let _ = (fun x -> x + 10) 10;;

let incr = fun x -> x + 10;;
incr 1;;

(* equivalent following *)
let add x y = x + y;;
let add x   = fun y -> x + y;;
let add     = fun x y -> x + y;;
let add     = fun x -> (fun y -> x + y);;


(int -> int) -> int -> int

(int -> int) -> (int -> int)

(fun f -> (fun x -> (f x) + x))

let foo = fun x1 -> fun x2 -> fun x3 -> x1 + x2 + x3 
   foo: int -> int -> int -> int
        x1  -> x2  -> x3  -> result
   foo: int -> (int -> (int -> int))

let foo = fun x1 -> fun x2-> x1 + x2
   foo: int -> int -> int
        x1  -> x2  -> result

(* Fun1 :: int -> int -> int *)
let fun1 = fun x1 -> fun x2 -> x1 + x2;;
(* Fun2 :: (int -> int) -> int *)
let fun2 = fun f -> (f 0) + 7;;

(* automatic determine type *)
let _ = (fun x -> not x);; (* bool -> bool *)

(* val negate : ('a -> bool) -> 'a -> bool = <fun> *)
let negate = fun f -> fun x -> not (f x);;
let negate = fun f x -> not (f x);;
let negate f = fun x -> not (f x);;
let isEven x = x mod 2 = 0;;
let isOdd = negate isEven;;
let isOdd = fun x -> not (isEven x)

let lt = fun x y -> x < y;;
let is5lt = lt 5;;
is5lt 10;;

(* filter isEven [1;2;3;4;5;6] => [2;4;6], returns even *)
(* filter (fun y -> y >= 5) [1;2;3;4;5;6] => [5;6], returns odd *)

let rec filter cond l = match l with
  | []      -> []
  | (x::xs) -> let rest = filter cond xs in 
                 if cond x then x :: rest else rest;;

(* 1 *)
(if c then [1] else []) @ [2,3,4,5]
(* 2 *)
let tail = [2,3,4,5] in

(* @ operator speed propotional to the size of the left *)
[] @ [1;2;3;4]
[1;2;3;4;5] @ [1;2;3;4]

(* partition isEven [1;2;3;4;5;6] = ([2;4;6], [1;3;5]) *)
let partition cond l = (filter cond l, filter (negate cond) l);;

(* [x1; x2; x3; x4 ...]
smallers = [elements in tail that are less than x1]
greaters = [elements in tail that are greater than x1]
(sort smallers) x1 (sort graters) *)

let rec sort l = match l with
  | [] -> []
  | h :: t -> let smallers = filter (fun y -> y <= h) t in
              let greaters = filter (fun y -> y > h) t in
              (sort smallers) @ ((h :: sort greaters));;

let rec sort l = match l with
  | [] -> []
  | h :: t -> let (smalls, bigs) = partition (fun y -> y > h) t in
              (sort smallers) @ ((h :: sort bigs));;

(* ocamls only overloads comparision and equality *)

type person = {name: string; age: int};;
let rj = {name = "Ranjit"; age = 37};;
let rjNext = {rj with age = 38};;


type attrib = Name of string
              | Age of int
              | Height of float;;

let firstEltOrZero = fun l -> match l with 
  | [] -> 0
  | h::t -> h
;;

let first2 = fun l -> match l with 
  | h1::h2::_ -> (h1, h2)
  | _  -> (0,0)
;;

first2 [1];;


let _ = firstEltOrZero [];;
let _ = firstEltOrZero [1;12;12];;


let x1 = 1 in
let x2 = 2 in
let x3 = 3 in
  x1 + x2 + x3;;

(*
EXPR :=
| 0,1,2,3, ...
| x, y, z, ...
| EXPR + EXPR ...
| let x = EXPR in EXPR ...
*)

let _ = 2 + + + + 3;;


let _   = (fun x -> x + 1) 10 ;;

let inc = (fun x -> x + 1);; (* "ANONYMOUS FUNCTION" *)

let _   = inc 100;;
let _   = inc 200;;
let _   = inc 10413;;

let add4 = fun (x, y,z,a) -> x + y + z +a;;

let _ = add2 (100, 200);;

let add2 = fun x -> (fun y -> x + y) ;;

let _ = (add2 10) 20;;

let incr = add2 1 ;;

let _ = incr 100 ;;
let _ = incr 200 ;;





(* 

"CHAINING"
"CURRY"

*)

let isEven = fun x -> (x mod 2) = 0;;


let inc3       = fun x -> fun y -> fun z -> x + y + z  ;;
let inc3       = fun x y z -> x + y + z  ;;
let inc3 x y z = x + y + z  ;;

let neg     = fun f -> fun x -> not (f x) ;;
let neg     = fun f x -> not (f x);;
let neg f x = not (f x) ;;


let isOdd = neg isEven
;;

let _ = isEven 10;;
let _ = isEven 1;;

let _ = isOdd 10;;
let _ = isOdd 1;;

let isBiggerThan100 = (fun x -> x > 100) ;;
let foo = neg isBiggerThan100 ;;

let _ = foo 5 ;;

(* filter isEven [1;2;3;4;5;6;7;8] = [2;4;6;8] *)

let rec filterEven xs = match xs with
  | []   -> 
      []
  | h::t -> 
      let rest = filterEven t in
        if isEven h then h :: rest else rest
;;

filterEven [1;2;3;4;5;6];;

(* STOP *)













                              
(9-3,"ab"^"cd",(2+2 ,7>8));;
                               
[];;

[1;2;3];;

[1+1;2+2;3+3;4+4];;

["a";"b";"c"^"d"];;

[[1];[2;3];[4;5;6]];;

[1;"pq"];;













                              
1::[];;
                               
1::[2];;
                               
"a"::["b";"c"];;
                               
1::["b";"cd"];;
 
 
 
 
 
 
 
 
 
                               
[1;2]@[3;4;5];;
                               
["a"]@["b"];;
                               
[]@[1];;
                               
1@[2;3];;
                               
[1]@["a";"b"];;
 
 
 
                               
let x = 2 + 2;;
                               
let y = x * x * x;;
                               
let z = [x;y;x+y];;
                               
let p = a + 1;;
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
let tempVar = x + (2 * y) in
tempVar * tempVar;;

tempVar;;
 
 
                               
let y = 
  let tempVar = x + (2 * y) in
    tempVar * tempVar
;;
                               
tempVar;;
 
 
let g = 2 in 
let h = 1 in
g + h;;
 
 
 
 
 
 
 
 
 
 
 
 
 
                               
let (x,y,z) = (2+3,"a"^"b", 1::[2]);;


let (2+3,"a"^"b", 1::[2]) = (x,y,z);;
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
                               
let h::t = [1;2;3];;
 
                               
let l = [1;2;3];;
                               
let h::t = l;;
 
(* if l is empty, that is the reson for warning *)
 
let (h1 :: h2 :: t) = [1; 2; 3; 4];;
let (h1 :: h2 :: t) = [1; 2; 3];;
let (h1 :: h2 :: t) = [1; 2];;

(*good func def*)
let random x = 
  let (h1::h2::t) = [x; x] in
    print_string "hello!"
;;
(*good func exec*)
let _ = random 1;;

(*good func def*)
let random x = 
  let (h1::h2::t) = [x] in
    print_string "hello!"
;;
(*not good func exec*)
let _ = random 1;;

let add x y = x + y;;
let _ = add 10 12;;

let first xs = 
  let (h::t) = xs in
    h
;;
let _ = first ["car"; "dog"; "mouse"];;
let _ = first [];; (* error *)

let first xs = 
  match xs with
    | [] -> -1
    | (h::t) -> h;;
;;

let _ = first ["cat"; "dog"; "mouse"];;
let _ = first [];;

let first xs = 
  match xs with
    | [] -> []
    | (h::t) -> h
;;

let first xs = 
  match xs with
    | [] -> failwith "don't call first with empty list"
    | (h::t) -> h
;;


let first d xs = 
  match xs with
    | []     -> d
    | (h::t) -> h
;;
 

let _ = first (-1) [];;
let _ = first "" ["cat"; "dog"; "mouse"];;
 
                               
let partition f xs = (filter f xs, filter (negate f) xs)
 
 
 
 
 
                               
let rec filter f xs = match xs with
  | []     -> []
  | x::xs' -> if f x then x :: (filter f xs') else (filter f xs')
;;
 
 
                               
let rec filter f xs = match xs with
  | []     -> []
  | x::xs' -> if f x then x :: (filter f xs') else (filter f xs')
;;
 
 
 
 
 
                               
let rec sort l = 
  match l with [] -> []
             | (h::t) ->
                 let (l,r) = partition ((<) h) t in
                   (sort l)@(h::(sort r))
;;
 
 
 
 
                               
sort list1;;
                               
sort ["p";"a";"z";"q";"r";"b"];;
 
 
                               
type myrec = {name : string; age : int; pass : bool};;
 
                               
let r = {name="ranjit"; age=27; pass=false};;
                               
r.age ;;
                               
r.name ;;
                               
r.pass ;;
 
                               
r.grade ;;
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
                               
hd [1;2;3;4;5];
tl [1;2;3;4;5];
                               
hd ["a";"b";"cd"];
tl ["a";"b";"cd"];
 
                               
hd [(1,"a");(7,"c")];
tl [(1,"a");(7,"c")];
                               
hd [[];[1;2;3];[4;5]];
tl [[];[1;2;3];[4;5]];
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
                               
(hd [[];[1;2;3]]) = (hd [[];["abc"]]);
hd [[];[1;2;3]];
hd [[];["abc"]];
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
                               
(* HERE HERE HERE 

   1. Int, String, Float

   2. Tuples, Lists

   3. +,-, ^, ::, hd, tl, @, [], etc.

   Features

   . IF-THEN-ELSE
   . Variables 
   . Functions
   . Recursion

   C. Loops (NO)
   D. Objects (definitely not)

*)

let _ = if (1<2) then true else false ;;
let _ = if (1<2) then [1;2;3] else [4;5] ;;
let _ = if (1<2) then "[1;2;3]" else "[4;5]" ;;


(* if e then true else false ========= e *)

(* eb ? e1 : e2 *)


      
      
      
      
                               
      
if (1 < 2) then ["ab";"cd"] else ["x"];
                               
let x = 1 ;;
let y = 2 ;;
                               
if ( x < y) 
then if (x + y > 10) then [x] else [y]
else [];;
                               
if (1 < 2) then [1;2] else 5;
if false then [1;2] else 5;
                               
 
(if (1 > 2) then 0 else 1) + (if (x =y) then 10 else 11);
 
(if (1 > 2) then 0 else 1)::[];
 
(* but be careful *)
 
if 1>2 then [1;2] else [] ;;
 
if 1<2 then [] else ["a"];;
 
(* and so ... *)

(if 1>2 then [1;2] else []) = (if 1<2 then [] else ["a"]) ;;

(* LEC 2 END