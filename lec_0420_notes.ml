(* range 2 3 = 2::[] *)
let rec range i j =
  if i >= j then []
  else i::(range(i+1) j)

(* use pocket *)
let range i j =
  let rec helper pocket i j =
    if i > j then pocket
    else let pocket' = i :: pocket in
         helper pocket' (i+1) j
  in
    helper [] i j

let range i j =
  let rec helper pocket i j =
    if i > j then pocket
    else let pocket' = i :: pocket in
         helper pocket' (i+1) j
  in
    List.rev (helper [] i j)

let _ = range 0 3

(* higher order functions: get rid of recursion, important point of ML *)

let isEven n = n mod 2 = 0

let rec evens xs = match xs with
  | [] -> []
  | h::t -> let rest = evens t in (* rest is a local veriable *)
            if isEven h then h :: rest else rest

let isFourLetters s = String.length s = 0

let rec forLetters xs = match xs with
  | [] -> []
  | h::t -> let rest = fourletters t in
            if isFourLetters h
            then h :: rest
            else rest

(* add a paramter to pass in condition function for different types *)
let rec filter test xs = match xs with
  | [] -> []
  | h::t -> let rest = test t in
            if test h
            then h :: rest
            else rest

let evens = filter isEven
let forLetters = filter isFourLetters


let square x = x * x

(* listUpper *)
let rec listUpper xs = match xs with
  | [] -> []
  | h::t -> (String.uppercase h) :: (listUpper t)

(* listSquare *)
let rec listSquare xs = match xs with
  | [] -> []
  | h::t -> (square h) :: listSquare(t)

let rec listMap f xs = match xs with
  | [] -> []
  | h::t -> (f h) :: (listMap f t)

let listUpper = listMap String.uppercase
let listSquare = listMap square

let inc x = x + 1
let zinc y = inc y
zinc (* 10 = 11 *)

(* equivalent *)
let f a = g a
let f = g

let f a b = g a
let f = g


let rec listMap f xs = match xs with
  | [] -> []
  | h::t -> (f h) :: (listMap f t)

let listLen = listMap String.length
