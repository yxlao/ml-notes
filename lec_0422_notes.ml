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

let rec map f xs =
  match xs with
    | [] -> []
    | x::xs' -> (f x)::(map f xs')


(* consume two elements from a list *)
let rec map f xs = match xs with
  | x1 :: x2 :: rest -> f (x1, x2) :: map f (x2 :: rest)
  | _                -> []


let rec map f xs = match xs with
  | x1 :: x2 :: rest -> f (x1, x2) :: map f (x2 :: rest)
  | x1 :: []         -> []
  | []               -> []

map (fun (x,y) -> x + y) [1;2;3];;

(* 1 *)
let rec len xs = match xs with
  | [] -> 0
  | h::t -> 1 + len t

(* 2 *)
(* my non tail recursive version *)
let rec sumList xs = match xs with
  | [] -> 0
  | h::t -> h + sumList t

(* todo: tail recursive version *)
(* let rec sumList xs =
  let rec helper curr_sum xs = *)

(* 3 *)
let rec concat xs = match xs with
  | [] -> ""
  | h::t -> h ^ concat t

(* mix 1 2 3 *)
(* difference: base case and operator *)

let rec fold_right op vase xs = match xs with
  | [] -> base
  | h::t -> op h (fold_right op base t)

let concat xs = foo (fun s1 s2 -> s1 ^ s2) "" xs
let sum xs = foo (fun n1 n1 -> n1 + n2) 0 xs
let len xs = foo (fun n1 n2 -> 1 + n2) 0 xs

(* why fold_right -> rolling carpet from the right *)
fold_right op b [x1; x2; x3; x4; x5]
  ===> op x1 (fold_right op b [x2...x5])
    ===> op x1 (op x2 (fold_right op b [x3...x5]))
      ===> op x1 (op x2 (op x3 (fold_right op b [x4...x5])))
        ===> op x1 (op x2 (op x3 (op x4 (fold_right op b [x5]))))
          ===> op x1 (op x2 (op x3 (op x4 (op x5 b))))
