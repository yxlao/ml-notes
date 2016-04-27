let rec fold_right op base xs = match xs with
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

(* making fold right tail recursive *)
concat [] = ""
concat ["dog"; "cat"] = "dogcat"

let rec concat xs = match xs with
  | [] -> ""
  | h::t -> h ^ concat t

pocket0 = ""
pocket1 = pocket0 + "dog" => "dog"
pocket2 = pocket1 + "cat" => "dogcat"


let concat xs =
  let rec helper pocket ws = match ws with
    | [] -> pocket
    | h::t -> let newPocket = pocket ^ h in
              helper newPocket t
  in
    helper "" xs

let sum xs =
  let rec helper pocket ws = match ws with
    | [] -> pocket
    | h::t -> let newPocket = pocket + h in
              helper newPocket t
  in
    helper 0 xs

let fold_right op base xs =
  let rec helper pocket ws = match ws with

let x = 4
let foo n = n + x
foo 0 (* 4 *)
let y = x + x
let z = [x; y; x * y]
let x = 8
foo 0 (* 4 *)
