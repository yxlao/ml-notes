
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
  | Bob    (Rage)   -> Printf.sprintf "RAGE"
  | Attrib (Name n) -> Printf.sprintf "name = %s" n
  | Attrib (Age  i) -> Printf.sprintf "age  = %d" i
  | _      -> "i am lazyyyy!"

let welcome a =
  match a with
  | Name s -> s

welcome (Name "Ranjit")


let _ = attribString (Age 7)
(*
 A. "i am lazyyy..."
 B. Type ERROR (in attribString)
 C. "Rage" is undefined
 * )

(*
   <A> A aFunc(x:A) { return x; }
*)

let aFuncPoly x = x

(*
   int aFunc(x:int) { return x; }
*)

let aFuncInt (x:int) : int = x



type mystery     = ()    (* "unit" *)
type one_int     = int
type int_int     = (int * int)
type int_int_int = (int * int * int)

let int_fun (x : int) = x
let int_int_fun (x : int_int) = x
let int_int_int_fun (x : int_int_int) = x

let mystery_fun (x : mystery) = x

(*
Which of the following will typecheck?

A. mystery_fun 6
D. mystery_fun "cat"

B. mystery_fun (6, 3)
C. mystery_fun (6, 3, 92)
E. mystery_fun ()

*)
