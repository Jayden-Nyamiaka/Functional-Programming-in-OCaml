 (* A.1 *)
type point = {x: float; y: float}
type segment = {startp: point; endp: point}

let midpoint_segment {startp; endp} = 
  { x = (startp.x +. endp.x) /. 2. ; y = (startp.y +. endp.y) /. 2. }

let segment_length {startp; endp} = 
  sqrt ((startp.x -. endp.x) ** 2. +. (startp.y -. endp.y) ** 2.)

let print_point {x; y}= Printf.printf "(%g, %g)\n" x y

let make_point x y = {x; y}

let get_coords {x; y} = (x, y)

let get_x p = fst (get_coords p)
let get_y p = snd (get_coords p)

let make_segment startp endp = {startp; endp}

let get_points {startp; endp} = (startp, endp)


 (* A.2 *)
type rectangle = {ll: point; ur: point}
type rectangle2 = {lx: float; ux: float; ly: float; uy: float}

let rectangle_lower_segment {ll; ur} = 
  make_segment ll ( make_point (get_x ur) (get_y ll) )
let rectangle_upper_segment {ll; ur} = 
  make_segment ( make_point (get_x ll) (get_y ur) ) ur
let rectangle_left_segment {ll; ur} = 
  make_segment ll ( make_point (get_x ll) (get_y ur) )
let rectangle_right_segment {ll; ur} = 
  make_segment ( make_point (get_x ur) (get_y ll) ) ur

let rectangle_lower_segment2 {lx; ux; ly; uy = _} = 
  make_segment (make_point lx ly) (make_point ux ly)
let rectangle_upper_segment2 {lx; ux; uy; ly = _} = 
  make_segment (make_point lx uy) (make_point ux uy)
let rectangle_left_segment2 {lx; ly; uy; ux = _} = 
  make_segment (make_point lx ly) (make_point lx uy)
let rectangle_right_segment2 {ux; ly; uy; lx = _} = 
  make_segment (make_point ux ly) (make_point ux uy)

let rectangle_perimeter rect = 
  2. *. ( abs_float (segment_length (rectangle_upper_segment rect) ) +.
    abs_float ( segment_length (rectangle_right_segment rect) ) )

let rectangle_area rect = 
  abs_float ( segment_length (rectangle_upper_segment rect) *. 
    segment_length (rectangle_right_segment rect) )

let rectangle_perimeter2 rect = 
  2. *. ( abs_float (segment_length (rectangle_upper_segment2 rect) ) +.
    abs_float ( segment_length (rectangle_right_segment2 rect) ) )

let rectangle_area2 rect = 
  abs_float ( segment_length (rectangle_upper_segment2 rect) *. 
    segment_length (rectangle_right_segment2 rect) )

let make_rectangle ll ur = {ll; ur}
let make_rectangle2 lx ly ux uy = {lx; ux; ly; uy}


 (* A.3 *)
let make_pair x y = fun m -> m x y
(* Or, equivalently: let make_pair x y m = m x y *)

(* Replaced x and y respectively with _ to silence unused variable warnings *)
let first z = z (fun x _ -> x)
let second z = z (fun _ y -> y)

(* 
Initial Desugaring for the Substitution Evaluations Below:
let make_pair x y = fun m -> m x y
Desugars to
let make_pair = fun x y -> fun m -> m x y
Bind make_pair to the value fun x y -> fun m -> m x y

let first z = z (fun x y -> x)
Desugars to:
let first = fun z -> z (fun x y -> x)
Bind first to the value fun z -> z (fun x y -> x)

let second z = z (fun x y -> y)
Desugars to:
let second = fun z -> z (fun x y -> y)
Bind second to the value fun z -> z (fun x y -> y)


Showing first (make_pair x y) yields x for any objects x and y 
using partial substitution model:

Evaluate first (make_pair x y)
  * evaluate (make_pair x y)
    * evaluate x --> x
    * evaluate y --> y
    * evaluate make_pair --> fun x y -> fun m -> m x y
    apply fun x y -> fun m -> m x y to x y
    substitute x for x, y for y in (fun m -> m x y) 
      --> fun m -> m x y
  * evaluate first --> fun z -> z (fun x y -> x)
  apply (fun z -> z (fun x y -> x)) to (fun m -> m x y)
  substitute (fun m -> m x y) for z in (fun z -> z (fun x y -> x)) 
    --> (fun m -> m x y) (fun x y -> x)
  * evaluate (fun m -> m x y) (fun x y -> x)
    * evaluate (fun x y -> x) --> itself
    * evaluate (fun m -> m x y) --> itself
    * apply (fun m -> m x y) to (fun x y -> x)
    substitute (fun x y -> x) for m in m x y
      --> (fun x y -> x) x y
    * evaluate (fun x y -> x) x y
      * evaluate x --> x [where x is any defined value]
      * evaluate y --> y [where y is any defined value]
      * evaluate (fun x y -> x) --> itself
      apply (fun x y -> x) to x, y
      substitute x for x, y for y in x
        --> x
Results in x for any type or value of x!


Showing evaluation of second (make_pair 1 2) results in 2.

Evaluate second (make_pair 1 2)
  * evaluate (make_pair 1 2)
    * evaluate 1 --> 1
    * evaluate 2 --> 2
    * evaluate make_pair --> fun x y -> fun m -> m x y
    apply fun x y -> fun m -> m x y to 1 2
    substitute 1 for x, 2 for y in (fun m -> m x y) 
      --> fun m -> m 1 2
  * evaluate second --> fun z -> z (fun x y -> y)
  apply (fun z -> z (fun x y -> y)) to (fun m -> m 1 2)
  substitute (fun m -> m 1 2) for z in (fun z -> z (fun x y -> y)) 
    --> (fun m -> m 1 2) (fun x y -> y)
  * evaluate (fun m -> m 1 2) (fun x y -> y)
    * evaluate (fun x y -> y) --> itself
    * evaluate (fun m -> m 1 2) --> itself
    * apply (fun m -> m 1 2) to (fun x y -> y)
    substitute (fun x y -> y) for m in m 1 2
      --> (fun x y -> y) 1 2
    * evaluate (fun x y -> y) 1 2
      * evaluate 1 --> 1
      * evaluate 2 --> 2
      * evaluate (fun x y -> y) --> itself
      apply (fun x y -> y) to 1, 2
      substitute 1 for x, 2 for y in y
        --> 2
Results in 2!
*)


 (* A.4 *)
let pow base exp = 
  if exp < 0 then invalid_arg "argument must be nonnegative" else
  let rec iter i out =
    if i = exp 
      then out
      else iter (i + 1) (out * base)
  in iter 0 1

let int_log base num = 
  let rec iter count saved =
    if saved mod base = 0 
      then iter (count + 1) (saved / base)
      else count
  in iter 0 num

let make_pairi a b = (pow 2 a) * (pow 3 b)
let firsti pairi = int_log 2 pairi
let secondi pairi = int_log 3 pairi


 (* A.5 *)
let zero = []

let is_zero = function
  | [] -> true
  | () :: _ -> false

let succ u = () :: u

let prev = function
  | [] -> invalid_arg "argument must be > 0: no unary negative numbers"
  | _ :: t -> t

let integer_to_unary a = 
  let rec iter i out =
    if i == a
      then out
      else iter (i + 1) (succ out)
  in iter 0 zero

let unary_to_integer u = 
  let rec iter count saved =
    if is_zero saved 
      then count
      else iter (count + 1) (prev saved)
  in iter 0 u

let unary_add u1 u2 = 
  let rec iter out saved =
    if is_zero saved
      then out
      else iter (succ out) (prev saved)
  in iter u1 u2

type nat = Zero | Succ of nat

let zero' = Zero

let is_zero' = function
  | Zero -> true
  | Succ _ -> false

let succ' u = Succ u

let prev' = function 
  | Zero -> invalid_arg "argument must be > 0: no unary negative numbers"
  | Succ pre -> pre

let integer_to_unary' a = 
  let rec iter i out =
    if i == a
      then out
      else iter (i + 1) (succ' out)
  in iter 0 zero'

let unary_to_integer' u = 
  let rec iter count saved =
    if is_zero' saved 
      then count
      else iter (count + 1) (prev' saved)
  in iter 0 u

let unary_add' u1 u2 = 
  let rec iter out saved =
    if is_zero' saved
      then out
      else iter (succ' out) (prev' saved)
  in iter u1 u2

(* No, other than the obvious name changes of adding in the prime, none of the 
   function definitions for integer_to_unary, unary_to_integer, or unary_add had 
   to change due to our leveraging of the abstraction layer. *)


 (* A.6 *)
(* zerof = "functional zero"; we call it this so as not to be confused with
   zero or zero' previously defined. *)
let zerof = fun _ z -> z (* Replaced s with _ to silence unused variable warning *)
  (* or equivalently: let zerof = fun s -> fun z -> z *)
  (* or equivalently: let zerof s z = z *)

let add1 n = fun s -> fun z -> s (n s z)
  (* or equivalently: let add1 n = fun s z -> s (n s z) *)
  (* or equivalently: let add1 n s z = s (n s z) *)

(* Substitution work to find the direct number representations:
One:
add1 (zerof)
add1 (fun s -> fun z -> z)
fun s -> fun z -> s ( (fun s -> fun z -> z) s z)
fun s -> fun z -> s ( (fun z -> z) z)
fun s -> fun z -> s z
fun s z -> s z

Two:
add1 (one)
add1 (fun s -> fun z -> s z)
fun s -> fun z -> s ( (fun s -> fun z -> s z) s z)
fun s -> fun z -> s ( (fun z -> s z) z)
fun s -> fun z -> s s z
fun s z -> s s z
*)

let one s z   = s z
let two s z   = s (s z)
let three s z = s (s (s z) )
let four s z  = s (s (s (s z) ) )
let five s z  = s (s (s (s (s z) ) ) )
let six s z   = s (s (s (s (s (s z) ) ) ) )
let seven s z = s (s (s (s (s (s (s z) ) ) ) ) )
let eight s z = s (s (s (s (s (s (s (s z) ) ) ) ) ) )
let nine s z  = s (s (s (s (s (s (s (s (s z) ) ) ) ) ) ) )
let ten s z   = s (s (s (s (s (s (s (s (s (s z) ) ) ) ) ) ) ) )

let add m n s z = m s (n s z)

let church_to_integer a = a (fun x -> x + 1) 0


 (* A.7 *)
(*
If we look at our definition of church_to_integer, we can see it only takes one argument a. 
We know that this argument a is our representation of a church integer, but let's consider how 
OCaml sees this. In church_to_integer, we call a as a function on the two arguments 
(fun x -> x + 1) 0. From this, OCaml knows that a takes two arguments, the first being a 
function (int -> int) [inferred from (fun x -> x + 1)] and the second being an int [inferred 
from 0]. Then, it knows that a obviously has a return value once evaluated, but it doesn't know 
the type of that value since there is no indication of that without having the user supply a, 
so it has the return type as any arbitrary type 'a. Thus, ((int -> int) -> int -> 'a) is the 
type signature for our argument a. Finally, since we're returning the result of a, we know the 
return type for church_to_integer will be the same as that, yielding the final return type of 'a.
Therefore, we've explained the type signature of church_to_integer.

Going forward, consider the type signature for church_to_integer as ((int -> int) -> int -> 'c) -> 'c.

Now, we can consider what happens when we supply one to church_to_integer. Essentially, it takes the 
one type signature ('a -> 'b) -> 'a -> 'b and the church_to_integer type signature 
((int -> int) -> int -> 'c) -> 'c and substitutes in the unknown types to get the types for 'a, 'b, and 'c.
When we call church_to_integer with one, our argument a with type signature ((int -> int) -> int -> 'c) 
is now supplied with one, which has the type signature ('a -> 'b) -> 'a -> 'b. So, the types of one now 
get filled with the arguments from church_to_integer, such that (a' -> 'b) is typed to (int -> int) and
'a is typed to int. The signature for one also tells us that the return type of argument a is 'b by seeing 
the two places 'b appears here ('a -> 'b) -> 'a -> 'b. Thus, the 'c, which is the retun type of a in 
((int -> int) -> int -> 'c) -> 'c is also typed to int. Therefore, 'a, 'b, and 'c are all typed to int, 
yielding type signature of our evaluated church_to_integer one call as ((int -> int) -> int -> int) -> int, 
and showing how it returns int. 

Similarly, we can examine the call church_to_integer zerof in the same way.
By calling zerof with the type signature 'a -> 'b -> 'b on the arguments from church_to_integer, we've typed 
'a to (int -> int) and b to int. Thus, the type signature of 'a -> 'b -> 'b becomes 
(int -> int) -> int -> int. Since our argument a gets evaluated with this type signature, this gives a 
the type signature (int -> int) -> int -> int as well, where 'c has been typed to int. Therefore, 
church_to_integer's type signature of ((int -> int) -> int -> 'c) -> 'c gets typed to 
((int -> int) -> int -> int) -> int where 'a has been typed to (int -> int) and 'b and 'c have been 
typed to int, showing how the call to church_to_integer zerof returns int.
*)




 (* B.1 *)
type mobile = Mobile of branch * branch  (* left and right branches *)
and branch =
  | Weight    of int * int     (* length and weight *)
  | Structure of int * mobile  (* length and sub-mobile *)

let make_mobile l r = Mobile (l, r)
let make_weight l w = Weight (l, w)
let make_structure l m = Structure (l, m)

(* B.1.a *)
let left_branch (Mobile (l, _)) = l
let right_branch (Mobile (_, r)) = r

let branch_length = function 
  | Weight (len, _) -> len
  | Structure (len, _) -> len

let branch_structure = function 
  | Weight (_, w) -> `Weight w
  | Structure (_, m) -> `Structure m

(* B.1.b *)
(* Uses direct representation of data *)
let rec branch_weight1 = function
  | Weight (_, w) -> w
  | Structure (_, m) -> total_weight1 m
and total_weight1 (Mobile (l, r)) = 
  branch_weight1 l + branch_weight1 r

(* Uses abstraction layer *)
let rec branch_weight2 b = 
  match branch_structure b with
  | `Weight w -> w
  | `Structure m -> total_weight2 m
and total_weight2 m = 
  branch_weight2 (left_branch m) + branch_weight2 (right_branch m)

(* B.1.c *)
let rec is_balanced m = 
  let left = left_branch m and right = right_branch m in
  let left_tq = (branch_length left) * (branch_weight2 left)
  and right_tq = (branch_length right) * (branch_weight2 right) in 
    if left_tq <> right_tq then false else
    match (branch_structure left, branch_structure right) with
      | (`Weight _, `Weight _) -> true
      | (`Weight _, `Structure sub) -> is_balanced sub
      | (`Structure sub, `Weight _) -> is_balanced sub
      | (`Structure sub1, `Structure sub2) -> is_balanced sub1 && is_balanced sub2

(* B.1.d *)
type mobile'  = { left: branch'; right: branch' }
and  branch'  = Branch' of int * contents
and  contents = Weight' of int | Structure' of mobile'

let make_mobile' l r = {left=l; right=r}
let make_weight' l w = Branch' (l, Weight' w)
let make_structure' l m = Branch' (l, Structure' m) 
let left_branch' {left; right = _} = left
let right_branch' {right; left = _} = right
let branch_length' (Branch' (len, _)) = len
(* Note: Interface said to return (`Weight | `Structure) instead of 
   prime versions (`Weight' | `Structure').*)
let branch_structure' (Branch' (_, content)) =
  match content with 
    | Weight' w -> `Weight w
    | Structure' m -> `Structure m

(* Uses abstraction layer *)
let rec branch_weight' b = 
  match branch_structure' b with
  | `Weight w -> w
  | `Structure m -> total_weight' m
and total_weight' m = 
  branch_weight' (left_branch' m) + branch_weight' (right_branch' m)

let rec is_balanced' m = 
  let left = left_branch' m and right = right_branch' m in
  let left_tq = (branch_length' left) * (branch_weight' left)
  and right_tq = (branch_length' right) * (branch_weight' right) in 
    if left_tq <> right_tq then false else
    match (branch_structure' left, branch_structure' right) with
      | (`Weight _, `Weight _) -> true
      | (`Weight _, `Structure sub) -> is_balanced' sub
      | (`Structure sub, `Weight _) -> is_balanced' sub
      | (`Structure sub1, `Structure sub2) -> is_balanced' sub1 && is_balanced' sub2


 (* B.2 *)
type tree = Tree of elem list
and elem =
  | Num of int
  | Sub of tree

let square_tree (Tree lst) = 
  let rec square_list elem_lst =
    match elem_lst with 
    | [] -> []
    | h :: t -> 
      match h with 
        | Num x -> Num (x * x) :: square_list t
        | Sub (Tree lst') -> Sub (Tree (square_list lst')) :: square_list t
  in Tree (square_list lst)

let square_tree' (Tree lst) = 
  let rec tree_map elem = 
    match elem with
      | Num x -> Num (x * x)
      | Sub (Tree lst') -> Sub (Tree (List.map tree_map lst'))
  in Tree (List.map tree_map lst)


 (* B.3 *)
let tree_map map_func (Tree lst) = 
  let rec tree_map elem = 
    match elem with
      | Num x -> Num (map_func x)
      | Sub (Tree lst') -> Sub (Tree (List.map tree_map lst'))
  in Tree (List.map tree_map lst)

let square_tree'' tree = tree_map (fun n -> n * n) tree




 (* C.1 *)
type expr =
  | Int of int           (* constant *)
  | Var of string        (* variable *)
  | Add of expr * expr   (* expr1 + expr2 *)
  | Mul of expr * expr   (* expr1 * expr2 *)
  | Pow of expr * int    (* expr^n *)

(* Deals with all basic simplification cases for one scan over the whole expression *)
let rec simplify1 ex = 
  match ex with
    | Int _
    | Var _ -> ex
    (* Add Cases *)
    | Add (Int x, Int y) -> Int (x + y)
    | Add (Int 0, other)
    | Add (other, Int 0) -> other
    | Add (a1, a2) -> Add (simplify1 a1, simplify1 a2)
    (* Multiply Cases *)
    | Mul (Int x, Int y) -> Int (x * y)
    | Mul (Int 0, _)
    | Mul (_, Int 0) -> Int 0
    | Mul (Int 1, other)
    | Mul (other, Int 1) -> other
    | Mul (m1, m2) -> Mul (simplify1 m1, simplify1 m2)
    (* Exponent Cases *)
    | Pow (Int b, exp) -> Int (pow b exp)
    | Pow (_, 0) -> Int 1
    | Pow (base, 1) -> base
    | Pow (base, exp) -> Pow (simplify1 base, exp)

(* Simplifies until the expression converges using helper simplify1 *)
let rec simplify expr =
  let e = simplify1 expr in
    if expr = e
      then expr
      else simplify e


 (* C.2 *)
(* Computes the expanded derivative using basic differentiation rules *)
let rec deriv var ex =
  match ex with
    | Int _ -> Int 0
    | Var v when v = var -> Int 1
    | Var _ -> Int 0
    | Add (a1, a2) -> 
        Add (deriv var a1, deriv var a2)
    | Mul (m1, m2) ->
        Add (Mul (deriv var m1, m2), Mul (m1, deriv var m2))
    | Pow (base, exp) ->
        Mul (Mul (Int exp, Pow (base, exp - 1)), deriv var base)

(* Takes the derivative of expr with respect to var and then simplifies *)
let derivative var expr =
  let e = simplify expr in
  let d = deriv var e in
    simplify d