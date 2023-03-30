open Num


 (* Helper functions used throughout code *)
let ni = num_of_int     (* Alias to convert int to num *)
let add1 = fun n -> n +/ (ni 1)
let self = fun n -> n



 (* A.1 *)
(* The tree recursive fibonacci function has linear space complexity Theta(n). 
   To understand why, we can picture any use of the function as a recursive tree 
   of all the calls of fib where every call of fib is a node and all nodes have 
   2 children nodes until we reach leaf nodes of strictly fib 1 or fib 0. Because 
   OCaml's Substitution Model uses Applicative Order evaluation, OCaml has to 
   evaluate operands first before anything else. This means in out case of 
   fib (n - 1) + fib (n - 2), we must fully evaluate fib (n - 1) (and all 
   subsequent fib (n - 1) calls) before evaluating fib (n - 2). This has the 
   effect of tracing our tree depth-first going as deep as possible in evaluation 
   before evaluating any other fib calls. Because of this, the number of fib 
   calls that exist on our stack frame at once is limited by the depth of our tree. 
   Thus, since the max depth of our tree is n (as we have n fib (n - 1) calls before 
   reaching fib 1), the space complexity is Theta(n). This is obviously different than 
   time complexity of Theta(2^n) because the time complexity for our tree would be 
   proportional to how long it takes to evaluate all nodes in our tree. Since our 
   tree is binary with max depth n, there are at max 2^n nodes yileding Theta(2^n) 
   time complexity. *)


 (* A.2 *)
let cube x = x *. x *. x
let p x = 3.0 *. x -. 4.0 *. cube x
let rec sine angle =
  if abs_float angle < 0.1
    then angle
    else p (sine (angle /. 3.0))
(* 1. The p function would be applied 5 times in the call to sine 12.15 because we 
      need to divide 12.15 by a 3 5 times to get below 0.1, and thenwe'd apply p to 
      that resulting value (of 0.05) 5 times to get our value for sine 12.15. 
   2. The space complexity and time complexity are both Theta(log n). This is because
      we are dividing our input parameter angle by 3 with every subsequent call 
      yielding a number of steps proportional to log base 3 of x. Thus, these number 
      of steps define our time complexity Theta(log n). Then, for each step, we have 
      to set aside the same amount of memory for each of the subsequent calls, so 
      since we have Theta(log n) number of steps, we also have Theta(log n) space 
      complexity. *)


 (* A.3 *)
(*
let rec fast_expt b n =
  let is_even m = m mod 2 = 0 in
  let square m = m * m in
    if n = 0 then 1
    else if is_even n then square (fast_expt b (n / 2))
    else b * fast_expt b (n - 1)
*)

 (* A.3a *)
let rec fast_expt b n =
  let is_even m = m mod 2 = 0 in
  let square m = m * m in
    match n with 
      | 0 -> 1
      | n' when is_even n' -> square (fast_expt b (n' / 2))
      | _ -> b * fast_expt b (n - 1)

 (* A.3b *)
let ifast_expt b n =
  let is_even m = m mod 2 = 0 in
  let square m = m * m in
  let rec iter left base out = 
    match left with 
      | 0 -> out
      | left' when is_even left' -> iter (left' / 2) (square base) out
      | _ -> iter (left - 1) base (base * out)
  in iter n b 1 


 (* A.4 *)
let rec fast_mult b n =
  let double m = 2 * m in
  let halve m = m / 2 in
    match n with 
      | 0 -> 0
      | n' when n' mod 2 = 0 -> double (fast_mult b (halve n'))
      | _ -> b + fast_mult b (n - 1)


 (* A.5 *)
let ifast_mult b n =
  let double m = 2 * m in
  let halve m = m / 2 in
  let rec iter left base out = 
    match left with 
      | 0 -> out
      | left' when left' mod 2 = 0 -> iter (halve left') (double base) out
      | _ -> iter (left - 1) base (base + out)
  in iter n b 0


 (* A.6 *)
(*
let rec foo f n =
  if n <= 1
     then f 0
     else foo f (n / 2) + foo f (n / 2)
*)
(* Here, we have a similar case to A.1 but a little more tricky. The 2 calls to foo 
   generate tree recursive behaviour while the division by 2 of each subsequent n 
   suggests Theta(log n) behavour. So, how does this work? To figure this out, we 
   can see any call to foo as a binary tree where each node is a call to foo. Abiding 
   by applicative order evaluation, we must evaluate all foo f (n / 2) calls depth 
   first. Since we keep dividing n by 2, there will be log base 2 of n of these 
   subsequent calls before reaching a leaf node, making the depth of our tree log 
   base 2 of n. After getting to a leaf node at the end of the tree, we are able to 
   evaluate other calls. This means that proportionally, the space used on the stack 
   will never be more than these log base 2 of n subsequent calls making our space 
   complexity Theta(log n). Now, the time complexity is governed by how long it takes 
   to evaluate all of our nodes, which is proportional to the number of nodes since 
   all of the operations other than foo compute in constant time. Since the depth of 
   our tree is log base 2 of n and each non-leaf node has 2 children, there's at worst 
   case 2^(log base 2 of n) nodes yielding a time complexity of Theta(2^(log n)). *)


 (* A.7 *)
(*
let fib n =
  let rec last_two n =
    if n < 1
      then (0, 1)
      else
        let (p0, p1) = last_two (n - 1) in
          (p1, p0 + p1)
  in
    fst (last_two n)
*)
(* 1. This is a linear recursive process because there is a singular recursive call 
      with n - 1 where each call relies on and can't compute anything else until the
      subsequent call within it completes. Since there is only one recursive call 
      that decreases its argument by 1 every time, we know the process is linear. 
      And, since no call can finish its computation until its subsequent call yields 
      a result (no tail call recursion), we know the process is recursive.
   2. Since we have a linear recursive process, this generates n calls on our stack. 
      This makes the space complexity Theta(n). The time it takes to complete these n 
      calls is also linearly proportional to n because all other operations our 
      constant, yielding Theta(n) for our time complexity as well. *)




 (* B.1.a *)
(*
let x = 20
and y = 2 * 4
in x * (2 + y)
Desugared: 
(fun x y -> x * (2 + y)) 20 (2 * 4)
*)

 (* B.1.b *)
(*
let a = 1.0
and b = 20.0
and c = 3.0
in sqrt (b *. b -. 4.0 *. a *. c)
Desugared: 
(fun a b c -> sqrt (b *. b -. 4.0 *. a *. c)) 1.0 20.0 3.0
*)

 (* B.1.c *)
(*
let x = 1 in
let y = 2 in
let z = 3 in
  x * y * z
Desugared: 
(fun x -> (fun y -> (fun z -> x * y * z) 3) 2) 1   
*)

 (* B.1.d *)
(*
let x = 1 in
let x = 2 in
let x = 3 in
  x * x * x
Desugared:
(fun x -> (fun x -> (fun x -> x * x * x) 3) 2) 1
*)


 (* B.2 *)
(*
Evaluate: let x = 2 * 10 and y = 3 + 4 in let y = 14 in let z = 22 in x * y * z
  desugar let x = 2 * 10 and y = 3 + 4 in -->
    (fun x y -> let y = 14 in let z = 22 in x * y * z) (2 * 10) (3 + 4)
  desugar let y = 14 in -->
    (fun x y -> (fun y -> let z = 22 in x * y * z) 14) (2 * 10) (3 + 4)
  desugar let z = 22 in in -->
    (fun x y -> (fun y -> (fun z -> x * y * z) 22) 14) (2 * 10) (3 + 4)
  evaluate (2 * 10) --> 20
  evaluate (3 + 4) --> 7
  evaluate (fun x y -> ...) to itself
  apply (fun x y -> ...) to 20, 7
  substitute 20 for x, 7 for y in ... --> fun y -> (fun z -> 20 * y * z) 22) 14    
  ***Note y doesn't substitute because of inner declaration of y via lambda shielding.
  evaluate fun y -> (fun z -> 20 * y * z) 22) 14
    evaluate 14 --> 14
    evaluate (fun y -> ...) to itself
    apply (fun y -> ...) to 14
    substitute 14 for y in ... --> (fun z -> 20 * 14 * z) 22
    evaluate (fun z -> 20 * 14 * z) 22
      evaluate 22 --> 22
      evaluate (fun z -> ...) to itself
      substitute 22 for z in ... -> 20 * 14 * 22
      evaluate 20 * 14 * 22
        evaluate 20 --> 20
        evaluate 14 --> 14
        evaluate * to itself [primitive func *]
        apply * to 20, 14 -> 280
        evaluate 280 * 22
          evaluate 280 --> 280
          evaluate 22 --> 22
          evaluate * to itself [primitive func *]
          apply * to 280, 22 -> 6160
Result: 6160
*)


 (* B.3 *)
(*
let x = 10
and y = x * 2
and z = y + 3
in x + y + z
Desugared:
(fun x y z -> x + y + z) 10 (x * 2) (y + 3)

From this desugared form, it is clear why this doesn't work Ben.
You can think of 'let/and' as defining parameters in parallel such that any 
parameters you define all get defined at the same time, so obviously the 
parameters cannot rely on each other. Even though x is defined first, OCaml's 
applicative order evaluation means that all operands have to be evaluated 
individually first before applying the operator or doing anything else. Thus, 
10 evaluates to 10 then OCaml tries to evaluate (x * 2) and it errors because
x hasn't been bound to 10 yet. If you want to do what you want you need to use
nested lets so that the parameters are defined in succession using multiple fun 
definitions. Here's code of the behavour you want with a desugared version:

let x = 10 in
let y = x * 2 in
let z = y + 3
in x + y + z
Desugared:
(fun x -> (fun y -> (fun z -> x + y + z) (y + 3)) (x * 2)) 10
*)




 (* C.1 *)
let isum term a next b =
  let rec iter a result =
    if a >/ b
       then result
       else iter (next a) (result +/ term a)
  in
    iter a (ni 0)


 (* C.2 *)
(* Note that these could also be used to redefine the sum functions *)
let put_together_iter oper base term a next b =
  let rec iter a result =
    if a >/ b
      then result
      else iter (next a) (oper result (term a))
  in
    iter a base

let rec put_together_rec oper base term a next b =
  if a >/ b
    then base
    else oper (term a) (put_together_rec oper base term (next a) next b)

let product_iter = put_together_iter ( */ ) (ni 1)

let product_rec = put_together_rec ( */ ) (ni 1)

let factorial_iter = product_iter self (ni 1) add1

let factorial_rec = product_rec self (ni 1) add1

let pi_product i = 
  (ni 2) */ i */ ((ni 2) */ i +/ (ni 2)) // (((ni 2) */ i +/ (ni 1)) **/ (ni 2))

let pi_approx = float_of_num ((ni 4) */ product_iter pi_product (ni 1) add1 (ni 2500))


 (* C.3 *)
(* Note: I already did this for Part C.3 not anticipating I would be asked 
 * to do the same thing again. Because of this, my solutions for this part 
 * are very short. Please refer to my initial implementations above that I 
 * use to define these functions. *)
let accumulate_iter = put_together_iter
let accumulate_rec = put_together_rec
let sum = accumulate_iter (+/) (ni 0)
let product = accumulate_rec ( */ ) (ni 1)


 (* C.4 *)
let compose f g x = f (g x)


 (* C.5 *)
let repeated func (n: int) =
  let rec iter i out =
    if i = 0
      then out
      else iter (i - 1) (compose func out)
  in
    iter n self


 (* C.6 *)
let smooth dx func x =
  (func (x -. dx) +. func (x) +. func (x +. dx)) /. 3.

let nsmoothed dx func n = repeated (smooth dx) n func




 (* D.1 *)
let is_prime x =
  if x < 2 then false else
  if x <> 2 && x mod 2 = 0 then false else
    let rec iter i stop =
      if i > stop
        then true
        else if x mod i = 0
          then false
          else iter (i + 2) stop
    in iter 3 (int_of_float (sqrt (float_of_int x)))


 (* D.2 *)
let smallest_prime_factor x =
  if is_prime x || x < 2 then failwith "invalid arg" else 
  if x mod 2 = 0 then 2 else
    let rec iter i stop =
      if x mod i = 0
        then i
        else iter (i + 2) stop
    in iter 3 (int_of_float (sqrt (float_of_int x)))