(* A.1 *)
(* 
 *  10
 *  Prints - : int = 10

 *  10.
 *  Prints - : float = 10.

 *  5 + 3 + 4
 *  Prints - : int = 12 

 *  3.2 + 4.2;
 *  Prints Error: This expression has type float but an expression was expected of type int 
 *  The + operator is made for integer addition, but we're trying to use it on floats 

 *  3 +. 4;
 *  Prints Error: This expression has type int but an expression was expected of type float 
 *  The +. operator is made for float addition, but we're trying to use it on integers 

 *  3 + 4.2;
 *  Prints Error: This expression has type float but an expression was expected of type int 
 *  The + operator is made for integer addition, but the 2nd parameter we're giving it is a float 

 *  3 +. 4.2;
 *  Prints Error: This expression has type int but an expression was expected of type float 
 *  The +. operator is made for float addition, but the 2nd parameter we're giving it is an integer 

 *  3.0 +. 4.2;
 *  Prints - : float = 7.2 

 *  9 - 3 - 1;
 *  Prints - : int = 5 

 *  9 - (3 - 1);
 *  Prints - : int = 7 

 *  let a = 3;
 *  Prints val a : int = 3 

 *  let b = a + 1;
 *  Prints val b : int = 4 

 *  a = b;
 *  Prints - : bool = false 

 *  [1; 2; 3] = [1; 2; 3];
 *  Prints - : bool = true 

 *  [1; 2; 3] == [1; 2; 3];  Is this the same as or different from the previous expression? Why? 
 *  Prints - : bool = false 
 *  It's different because the = operator compares the contents of the parameters while the == operator compares their addresses 

 *  [(1, 2, 3)];
 *  Prints - : (int * int * int) list = [(1, 2, 3)] 

 *  [1, 2, 3];   Explain why this gives the result it does. This is a nasty pitfall which highlights one of the less desirable features of OCaml’s syntax. (See the OCaml cheat sheet.) 
 *  Prints - : (int * int * int) list = [(1, 2, 3)] 
 *  Commas are reserved for tuples, so OCaml interprets the 1, 2, 3 as a (1, 2, 3) tuple with the () simply left out as a shortcut. 
 *   * Then, the [] puts the (1, 2, 3) tuple into a list of tuples of that type. 

 *  if b > a && b < a * b then b else a;
 *  Prints - : int = 4 

 *  if b > a and b < a * b then b else a;
 *  Prints Error: Syntax error 
 *  The and keyword is not used as a comparison operator. For the and compare operator, && should be used. 

 *  2 + if b > a then b else a;
 *  Prints - : int = 6 

 *  if b > a then b else a + 2;   Why is this different from the previous case? 
 *  Prints - : int = 4 
 *  It's different because now the + 2 is part of a + 2 where the a is evaluated as the first parameter for the + operator
 *  whereas before the 2 + was separately the first part of an expression, and the second parameter for the + operator was the evaulation of the if. 
 *  The if/then/else operator has lower precedent that the + operator so it evaluates the a + 2 first. 

 *  (if b > a then b else a) + 2;
 *  Prints - : int = 6 

 *  if b > a then b;
 *  Prints Error: This expression has type int but an expression was expected of type 
 *         unit because it is in the result of a conditional with no else branch 
 *  This is not a syntax error. Why does this give a type error? Hint: What does OCaml assume if the else in an if/then/else form is left off?
 *  It is a type error because all if statements require a following then and else statements where the return value for the then and else statements must be the same type. 
 *     f no else statement is provided, the an implicit else statement is evaluated with type unit. 
 *     Because the b is of type of int, the then statement is of type int, yet the else statement is implicity of type unit. Since their types don't match, it's a type error. 
 *)


(* A.2 *)
let sum_of_squares_of_two_largest x y z = 
    if x <= y && x <= z
        then y * y + z * z
        else if y <= x && y <= z
            then x * x + z * z
            else x * x + y * y


(* A.3 *)
let a_plus_abs_b a b =
    (if b > 0 then (+) else (-)) a b
 (* Abiding by our evaulation model, we will either return a + b or a - b via the integer addition function or integer subtraction function. 
    The if statement within the parenthesis is a compound statement that evaluates to either int addition (if b > 0) or int subtraction (b <= 0),
    so we're able to have more dynamic control over calculations by conditionally selecting functions.
    Note that the effect of this is that the absolute value of b is added to a because if b is positive reguat a + b int addition occurs, but 
    if b is negative, int subtraction a - b occurs such that b's negative value get negated by the subtraction yielding a + |b| either way. 
    Also, note that we don't control (or take the abs value of) a, so if a is negative, we can still get a negative int result. *)




(* B.1 *)
 (* 
let rec p () = p ()
let test x y = if x = 0 then 0 else y

test 0 (p ())

    Using applicative order evaluation, the operands are evaulated first and then the operator is applied to the evaulated operands. 
    So, for test the operand 0 is evaulated, then p () is evaluated and because it has an infinitely recursive definition, the program 
    errors here and never eveluates test.

    Using normal order evaluation, all non-primitive operators are substituted for their compound procedures until only primitive operators are left,
    and then evaluation occurs. So, we first substitute in test yielding if 0 = 0 then 0 else (p ()). Then, we evaluate where the expression 0 = 0 is 
    true, so twe evaulate the then 0 resulting in 0, never needing to evaluate p (). 

    Here, applicative order evalaation errors due to the evaluation of the infinitely recursive p (), yet normal order evaluation yields a result of 0. 
 *)


(* B.2 *)
 (* 
let new_if predicate then_clause else_clause =
  match predicate with
    | true  -> then_clause
    | false -> else_clause

let square x = x *. x
let average x y = (x +. y) /. 2.0

let improve guess x = average guess (x /. guess)
let is_good_enough guess x =
  abs_float (square guess -. x) < 0.00001

let rec sqrt_iter guess x =
  new_if (is_good_enough guess x)
         guess
         (sqrt_iter (improve guess x) x)

    There will be an error here, yet using regular if then else would run fine. Applicative order evaluation says to evaluate all 
    operands before applying the operator. So here, we must evaluate all operands of new_if first, regardless of the value of 
    its predicate operand. So, we end up with looping recursion such that (sqrt_iter (improve guess x) x) keeps being run continously 
    because new_if can't short circuit like if then else can to return guess. 
    The short circuit ability of if then else is necessary for if statements.
 *)


(* B.3 *)
(*

Functions:
let rec add_a a b =
    if a = 0 then b else inc (add_a (dec a) b)

let rec add_b a b =
    if a = 0 then b else add_b (dec a) (inc b)

*** Treating inc and dec as primitive functions

   1. State: add_a is a recursive process while add_b is an iterative process.

   2. 
Substitution Model Evaluation: 

let rec add_a a b =
    if a = 0 then b else inc (add_a (dec a) b)

Desugar this to:

let rec add_a =
    fun a b -> if a = 0 then b else inc (add_a (dec a) b)

Bind the name "add_a" to the value:

fun a b -> if a = 0 then b else inc (add_a (dec a) b)

Evaluate: add_a 2 5 
* evaluate 2 --> 2
* evaluate 5 --> 5
* evaluate add_a --> fun a b -> ...
apply (fun a b -> ...) to 2 5 -->
    substitute 2 for a, 5 for b in if a = 0 then b else inc (add_a (dec a) b) -->
        if 2 = 0 then 5 else inc (add_a (dec 2) 5)
    evaluate: if 2 = 0 then 5 else inc (add_a (dec 2) 5)
        Special form if, first operand: evaluate 2 = 0
            * evaluate 2 --> 2
            * evaluate 0 --> 0
            * evaluate = --> [primitive function =]
            apply = to 2 0 --> false
        For false case (else), third operand: evaluate inc (add_a (dec 2) 5)
            * evaluate add_a (dec 2) 5 -->
                * evaluate dec 2 -->
                    * evaluate 2 --> 2
                    * evaluate dec --> [primitive function dec]
                    apply dec to 2 --> 1
                * evaluate 5 --> 5
                * evaluate add_a --> fun a b -> ...
                apply (fun a b -> ...) to 1 5
                    substitute 1 for a, 5 for b in if a = 0 then b else inc (add_a (dec a) b) -->
                        if 1 = 0 then 5 else inc (add_a (dec 1) 5)
                    evaluate: if 1 = 0 then 5 else inc (add_a (dec 1) 5)
                        Special form if: evaluate 1 = 0
                            * evaluate 1 --> 1
                            * evaluate 0 --> 0
                            * evaluate = --> [primitive function =]
                            apply = to 1 0 --> false
                        For false case (else), third operand: evaluate inc (add_a (dec 1) 5)
                            * evaluate add_a (dec 1) 5 -->
                                * evaluate dec 1 -->
                                    * evaluate 1 --> 1
                                    * evaluate dec --> [primitive function dec]
                                    apply dec to 1 --> 0
                                * evaluate 5 --> 5
                                * evaluate add_a --> fun a b -> ...
                                apply (fun a b -> ...) to 0 5
                                    substitute 0 for a, 5 for b in if a = 0 then b else inc (add_a (dec a) b) -->
                                        if 0 = 0 then 5 else inc (add_a (dec 0) 5)
                                    evaluate: if 0 = 0 then 5 else inc (add_a (dec 0) 5)
                                        Special form if: evaluate 0 = 0
                                            * evaluate 0 --> 0
                                            * evaluate 0 --> 0
                                            * evaluate = --> [primitive function =]
                                            apply = to 0 0 --> true
                                        For true case (then), second operator: evaluate 5 --> 5
                            * evaluate inc --> [primitive function inc]
                            apply to inc to 5 --> 6       
            * evaluate inc --> [primitive function inc]
            apply to inc to 6 --> 7
Result: 7


3. 
Substitution Model Correction: 

let rec add_b a b =
    if a = 0 then b else add_b (dec a) (inc b)

Desugar this to:

let rec add_b =
    fun a b -> if a = 0 then b else add_b (dec a) (inc b)

Bind the name "add_b" to the value:

fun a b -> if a = 0 then b else add_b (dec a) (inc b)

Evaluate (add_b 2 5)
    >>> evaluate 2 --> 2
    >>> evaluate 5 --> 5
    >>> evaluate add_b --> fun a b -> ...
    apply (fun a b -> if ...) to 2, 5
    substitute 2 for a, 5 for b in (if ...)
        -> if 2 = 0 then 5 else add_b (dec 2) (inc 5)
    evaluate (if 2 = 0 then 5 else add_b (dec 2) (inc 5))
        if is a special form, so evaluate the first operand:
            evaluate (2 = 0)
                >>> evaluate 2 --> 2
                >>> evaluate 0 --> 0
                >>> evaluate = --> [primitive function =]
                apply = to 2, 0 -> false
        first argument of if is false, so evaluate the third operand:
            evaluate (add_b (dec 2) (inc 5))
                evaluate (dec 2)
                    >>> evaluate 2 --> 2
                    >>> evaluate dec --> [primitive function dec]
                    apply dec to 2 -> 1
                evaluate (inc 5)
                    >>> evaluate 5 --> 5
                    >>> evaluate inc --> [primitive function inc]
                    apply inc to 5 -> 6
                >>> evaluate add_b --> fun a b -> ...
                apply (fun a b -> if ...) to 1, 6
                substitute 1 for a, 6 for b in (if ...)
                    -> if 1 = 0 then 6 else add_b (dec 1) (inc 6)
                evaluate (if 1 = 0 then 6 else add_b (dec 1) (inc 6))
                    if is a special form, so evaluate the first operand:
                        evaluate (1 = 0)
                            >>> evaluate 1 --> 1
                            >>> evaluate 0 --> 0
                            >>> evaluate = --> [primitive function =]
                            apply = to 1, 0 -> false
                    first argument of if is false, so evaluate the third operand:
                        evaluate (add_b (dec 1) (inc 6))
                            evaluate (dec 1)
                                >>> evaluate 1 --> 1
                                >>> evaluate dec --> [primitive function dec]
                                apply dec to 1 -> 0
                            evaluate (inc 6)
                                >>> evaluate 6 --> 6
                                >>> evaluate inc --> [primitive function inc]
                                apply inc to 6 -> 7
                            >>> evaluate add_b --> fun a b -> ...
                            apply (fun a b -> if ...) to 0, 7
                            substitute 0 for a, 7 for b in (if ...)
                                -> if 0 = 0 then 7 else add_b (dec 0) (inc 7)
                            evaluate (if 0 = 0 then 7 else add_b (dec 0) (inc 7))
                                if is a special form, so evaluate the first operand:
                                    evaluate (0 = 0)
                                        >>> evaluate 0 --> 0
                                        >>> evaluate 0 --> 0
                                        >>> evaluate = --> [primitive function =]
                                        apply = to 0, 0 -> true
                                first argument of if is true, so evaluate the second operand:
                                    >>> evaluate 7 --> 7
                                    result: 7
*)




(* C.1 *)
 (* This function computes the factorial of the input number,
   which for a number n is equal to n * (n-1) * ... * 1. *)
let rec factorial n =
    if n = 0 then 1 else n * factorial (n - 1)

(* C.1.a *)
let e_term (i : int) = 
    1.0 /. float_of_int (factorial i)

(* C.1.b *)
let rec e_approximation (n: int) = 
    match n with
        | 0 -> 1.0
        | n' -> (e_term n') +. e_approximation (n - 1)

(* C.1.c *)
(* e_approximation 20 -->   2.71828182845904553
   exp 1.0 -->              2.71828182845904509 *)

(* C.1.d *)
(* The value of e_approximation 100 is a float of infinity. This happens because we have overflowed the float. 
   Floating point data types can only be so precise and that precision is limited by the amount of space the data type has. 
   In computing some e_term between 0 and 100 (around 64), the factorial i call results in an overflow and returns 0 such 
   that the e_term division produces 1 divided by 0, thus yielding infinity. The overflow occurs in factorial i because the 
   factorial product exceeds the number of bits that OCaml can hold for a float. Then, the e_term call results in and 
   returns infinity via the division by 0. Finally, once we try adding the result of that e_term to all the other e_term 
   calls to compute our summed e approximation, we get infinity since infinity + anything is still infinity. *)


(* C.2 *)
let rec is_even (x: int) = 
    if x = 0 
        then true
        else is_odd (x - 1)
and is_odd (x: int) = 
    if x = 0 
        then false
        else is_even (x - 1)


(* C.3 *)
let rec f_rec (n : int) = 
    if n < 3
        then n
        else f_rec (n - 1) + 2 * f_rec (n - 2) + 3 * f_rec (n - 3)

let f_iter (n : int) = 
    if n < 3 
        then n
        else
            let rec iter_help i fn1 fn2 fn3 = 
                if i = n 
                    then fn1 + 2 * fn2 + 3 * fn3
                    else iter_help (i + 1) (fn1 + 2 * fn2 + 3 * fn3) fn1 fn2
            in iter_help 3 2 1 0


(* C.4 *)
let pascal_coefficient row idx =
    match idx with 
        (* Deals with edge coefficients and invalid arguments *)
        | 1 -> 1
        | idx' when idx' = row -> 1
        | idx' when (row < 1 || idx' < 1 || idx' > row) -> failwith "invalid arguments"
        | _ ->
            (* Declares helper that builds the coefficient recursively *)
            let rec pascal_helper i stop coeff = 
                match stop with 
                    (* Base Case Return *)
                    | stop' when i = stop' -> coeff
                    (* Reselects bounds for stop to reflect pascal triangle symmetry *)
                    | stop' when stop' > (row - row / 2) -> pascal_helper i (row - stop + 1) coeff
                    (* Generates the next coefficient *)
                    | _ -> pascal_helper (i + 1) stop (coeff * (row - i) / i)
            (* Starts at the second coefficient since edge coefficients are already dealt with *)
            in pascal_helper 2 idx (row - 1)
