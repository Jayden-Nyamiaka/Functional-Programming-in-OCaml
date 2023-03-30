 (* Part A *)


 (* A.1 *)
let fibonacci n =
  if n = 0 then 0 else
  let i = ref 1
  and fi = ref 1
  and fim1 = ref 0
  and temp = ref 0 in
    while !i < n do
      temp := !fi;
      fi := !fi + !fim1;
      fim1 := !temp;
      i := !i + 1
    done;
    !fi


let fibonacci2 n =
  if n = 0 then 0 else
  let fi = ref 1
  and fim1 = ref 0
  and temp = ref 0 in
    for _ = 1 to n - 1 do
      temp := !fi;
      fi := !fi + !fim1;
      fim1 := !temp;
    done;
    !fi



 (* A.2 *)
let bubble_sort arr =
  let n = Array.length arr in
  for i = n - 1 downto 0 do
    for j = 0 to i - 1 do
      if arr.(j) > arr.(j + 1) then
        let temp = arr.(j) in
        arr.(j) <- arr.(j + 1);
        arr.(j + 1) <- temp;
    done
  done




 (* Part B *)


 (* B.1 *)
let meters_per_foot = 0.3048

let get_meters len =
  match len with
    | `Meter m -> m
    | `Foot f -> f *. meters_per_foot
    | `Inch i -> i /. 12. *. meters_per_foot

let length_add a b = `Meter (get_meters a +. get_meters b)



 (* B.2 *)
let grams_per_slug = 14593.903203

let get_grams mass =
  match mass with
    | `Gram g -> g
    | `Kilo kg -> kg *. 1000.
    | `Slug s -> s *. grams_per_slug 

let mass_add a b = `Gram (get_grams a +. get_grams b)


let get_seconds time =
  match time with
    | `Second s -> s
    | `Minute m -> m *. 60.
    | `Hour h -> h *. 3600. 
    | `Day d -> d *. 86400. 

let time_add a b = `Second (get_seconds a +. get_seconds b)



 (* B.3 *)
let unit_add a b = 
  match (a, b) with
    | (`Length x, `Length y) -> `Length (length_add x y)
    | (`Mass x, `Mass y) -> `Mass (mass_add x y)
    | (`Time x, `Time y) -> `Time (time_add x y)
    | _ -> failwith "Unit class tags don't match"
(* We do not get a combinatorial explosion as we add more unit classes 
   to unit_add because we've captured all possible units under their 
   appropriate class tags. Since we've done this, we only need to make 
   sure that their class tags match. So, with any new units under any 
   existing class tags, we don't have to change the function, and for 
   adding any new class, we only need to add one more match case. By 
   abstracting well and grouping units into classes, we can avoid a 
   combinatorial explosion. *)




 (* Part C *)


 (* C.1 *)
let rec make_gram g = 
  let matching_types other = 
    match other#unit_type with
          | `Gram
          | `Slug -> true
          | _ -> false
  in
    object
      method get_grams = g
      method get_slugs = g /. 14593.903203
      method unit_type = `Gram
      method compatible other = matching_types other
      method add other = 
        if (matching_types other)
          then make_gram (g +. other#get_grams)
          else failwith "Units are incompatible"
    end



 (* C.2 *)

 (* C.2.a *)
(* Define a number as a message-passing object. *)
(* "i" is an int. *)
let rec make_number i =
  object
    method value = i
    method show = string_of_int i
    method is_zero = i = 0
    method is_number = true
    method evaluate _ _ = make_number i  (* must evaluate to an object *)
    method derive _ = make_number 0  (* derivative of a number is 0 *)
  end

(* Define a variable as a message-passing object. *)
(* "v" is a string. *)
let rec make_variable v =
  object
    method value = failwith "variable has no numerical value"
    method show  = v
    method is_zero = false
    method is_number = false
    method evaluate v' n =
      if v = v'
        then make_number n
        else make_variable v
    method derive v' =
      if v = v'
        then make_number 1  (* d/dx(x) = 1 *)
        else make_number 0  (* d/dx(y) = 0 *)
  end

(* Evaluate a message-passing expression with a number
   substituted for a variable. *)
let evaluate expr v n = expr#evaluate v n

(* Return the string representation of an expression. *)
let show expr = expr#show

(* Return the derivative of an expression. *)
let differentiate expr v = expr#derive v

(* Define a sum as a message-passing object. *)
let rec make_sum expr1 expr2 =
  match () with
    | _ when expr1#is_zero -> expr2  (* 0 + expr = expr *)
    | _ when expr2#is_zero -> expr1  (* expr + 0 = expr *)
    | _ when expr1#is_number && expr2#is_number ->  (* add numbers *)
          make_number (expr1#value + expr2#value)
    | _ when not expr1#is_number && not expr2#is_number
          && expr1#show = expr2#show -> 
            make_product (make_number 2) expr1  (* var + var = 2 * var *)
    | _ ->  (* create a new object representing the sum *)
          object
            method value = failwith "sum expression has no numerical value"
            method show = "(" ^ expr1#show ^ " + " ^ expr2#show ^ ")"
            method is_zero = false
            method is_number = false
            method evaluate v n =
              make_sum (expr1#evaluate v n) (expr2#evaluate v n)
            method derive v =
              make_sum (expr1#derive v) (expr2#derive v)
          end
(* Define a product as a message-passing object. *)
and make_product expr1 expr2 =
  match () with
    | _ when expr1#is_zero || expr2#is_zero -> make_number 0 (* anything x 0 = 0 *)
    | _ when expr1#is_number && expr2#is_number ->  (* multiply numbers *)
          make_number (expr1#value * expr2#value)
    | _ when expr1#is_number && expr1#value = 1 -> expr2 (* 1 x expr = expr *)
    | _ when expr2#is_number && expr2#value = 1 -> expr1 (* expr x 1 = expr *)
    | _ ->  (* create a new object representing the product *)
          object
            method value = failwith "product expression has no numerical value"
            method show = "(" ^ expr1#show ^ " * " ^ expr2#show ^ ")"
            method is_zero = false
            method is_number = false
            method evaluate v n =
              make_product (expr1#evaluate v n) (expr2#evaluate v n)
            method derive v =
              make_sum (make_product (expr1#derive v) expr2)
                    (make_product expr1 (expr2#derive v))
          end


 (* C.2.b *)

(* f = x^3*y + 3*x^2*y^2 + y^2 + 2 *)
let f =
  make_sum
   (make_product
    (make_variable "x")
    (make_product
     (make_variable "x")
     (make_product
      (make_variable "x")
      (make_variable "y"))))
   (make_sum
    (make_product
     (make_number 3)
     (make_product
      (make_variable "x")
      (make_product
       (make_variable "x")
       (make_product
        (make_variable "y")
        (make_variable "y")))))
    (make_sum
     (make_product
      (make_variable "y")
      (make_variable "y"))
     (make_number 2))) ;;
(*  val f :
    < derive : string -> 'a; evaluate : string -> int -> 'a; is_number :
      bool; is_zero : bool; show : string; value : int >
    as 'a = <obj>  *)

let dfdx = differentiate f "x" ;;
(*  val dfdx :
    < derive : string -> 'a; evaluate : string -> int -> 'a; is_number :
      bool; is_zero : bool; show : string; value : int >
    as 'a = <obj>  *)

show dfdx ;;
(*  - : string =
    "(((x * (x * y)) + (x * (2 * (x * y)))) + (3 * (2 * (x * (y * y)))))"  *)

show (evaluate f "x" 3) ;;
(*  - : string =
    "((3 * (3 * (3 * y))) + ((3 * (3 * (3 * (y * y)))) + ((y * y) + 2)))"  *)

show (evaluate (evaluate f "x" 3) "y" 4) ;;
(*  - : string = "558"  *)

show (evaluate (evaluate dfdx "x" 3) "y" 4) ;;
(*  - : string = "396"  *)
