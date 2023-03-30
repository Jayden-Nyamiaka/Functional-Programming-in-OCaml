 (* A.1 *)
let rec last_sublist = function
  | [] -> invalid_arg "last_sublist: empty list"
  | [x] -> [x]
  | _ :: t -> last_sublist t


 (* A.2 *)
let reverse lst =
  let rec iter oldL newL = 
    match oldL with 
      | [] -> newL
      | h :: t -> iter t (h :: newL)
  in iter lst []


 (* A.3 *)
let rec square_list = function
 | [] -> []
 | h :: t -> (h * h) :: square_list t

let square_list2 items = List.map (fun x -> x * x) items


 (* A.4 *)
(*
let square_list items =
  let rec iter things answer =
    match things with
      | [] -> answer
      | h :: t -> iter t ((h * h) :: answer)
  in iter items []

This doesn't work as intended because the :: constructor creates list by appending the left side 
element to the right side list, so because Louis is immediately appending the first computed head 
to the answer, the first head becomes the most right in the answer list. Then, by continuously 
doing this, the elements that appear as heads first (which are at the front of the original list) 
become the most right elements in the answer list (which are at the back of the new list). This 
then produces the undesired reverse order.

let square_list items =
  let rec iter things answer =
    match things with
      | [] -> answer
      | h :: t -> iter t (answer :: (h * h))
  in iter items []

The reason this one doesn't work is simple. The :: constructor expects 2 arguments, which are an 
element to the left of it and a list to the right. Here, they were provided in reverse order such 
that the computed element is provided to the right and the list is provided to the left. It 
doesn't work because a list wasn't provided as the right side argument.
*)

let square_list_i_fixed items =
  let rec iter things answer =
    match things with
      | [] -> answer 
      | h :: t -> iter t (answer @ [h * h])
  in iter items []

(* We can modify Louis’ second solution slightly, as done above, to make it produce the desired 
   list, but it's far from efficient. Because Ocaml implements list as singly linked list, the 
   append operator has to start at the head of the first list and recursively trace that head back 
   to its last element node in order to append two lists together. So, if the first list has n 
   elements, append has Theta(n) time complexity. Since our square_list_i_fixed function appends 
   every computed head at the back of our saved answer list, that gives our function Theta(N!) 
   time complexity where N is the number of elements in our original items list. If done right, 
   however, a time efficient implementation of this function should have Theta(N) time complexity, 
   as does our implementation of the recursive square_list function above. So although this 
   function yields the desired output, this function is not time efficient as it could be 
   implemented much better. *)


 (* A.5 *)
let count_negative_numbers lst =
  let rec counter count = function 
    | [] -> count
    | (h :: t) when h < 0 -> counter (count + 1) t
    | (_ :: t) -> counter count t
  in counter 0 lst


 (* A.6 *)
let power_of_two_list n =
  if n < 0 then invalid_arg "power_of_two_list: negative number" else
  let rec helper power i = 
    match i with
      | 0 -> []
      | i' -> power :: helper (power * 2) (i - 1)
  in helper 1 n


 (* A.7 *)
let prefix_sum lst =
  let rec helper sum remain = 
    match remain with
      | [] -> []
      | (h :: t) -> let h_sum = sum + h in
            h_sum :: helper (h_sum) t
  in helper 0 lst


 (* A.8 *)
let deep_reverse lst =
  let rec iter oldL newL = 
    match oldL with 
      | [] -> newL
      | h :: t -> iter t ( reverse h :: newL)
  in iter lst []


 (* A.9 *)
type 'a nested_list =
 | Value of 'a
 | List of 'a nested_list list

let deep_reverse_nested (lst: 'a nested_list) =
  let rec iter oldL newL = 
    match oldL with 
      | Value _ -> oldL
      | List [] -> List newL
      | List (h :: t) -> iter (List t) ( (iter h []) :: newL )
  in iter lst []

(* Thought Process:
reversed head --> (iter h [])
tack on the reversed head to the :: newList --> ( rev_head :: newL )
continue doing computation with oldList and new newList --> iter (List t) (newL with rev_head)
Full line --> iter (List t) ( (iter h []) :: newL )       *)




 (* B.1 *)
let rec quicksort cmp lst =
  let neg a b = (cmp a b) = false in
  match lst with
  | [] -> []
  | (h :: t) -> (quicksort cmp (List.filter (neg h) t) ) 
                @ [h] 
                @ (quicksort cmp (List.filter (cmp h) t) )
  

 (* B.2 *)
(* This is an instance of generative recursion because the data we are recursing on doesn't only 
   depend on the structure of our data. Here, our recursion also depends on the problem we are 
   trying to solve such that the data gets switched around at each recursive step. Since our 
   continued recursion depends on how cmp and filter compute and organize the tail, not the pure 
   structure of the tail itself, it's clear we have generative recursion. *)


 (* B.3 *)
(* Ben's version doesn't work because he left out the length 1 list base case. In setting up merge 
   sort, the list has to be split up into two halves using the even_half and odd_half helper 
   functions. However, in order for this splitting into 2 lists to work, we need at least 2 
   elements, so we need to weed out the length 1 list out as a base case. Without this base case, 
   any length 1 list [x] that we try to sort would follow an infinite loop of being split into 
   eh = [] and oh = [x] then calling merge_sort on oh (which is merge_sort [x]). So, merge_sort [x] 
   would recur infinitely, resulting in a stack overflow during evaluation on any use of merge_sort 
   as they would all eventually lead to this loop. *)


 (* B.4 *)
let rec insert_in_order cmp new_result a_list =
  match a_list with
    | [] -> [new_result]
    | h :: t when cmp new_result h -> new_result :: a_list
    | h :: t ->  h :: insert_in_order cmp new_result t

let rec insertion_sort cmp a_list =
  match a_list with
    | [] -> []
    | h :: t -> insert_in_order cmp h (insertion_sort cmp t)

(* This is structural recursion because our recursive call on the tail depends solely on the 
   structure of our list. No matter what computation occurs during insertion sort, we are still 
   going to perform our recursive call on the tail of the list, taking on one element at a time. 
   Because our computation has no effect on our recursion, our recursion is clearly structural. *)




 (* C.1 *)
let rec subsets = function
 | [] -> [[]]
 | h :: t -> let rest = subsets t in
     rest @ (List.map (fun lst -> h :: lst) rest)

(* This works by tacking each head on to every subset. For each head, we recursively generate the 
   all the subsets of the tail and then tack the head on to the front of all of those subsets. 
   This generates every possible set combination because each element of the list has been the 
   head of the recursing list at some point, so they've all been added to every possible 
   combination. *)


 (* C.2 *)
let rec accumulate op initial sequence =
  match sequence with
    | [] -> initial
    | h :: t -> op h (accumulate op initial t)

let map p sequence =
  accumulate (fun x r -> (p x) :: r) [] sequence

let append seq1 seq2 =
  accumulate (fun x r -> x :: r) seq2 seq1

let length sequence =
  accumulate (fun x r -> 1 + r) 0 sequence


 (* C.3 *)
let rec accumulate_n op init seqs =
  match seqs with
    | [] -> failwith "empty list"
    | [] :: _ -> []   (* assume all sequences are empty *)
    | h :: t -> accumulate op init (map (fun lst -> List.hd lst) seqs) 
                :: accumulate_n op init (map (fun lst -> List.tl lst) seqs)


 (* C.4 *)
let rec map2 f x y =
  match (x, y) with
    | ([], []) -> []
    | ([], _) -> failwith "unequal lists"
    | (_, []) -> failwith "unequal lists"
    | (h1 :: t1, h2:: t2) -> (f h1 h2) :: (map2 f t1 t2)

let dot_product v w = accumulate (+) 0 (map2 ( * ) v w)

let matrix_times_vector m v = map (fun mat_row -> dot_product mat_row v) m

let transpose mat = accumulate_n (fun x y -> x :: y) [] mat

let matrix_times_matrix m n =
  let cols = transpose n in
    map (fun mat_row -> matrix_times_vector cols mat_row) m