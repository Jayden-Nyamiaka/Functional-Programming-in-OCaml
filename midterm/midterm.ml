(* name: Jayden Nyamiaka *)
(* email: jnyamiak@caltech.edu *)



 (* Part A *)


 (* A.1 *)
(* Function: 
let f n =
  let rec aux n r =
    match n with
      | 0 -> r
      | _ -> aux (n - 1) (n + 1000 * n * r)
  in
    aux n 0

Time Complexity: O(n) 
Reasoning: This function uses a tail call linear iterative helper method to 
compute some term dependent on n. The amount of times the aux method runs is 
dependent on the first variable n and the computing term that will eventually
be returned from the function is held in the second variable r. The aux is 
called with n, and for each run, it recalls itself with n decremented by one
until it returns the result in r once n equals 0. Thus, it will run n times,
yielding linear O(n). *)



 (* A.2 *)
(* Function: 
let rec bounce m n =
  if m = n
    then n
    else
      if m < n
        then m + bounce (m + 1) n
        else n + bounce (m - 1) n

Time Complexity: O(|m - n|)
Reasoning: This function is a linear recursive that keeps running until the 
value of m reaches n, yielding linear recursion depending on the difference 
between their values. If we look at the conditions, we see that the function
only stops recurring once m = n. If m is smaller, the function recalls itself
with m incremented by 1, and if m is larger, it recalls itself with m 
decremented by 1. Thus, until m equals n, each recursive call keeps n at the
same value and brings m one closer to n. Thus, the run time is linearly 
dependent on the absolute value of m - n, yielding O(|m - n|). *)



 (* A.3 *)
(* Function: 
let rec trib n =
  match n with
    | 0 -> 0
    | 1 -> 0
    | 2 -> 1
    | _ -> trib (n - 1) + trib (n - 2) + trib (n - 3)

Time Complexity: O(3^n) (or more specifically O(3^(n-2)) 
                 with the exception of n = 0, 1 which are O(1))
Reasoning: For each recursive call of trib, 3 more calls are made and all 
those 3 then call three more calls of trib and so on until the calls reach 
n = 2, 1, or 0 from which we can then compute all the addition stored on 
the stack frame. So, for large n values, there will be an upper bound of 3^n
trib calls. More specifically though, since the last 3 trib calls (i.e 
trib 2, trib 1, and trib 0) are base cases, this will yield O(3^(n-2)) for
all n >= 2 and O(1) for n = 0, 1 as indicated above, but O(3^n) works as a 
simpler upper bound. It's also true that trib (n - 1) will generate more 
calls than trib (n - 2) and trib (n - 3), and similarly, trib (n - 2) will 
generate more calls than trib (n - 3), but that also still doesn't change the
O(3^n) upper bound. *)



 (* A.4 *)
(* Function: 
let rec weird n =
  match () with
    | _ when n < 1 -> failwith "bad"
    | _ when n = 1 -> 1
    | _ when n mod 3 = 0 -> weird (n / 3)
    | _ when n mod 2 = 0 -> weird (n / 2)
    | _ -> weird (n - 1)

*** Ignore: Used for Testing
let w n =
  let count = ref 0 in
    let rec weird n =
    if (incr count) = () 
      && (Printf.printf "Count %d: n = %d\n" !count n) = () 
      then match () with
        | _ when n < 1 -> failwith "bad"
        | _ when n = 1 -> 1
        | _ when n mod 3 = 0 -> weird (n / 3)
        | _ when n mod 2 = 0 -> weird (n / 2)
        | _ -> weird (n - 1)
    else n
  in weird n
***

Time Complexity: O(log n)
Reasoning: For this function, until the input is 1, we divide by 2 or 3 
if the input is divisible by 2 or 3 respectively and subtract 1 otherwise. 
This would usually yield log of log x behavior, but it's trickier than that 
because it's strictly integer log. In the worst case for n, you'll end up 
in a situation where we keep getting numbers indivisible by 2 and 3 such that
our input must be decremented (getting a number divisible by 2) and then 
halved in the next iteration, which again yields another number indivisible 
by 2 or 3 (not always the case but can happen in the worst case). This 
recurring indivisibility is the worst case (example: n = 383 -> 16 runs). 
This repeated behavior of decrement and halving evinces 2 * log2 n steps, 
expressed by O(2 * log2 n) = O(log n). *)



 (* A.5 *)
(* Function: "Collatz Conjecture"
let rec f n =
  match () with
    | _ when n < 1 -> failwith "bad"
    | _ when n = 1 -> 1
    | _ when n mod 2 = 0 -> f (n / 2)
    | _ -> f (3 * n + 1)

We cannot analyze this method for its time complexity or prove it terminates 
because its recursion doesn't follow a simple pattern that we can break into 
smaller instances of the same pattern. If n is even, n is divided by 2, but 
if n is odd, we do 3n + 1. And, since we know the product of any two odd 
numbers is also odd, then 3n + 1 always results in an even number. Thus, 
we have some oscillating behaviour of n instead of the solely incrementing or
decrementing recursion that we've analyzed so far. Instead of asymptotically 
approaching some value like 0, the recursion jumps up and down. So far, we've
worked strictly with recursion dependent on solely increasing or decreasing 
values because then our bigger problem would be split into instances of 
smaller problems, yielding some asymptotic behavior. Here, however, our 
function may take our initial problem and make an instance of an even bigger
problem, so instead of getting closer to terminating at our solution, we get
farther away. Thus, without that asymptotic behavior, we can't analyze. *)




 (* PART B *)


 (* B.1: merge_sort3 *)

 (* B.1.a: split3 *)
let split3 lst = 
  let rec iter lst ls1 ls2 ls3 i =
    match (i, lst) with
      | (_, []) -> (List.rev ls1, List.rev ls2, List.rev ls3)
      | (0, h :: t) -> iter t (h :: ls1) ls2 ls3 1 
      | (1, h :: t) -> iter t ls1 (h :: ls2) ls3 2 
      | (_, h :: t) -> iter t ls1 ls2 (h :: ls3) 0 
  in iter lst [] [] [] 0


 (* B.1.b: merge3 *)
(* Assumes lists are sorted *)
let merge3 lst1 lst2 lst3 = 
  let merge2 lst1 lst2 =
    let rec iter out lst1 lst2 =
      match (lst1, lst2) with
        | ([], []) -> List.rev out
        | ([], _) -> (List.rev out) @ lst2
        | (_, []) -> (List.rev out) @ lst1
        | (h1 :: t1, h2 :: t2) -> 
            if h1 < h2 
              then iter (h1 :: out) t1 lst2
              else iter (h2 :: out) lst1 t2
    in iter [] lst1 lst2
  in merge2 (merge2 lst1 lst2) lst3


 (* B.1.c: merge_sort3 *)
let rec merge_sort3 lst =
  match lst with
    | []
    | [_] -> lst
    | _ -> match split3 lst with (ls1, ls2, ls3) -> 
      merge3 (merge_sort3 ls1) (merge_sort3 ls2) (merge_sort3 ls3)



 (* B.2: Block sorting *)

 (* B.2.a: smallest_index *)
let smallest_index = function 
  | [] -> invalid_arg "smallest_index: not enough elements"
  | [_] -> 0
  | h :: t ->  
    let rec iter i smidx smval = function
      | [] -> smidx
      | h :: t -> 
        if h < smval 
          then iter (i + 1) i h t
          else iter (i + 1) smidx smval t
    in iter 1 0 h t


 (* B.2.b: flip_n *)
(* Assumes argument n is non negative *)
let flip_n n lst = 
  let rec iter count new_lst rem_lst = 
    match rem_lst with 
      | _ when count = n -> new_lst @ rem_lst
      | [] -> invalid_arg "flip_n: not enough elements"
      | h :: t -> iter (count + 1) (h :: new_lst) t
  in iter 0 [] lst


 (* B.2.c: block_sort1 *)
let block_sort1 lst = 
  if lst = [] 
    then []
    else flip_n (smallest_index lst + 1) lst


 (* B.2.d: block_sort_r and block_sort_i *)
let rec block_sort_r lst =
  match block_sort1 lst with
    | []
    | [_] -> lst
    | h :: t -> h :: (block_sort_r t)

let block_sort_i lst = 
  let rec iter new_lst old_lst = 
    match old_lst with 
      | [] -> List.rev new_lst
      | h :: t -> iter (h :: new_lst) (block_sort1 t)
  in iter [] (block_sort1 lst)



 (* B.3: linrec *)

 (* B.3.a: Implementing linrec *)
let linrec is_base on_base split combine =
  let rec f x =
    if is_base x then
      on_base x
    else
      let res = split x in
      combine (fst res) ( f (snd res) )
  in f


 (* B.3.b: insert_r *)
let insert_r item =
  (* two base cases: the empty list
   * and when the item < the first element of the list *)
  let is_base lst = lst = [] || item <= List.hd lst in

  (* for both base cases, put the item on the front of the list *)
  let on_base lst = item :: lst in

  (* split the list.  Hint: structural recursion. *)
  let split lst = (List.hd lst, List.tl lst) in

  (* combine to get the final result *)
  let combine first rest_after_rec = first :: rest_after_rec in

    linrec is_base on_base split combine


 (* B.3.c: insertion_sort *)
let insertion_sort =
  (* base case: the list is empty *)
  let is_base lst = lst = [] in

  (* if it's a base case, return the empty list *)
  let on_base _ = [] in

  (* otherwise, split (hint: structural recursion again) *)
  let split lst = (List.hd lst, List.tl lst) in

  (* then combine *)
  let combine first rest_after_rec = insert_r first rest_after_rec in

    linrec is_base on_base split combine



 (* B.4: binrec *)

 (* B.4.a: Implementing binrec *)
let binrec is_base on_base split combine =
  let e1 (a, _, _) = a and e2 (_, b, _) = b and e3 (_, _, c) = c in
  let rec f x =
    if is_base x then
      on_base x
    else
      let res = split x in
      combine (e1 res) ( f (e2 res) ) ( f (e3 res) ) 
  in f

 
 (* B.4.b: quicksort *)
let quicksort =
  let is_base lst = lst = [] in
  let on_base _ = [] in
  let split lst =
    match lst with
      | [] -> invalid_arg "quicksort: can't split"
      | h :: t -> (h, List.filter ( (>) h) t, List.filter ( (<=) h) t)
  in
  let combine pivot lt ge = lt @ (pivot :: ge) in
    binrec is_base on_base split combine



 (* B.5: tailrec *)

 (* B.5.a: Implementing tailrec *)
let tailrec is_base on_base next =
  let rec f inputs =
    if is_base inputs then
      on_base inputs
    else
      f (next inputs)
  in f


 (* B.5.b: insert_i *)
let insert_i item lst =
  let is_base (_, rest) = rest = [] || item <= List.hd rest in
  let on_base (prev, rest) = (List.rev prev) @ (item :: rest) in
  let next (prev, rest) = ( (List.hd rest) :: prev, List.tl rest) in
  let iter = tailrec is_base on_base next in
    iter ([], lst)


 (* B.5.c: insertion_sort_i *)
let insertion_sort_i lst =
  let is_base (_, rest) = rest = [] in
  let on_base (prev, _) = prev in
  let next (prev, rest) = (insert_i (List.hd rest) prev, List.tl rest) in
  let iter = tailrec is_base on_base next in
    iter ([], lst)




 (* PART C *)

type tree = 
  | Leaf 
  | Node of int * int * tree * tree


 (* C.1: member *)
let rec member find aa = 
  match aa with 
    | Leaf -> false
    | Node (_, vl, sleft, sright) -> 
      if find = vl
        then true
        else (member find sleft) || (member find sright)



 (* C.2: AA rotations *)
 (*
let skew aa = 
  match aa with
    | Leaf (* So topmost is not a leaf *)
    | Node (_, _, Leaf, _) -> aa (* So left subtree is not a leaf *)
    | Node (lvl, _, Node (lvl_l, _, _, _), _) when lvl <> lvl_l -> aa
    (* So left horizontal link exists btwn aa and its left subtree *)
    (* Thus perform skew *)
    | Node (lvl, vl, Node (_, vl_l, sleft_l, sright_l), sright) ->
        Node (lvl, vl_l, sleft_l, Node (lvl, vl, sright_l, sright))

let split aa = 
  match aa with
    | Leaf (* So topmost is not a leaf *)
    | Node (_, _, _, Leaf) (* So right subtree is not a leaf *)
    | Node (_, _, _, Node (_, _, _, Leaf)) -> aa
    (* So right subtree of right subtree is not a leaf *)
    | Node (lvl, _, _, Node (lvl_r, _, _, Node (lvl_rr, _, _, _)))
        when lvl <> lvl_r || lvl <> lvl_rr -> aa
    (* So 2 consecutive right horizontal links exists from aa *)
    (* Thus perform split *)
    | Node (lvl, vl, sleft, Node (_, vl_r, sleft_r, sright_r)) ->
        Node(lvl + 1, vl_r, Node(lvl, vl, sleft, sleft_r), sright_r)
  *)
(* Alternate Implementation *)
let skew aa = 
  match aa with
    | Node (lvl, vl, Node (lvl_l, vl_l, sleft_l, sright_l), sright) 
      when lvl = lvl_l ->
        Node (lvl, vl_l, sleft_l, Node (lvl, vl, sright_l, sright))
    | _ -> aa

let split aa = 
    match aa with 
      | Node (lvl, _, _, Node (lvl_r, _, _, Node (lvl_rr, _, _, _)))
          when lvl <> lvl_r || lvl <> lvl_rr -> aa
      | Node (_, _, _, Node (_, _, _, Leaf)) -> aa
      | Node (lvl, vl, sleft, Node (_, vl_r, sleft_r, sright_r)) ->
          Node(lvl + 1, vl_r, Node(lvl, vl, sleft, sleft_r), sright_r)
      | _ -> aa


    
 (* C.3: insert *)
let rec insert item t =
  match t with
    | Leaf -> Node(1, item, Leaf, Leaf)
    | Node (_, v, _, _) when v = item -> t
    | Node (lvl, v, l, r) -> 
        if (item < v) 
          then split (skew (Node(lvl, v, insert item l, r)))
          else split (skew (Node(lvl, v, l, insert item r)))



 (* Extra C: Visualizing trees *)
let print_tree tree =
  let blanks n = String.make n ' ' in
  let rec aux tree indent =
    let ind = blanks indent in
      match tree with
        | Leaf -> Printf.printf "%sLeaf\n" ind
        | Node (d, v, l, r) ->
          begin
            Printf.printf "%sNode[(%d) [level %d]\n" ind v d;
            aux l (indent + 2);
            Printf.printf "%s  ----\n" ind;
            aux r (indent + 2);
            Printf.printf "%s]\n" ind;
          end
  in
    aux tree 0