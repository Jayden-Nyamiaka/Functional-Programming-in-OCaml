(* Jayden Nyamiaka *)

(* Part A: The Environmental Model *)



(* A.1: Factorial *)
(*
CODE:
  let factorial n =
    let rec iter m r =
      if m = 0
        then r
        else iter (m - 1) (r * m)
    in iter n 1
  in
    factorial 3



WORK:
  -- Desugar let factorial n = ... to:
    let factorial = fun n -> ...
   
  FRAME 0 (initial environment)
    parent: none
    bindings:
      - : [primitive function -]
      * : [primitive function *]
  
  -- Evaluate expression (fun n -> ...) in current frame (F0)
  
  FUNCTION 0 (fun n -> ...)
    env: FRAME 0    (* environment function was defined in
                       AKA enclosing environment *)
    param: n
    body: 
      let rec iter m r =
        if m = 0
          then r
          else iter (m - 1) (r * m)
      in iter n 1

  -- Create new frame (F1), whose parent is the current frame (F0)
  -- Bind name (factorial) to expression (FUNCTION 0) in the new frame (F1)

  FRAME 1
    parent: FRAME 0
    bindings:
      factorial : FUNCTION 0

  -- Evaluate body (factorial 3) in the new frame (F1), which is now the current frame
  -- Evaluating factorial 3
    -- 3 evaluates to 3, factorial evaluates to FUNCTION 0
    -- Create new frame (F2), whose parent is the frame of the function (F0)
    -- Bind parameters to arguments (bind n to 3) in the new frame (F2)
  
  FRAME 2
    parent: FRAME 0
    bindings:
      n: 3
  
  -- Evaluate the function body (FUNCTION 0) in the new frame (F2), which is now the current frame
  -- Create new frame (F3), whose parent is the current frame (F2)
  -- Bind recursive name (iter) to a dummy value in the new frame (F3)

  FRAME 3
    parent: FRAME 2
    bindings:
      iter : <dummy value>
      
  -- Evaluate expression (let rec iter m r = ...) in new frame (F3)

  FUNCTION 1 (let rec iter m r = ...)
    env: FRAME 3    (* environment function was defined in
                       AKA enclosing environment *)
    param: m r
    body: 
      if m = 0
        then r
        else iter (m - 1) (r * m)
  
  -- Rebind recursive name (iter) to evaluated expression (FUNCTION 1)

  FRAME 3
    parent: FRAME 2
    bindings:
      iter : FUNCTION 1

  -- Evaluate body (iter n 1) in new frame (F3), which is now the current frame
  -- Evaluating iter n 1
    -- n evaluates to 3, 1 evaluates to 1, iter evaluates to FUNCTION 1
    -- Create new frame (F4), whose parent is the frame of the function (F3)
    -- Bind parameters to arguments (bind m to 3, bind r to 1) in the new frame (F4)
  
  FRAME 4
    parent: FRAME 3
    bindings:
      m : 3
      r: 1

  -- Evaluate the function body (FUNCTION 1) in the new frame (F4), which is now the current frame
    -- m = 0 is false --> else iter (m - 1) (r * m)
  
  -- Evaluating iter (m - 1) (r * m)
    -- (m - 1) evaluates to 2, (r * m) evaluates to 3, iter evaluates to FUNCTION 1
    -- Create new frame (F5), whose parent is the frame of the function (F3)
    -- Bind parameters to arguments (bind m to 2, bind r to 3) in the new frame (F5)
  
  FRAME 5
    parent: FRAME 3
    bindings:
      m : 2
      r: 3

  -- Evaluate the function body (FUNCTION 1) in the new frame (F5), which is now the current frame
    -- m = 0 is false --> else iter (m - 1) (r * m)
  
  -- Evaluating iter (m - 1) (r * m)
    -- (m - 1) evaluates to 1, (r * m) evaluates to 6, iter evaluates to FUNCTION 1
    -- Create new frame (F6), whose parent is the frame of the function (F3)
    -- Bind parameters to arguments (bind m to 1, bind r to 6) in the new frame (F6)
  
  FRAME 6
    parent: FRAME 3
    bindings:
      m : 1
      r: 6
  
  -- Evaluate the function body (FUNCTION 1) in the new frame (F6), which is now the current frame
    -- m = 0 is false --> else iter (m - 1) (r * m)
  
  -- Evaluating iter (m - 1) (r * m)
    -- (m - 1) evaluates to 0, (r * m) evaluates to 6, iter evaluates to FUNCTION 1
    -- Create new frame (F7), whose parent is the frame of the function (F3)
    -- Bind parameters to arguments (bind m to 0, bind r to 6) in the new frame (F7)

  FRAME 7
    parent: FRAME 3
    bindings:
      m : 0
      r: 6
  
  -- Evaluate the function body (FUNCTION 1) in the new frame (F7), which is now the current frame
    -- m = 0 is true --> then r
    -- r evaluates to 6 
  
  -- Everything goes away, leaving us with a result of 6 in the initial environment



DESCRIPTION (FRAMES & FUNCTIONS):

  FRAME 0 (initial environment)
    parent: none
    bindings:
      - : [primitive function -]
      * : [primitive function *]
  
  FUNCTION 0 (fun n -> ...)
    env: FRAME 0    (* environment function was defined in
                       AKA enclosing environment *)
    param: n
    body: 
      let rec iter m r =
        if m = 0
          then r
          else iter (m - 1) (r * m)
      in iter n 1

  FRAME 1
    parent: FRAME 0
    bindings:
      factorial : FUNCTION 0
  
  FRAME 2
    parent: FRAME 0
    bindings:
      n: 3

  FRAME 3
    parent: FRAME 2
    bindings:
      iter : <dummy value>

  FUNCTION 1 (fun m r -> ...)
    env: FRAME 3    (* environment function was defined in
                       AKA enclosing environment *)
    param: m r
    body: 
      if m = 0
        then r
        else iter (m - 1) (r * m)

  FRAME 3
    parent: FRAME 2
    bindings:
      iter : FUNCTION 1
  
  FRAME 4
    parent: FRAME 3
    bindings:
      m : 3
      r: 1

  FRAME 5
    parent: FRAME 3
    bindings:
      m : 2
      r: 3
  
  FRAME 6
    parent: FRAME 3
    bindings:
      m : 1
      r: 6
 
  FRAME 7
    parent: FRAME 3
    bindings:
      m : 0
      r: 6
*)




(* A.2: Recursion using ref cells *)

(* Recursive Version
let rec factorial n =
  if n = 0 then 1 else n * factorial (n - 1)
*)

(* Simulated Recursion *)
let factorial m =
  let f = ref (fun _ -> 0) in
  f := (fun n -> if n = 0 then 1 else n * !f (n - 1));
  !f m





(* Part B: Imperative Objects *)



exception Stat_error of string


(* B.1: make_stat_1 *)
let make_stat_1 () =
  let sum = ref 0. 
  and sumsq = ref 0.
  and n = ref 0 in
  object
    method append x =
      n := !n + 1;
      sum := !sum +. x;
      sumsq := !sumsq +. x *. x
    method clear = 
      n := 0;
      sum := 0.;
      sumsq := 0.
    method mean = 
      if !n = 0 then raise (Stat_error "need at least one value for mean") else
        !sum /. (float_of_int !n)
    method variance =
      if !n = 0 then raise (Stat_error "need at least one value for variance") else
        (!sumsq -. ((!sum *. !sum) /. (float_of_int !n))) /. (float_of_int !n)
    method stdev =
      if !n = 0 then raise (Stat_error "need at least one value for stdev") else
        Float.sqrt ((!sumsq -. ((!sum *. !sum) /. (float_of_int !n))) /. (float_of_int !n))
  end




(* B.2: make_stat_2 *)
let make_stat_2 () =
  let sum = ref 0. 
  and sumsq = ref 0.
  and n = ref 0 in
  object (self)
    method private _multiple_vals name = 
      if !n = 0 then raise (Stat_error ("need at least one value for " ^ name))
    method private _variance = 
      (!sumsq -. ((!sum *. !sum) /. (float_of_int !n))) /. (float_of_int !n)
      
    method append x =
      n := !n + 1;
      sum := !sum +. x;
      sumsq := !sumsq +. x *. x
    method clear = 
      n := 0;
      sum := 0.;
      sumsq := 0.
    method mean = 
      self#_multiple_vals "mean";
      !sum /. (float_of_int !n)
    method variance =
      self#_multiple_vals "variance";
      self#_variance
    method stdev =
      self#_multiple_vals "stdev";
      Float.sqrt self#_variance
  end





(* Part C: Modules and functors *)

(* Signature for priority queues. *)
module type PRIORITY_QUEUE =
  sig
    exception Empty

    type elem      (* Abstract type of elements of queue. *)
    type t         (* Abstract type of queue. *)

    val empty      : t                (* The empty queue.         *)
    val is_empty   : t -> bool        (* Check if queue is empty. *)
    val insert     : t -> elem -> t   (* Insert item into queue.  *)
    val find_min   : t -> elem        (* Return minimum element.  *)
    val delete_min : t -> t           (* Delete minimum element.  *)
    val from_list  : elem list -> t   (* Convert list to queue.   *)
  end



(* C.1: PriorityQueue module *)
module PriorityQueue : (PRIORITY_QUEUE with type elem = int) =
  struct
    exception Empty

    type elem = int

    (*
     * Data type: either
     * -- a Leaf, or
     * -- a Node of (rank, item, left heap, right heap).
     *)
    type t = Leaf | Node of int * elem * t * t


    (* Private (Unexported) Functions *)
    let get_rank = function 
      | Leaf -> 0
      | Node (r, _, _, _) -> r 
    
    let rec merge h1 h2 =  
      let new_heap minn left right = 
        let rank = (Int.min (get_rank left) (get_rank right)) + 1 in
          Node (rank, minn, left, right)
      in
      match (h1, h2) with
        | (Leaf, Leaf) -> Leaf
        | (_, Leaf) -> h1
        | (Leaf, _) -> h2
        | (Node (_, minn1, left1, right1), Node (_, minn2, _, _)) 
          when (Stdlib.compare minn1 minn2) < 0 ->
            new_heap minn1 left1 (merge right1 h2)
        | (_, Node (_, minn2, left2, right2)) ->
            new_heap minn2 left2 (merge right2 h1)
      
    
    (* Exported Functions *)
    let empty = Leaf

    let is_empty = function
      | Leaf -> true
      | Node _ -> false

    let insert h e = merge h (Node (0, e, Leaf, Leaf))

    let find_min h = 
      match h with
        | Leaf -> raise Empty
        | Node (_, minn, _, _) -> minn
    
    let delete_min h = 
      match h with
        | Leaf -> raise Empty
        | Node (_, _, left, right) -> merge left right
    
    let from_list lst =
      let rec iter rest heap = 
        match rest with
          | [] -> heap
          | (h :: t) -> iter t (insert heap h)
      in iter lst empty
  end



let heap_sort raw_lst = 
  let rec iter heap lst = 
    if PriorityQueue.is_empty heap 
      then List.rev lst
      else iter (PriorityQueue.delete_min heap) 
                ( (PriorityQueue.find_min heap) :: lst)
  in iter (PriorityQueue.from_list raw_lst) []




(* C.2: MakePriorityQueue functor *)

(* Signature for ordered objects. *)
module type ORDERED_TYPE =
  sig
    type t
    val compare : t -> t -> int
  end

(* Instance of ORDERED_TYPE *)
module OrderedString =
  struct
    type t = string
    let compare x y =
      if x = y then 0 else if x < y then -1 else 1
  end


module MakePriorityQueue (Elt : ORDERED_TYPE)
: (PRIORITY_QUEUE with type elem = Elt.t) =
  struct
    exception Empty

    type elem = Elt.t

    (*
     * Data type: either
     * -- a Leaf, or
     * -- a Node of (rank, item, left heap, right heap).
     *)
    type t = Leaf | Node of int * elem * t * t


    (* Private (Unexported) Functions *)
    let get_rank = function 
      | Leaf -> 0
      | Node (r, _, _, _) -> r 
    
    let rec merge h1 h2 =  
      let new_heap minn left right = 
        let rank = (Int.min (get_rank left) (get_rank right)) + 1 in
          Node (rank, minn, left, right)
      in
      match (h1, h2) with
        | (Leaf, Leaf) -> Leaf
        | (_, Leaf) -> h1
        | (Leaf, _) -> h2
        | (Node (_, minn1, left1, right1), Node (_, minn2, _, _)) 
          when (Elt.compare minn1 minn2) < 0 ->
            new_heap minn1 left1 (merge right1 h2)
        | (_, Node (_, minn2, left2, right2)) ->
            new_heap minn2 left2 (merge right2 h1)
      
    
    (* Exported Functions *)
    let empty = Leaf

    let is_empty = function
      | Leaf -> true
      | Node _ -> false

    let insert h e = merge h (Node (0, e, Leaf, Leaf))

    let find_min h = 
      match h with
        | Leaf -> raise Empty
        | Node (_, minn, _, _) -> minn
    
    let delete_min h = 
      match h with
        | Leaf -> raise Empty
        | Node (_, _, left, right) -> merge left right
    
    let from_list lst =
      let rec iter rest heap = 
        match rest with
          | [] -> heap
          | (h :: t) -> iter t (insert heap h)
      in iter lst empty
  end



module StringPQ = MakePriorityQueue(OrderedString)

let heap_sort_2 raw_lst = 
  let rec iter heap lst = 
    if StringPQ.is_empty heap 
      then List.rev lst
      else iter (StringPQ.delete_min heap) 
                (StringPQ.find_min heap :: lst)
  in iter (StringPQ.from_list raw_lst) []





(* Part D: Special Topics *)



(* D.1: Streams *)
type 'a contents = Value of 'a | Lazy of (unit -> 'a)
type 'a lazy_t = 'a contents ref

let make_lazy e = ref (Lazy e)
let force lz = 
  match !lz with
    | Value v -> v
    | Lazy func -> 
      let value = func () in
      lz := Value value;
      value

let val_test = ref (Value 5)
let lazy_test = make_lazy (fun () -> 10 * 200 * 3000)
let val_test2 = force val_test
let lazy_test2 = force lazy_test




(* D.2: The Y combinator *)

let y =
  fun f ->
    (fun z -> z (`Roll z))
    (fun (`Roll w) -> f (fun x -> w (`Roll w) x))



(* D.2.a: sum *)
let sum = 
  let almost_sum =
    fun f ->
      fun lst ->
        match lst with
          | [] -> 0
          | (h :: t) -> h + (f t)
  in y almost_sum



(* D.2.b: Two-argument functions *)
let factorial2 n = 
  let almost_iter = 
    fun f ->
      fun (n, r) ->
        if n = 0
          then r
          else f ((n - 1), (n * r))
  in (y almost_iter) (n, 1)