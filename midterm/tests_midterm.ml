open OUnit2
open Midterm

(* 
 * Utilities. 
 *)

let assert_false msg x = assert_bool msg (not x)
let assert_true msg x = assert_bool msg x
let assert_not_equal msg x y = assert_bool msg (not (x = y))

let assert_raises_failure msg x =
  assert_bool msg
    (try
       begin
          ignore (x ());
          false
       end
     with
       | Failure _ -> true
       | _ -> false)

let assert_raises_invalid_arg msg x =
  assert_bool msg
    (try
       begin
          ignore (x ());
          false
       end
     with
       | Invalid_argument _ -> true
       | _ -> false)

let string_of_int_list lst =
  "[" ^ String.concat "; " (List.map string_of_int lst) ^ "]"

let tree_of_list lst =
  List.fold_left (fun l i -> insert i l) Leaf lst

let level = function
  | Leaf -> 0
  | Node (lvl, _, _, _) -> lvl

let value = function
  | Leaf -> -1  (* sentinel value; we assume nodes only have positive values *)
  | Node (_, v, _, _) -> v

let rec is_aa_tree tree = 
  let levels_ok lvl ll lr parent_lvl =
    lvl = ll + 1 && (lvl = lr + 1 || (lvl = lr && lvl + 1 = parent_lvl))
  in
  let values_ok v lv rv =
    match (lv, rv) with
      | (-1, -1) -> true
      | (-1, r)  -> v < r
      | (l, -1)  -> v > l
      | (l, r)   -> v > l && v < r
  in
  let rec aux t parent_lvl =
    match t with
      | Leaf -> true
      | Node (lvl, _, _, _) when lvl < 1 -> false
      | Node (lvl, v, l, r) ->
        let ll = level l in
        let lr = level r in
        let lv = value l in
        let rv = value r in
        let ok_levels = levels_ok lvl ll lr parent_lvl in
        let ok_values = values_ok v lv rv in
          ok_levels && ok_values && aux l lvl && aux r lvl
  in
    aux tree (level tree + 1)


(*
 * Some sample trees. Some are not balanced and have the "_u" suffix. 
 *)

let t0 = Leaf

let t1_u = Node (1, 50, Node (1, 40, Leaf, Leaf), Leaf)
let t1   = Node (1, 40, Leaf, Node (1, 50, Leaf, Leaf))

let t2_u  = Node (1, 40, Node (1, 30, Leaf, Leaf), Node (1, 50, Leaf, Leaf))
let t2_u2 = Node (1, 30, Leaf, Node (1, 40, Leaf, Node (1, 50, Leaf, Leaf)))
let t2    = Node (2, 40, Node (1, 30, Leaf, Leaf), Node (1, 50, Leaf, Leaf))

let t3_u = 
  Node (2, 40,
   Node (1, 30, Leaf, Node (1, 33, Leaf, Node (1, 36, Leaf, Leaf))),
   Node (1, 50, Leaf, Leaf))

let t3_left_u = Node (1, 30, Leaf, Node (1, 33, Leaf, Node (1, 36, Leaf, Leaf)))
let t3_left   = Node (2, 33, Node (1, 30, Leaf, Leaf), Node (1, 36, Leaf, Leaf))

(* Inserting the numbers 50, 40, 30, 33, 36, 60, 70, 65, 62, 64 
 * in sequence to an empty AA tree. *)

let tt0 = Leaf
let tt1 = Node (1, 50, Leaf, Leaf)
let tt2 = Node (1, 40, Leaf, Node (1, 50, Leaf, Leaf))
let tt3 = Node (2, 40, Node (1, 30, Leaf, Leaf), Node (1, 50, Leaf, Leaf))
let tt4 = 
  Node (2, 40, Node (1, 30, Leaf, Node (1, 33, Leaf, Leaf)),
   Node (1, 50, Leaf, Leaf))
let tt5 = 
  Node (2, 33, Node (1, 30, Leaf, Leaf),
   Node (2, 40, Node (1, 36, Leaf, Leaf), Node (1, 50, Leaf, Leaf)))
let tt6 =
  Node (2, 33, Node (1, 30, Leaf, Leaf),
   Node (2, 40, Node (1, 36, Leaf, Leaf),
    Node (1, 50, Leaf, Node (1, 60, Leaf, Leaf))))
let tt7 =
  Node (3, 40,
   Node (2, 33, Node (1, 30, Leaf, Leaf), Node (1, 36, Leaf, Leaf)),
   Node (2, 60, Node (1, 50, Leaf, Leaf), Node (1, 70, Leaf, Leaf)))
let tt8 =
  Node (3, 40,
   Node (2, 33, Node (1, 30, Leaf, Leaf), Node (1, 36, Leaf, Leaf)),
   Node (2, 60, Node (1, 50, Leaf, Leaf),
    Node (1, 65, Leaf, Node (1, 70, Leaf, Leaf))))
let tt9 =
  Node (3, 40,
   Node (2, 33, Node (1, 30, Leaf, Leaf), Node (1, 36, Leaf, Leaf)),
   Node (2, 60, Node (1, 50, Leaf, Leaf),
    Node (2, 65, Node (1, 62, Leaf, Leaf), Node (1, 70, Leaf, Leaf))))
let tt10 =
  Node (3, 40,
   Node (2, 33, Node (1, 30, Leaf, Leaf), Node (1, 36, Leaf, Leaf)),
   Node (2, 60, Node (1, 50, Leaf, Leaf),
    Node (2, 65, Node (1, 62, Leaf, Node (1, 64, Leaf, Leaf)),
     Node (1, 70, Leaf, Leaf))))

(* 
 * Randomly-generated lists.
 *)

let _ = Random.self_init ()

(* Random list of length n of integers between 1 and m. *)
let random_list_mn m n =
  let next () = Random.int m + 1 in
  let arr = Array.init n (fun _ -> next ()) in
    Array.to_list arr

let random_list () =
  let n = Random.int 20 in
  let m = Random.int 100 + 1 in
    random_list_mn m n

(*
 * Test if a list is sorted.
 *)

let rec is_sorted lst =
  match lst with
    | []
    | [_] -> true
    | h1 :: (h2 :: t2 as t) ->
        if h1 > h2 then
          false
        else
          is_sorted t

(*
 * Code supplied to students.
 *)

let rec block_sort lst =
  match block_sort1 lst with
    | [] -> []
    | h :: t -> h :: block_sort t

(*
 * The tests.
 *)

let all_tests = "all" >:::
[ 

  (* ---------------------------------------------------------------------- 
   * Part B.
   * ---------------------------------------------------------------------- *)

  "Problem B.1.a: split3" >:: (fun _ ->
    assert_equal ~msg:"split3 1" (split3 []) ([], [], []);
    assert_equal ~msg:"split3 2" (split3 [1]) ([1], [], []);
    assert_equal ~msg:"split3 3" (split3 [2;1]) ([2], [1], []);
    assert_equal ~msg:"split3 4" (split3 [2;1;3]) ([2], [1], [3]);
    assert_equal ~msg:"split3 5" (split3 [2;1;3;10]) ([2;10], [1], [3]);
    assert_equal ~msg:"split3 6" (split3 [2;1;3;10;3]) ([2;10], [1;3], [3]);
    assert_equal ~msg:"split3 7" (split3 [2;1;3;10;3;4]) ([2;10], [1;3], [3;4]);
  );

  "Problem B.1.b: merge3" >:: (fun _ ->
    assert_equal ~msg:"merge3 1" (merge3 [] [] []) [];
    assert_equal ~msg:"merge3 2" (merge3 [1] [] []) [1];
    assert_equal ~msg:"merge3 3" (merge3 [] [2] []) [2];
    assert_equal ~msg:"merge3 4" (merge3 [] [] [3]) [3];
    assert_equal ~msg:"merge3 5" (merge3 [1] [2] []) [1;2];
    assert_equal ~msg:"merge3 6" (merge3 [] [2] [3]) [2;3];
    assert_equal ~msg:"merge3 7" (merge3 [1] [] [3]) [1;3];
    assert_equal ~msg:"merge3 8" (merge3 [1] [2] [3]) [1;2;3];
    assert_equal ~msg:"merge3 9" (merge3 [1] [3] [2]) [1;2;3];
    assert_equal ~msg:"merge3 10" (merge3 [2] [1] [3]) [1;2;3];
    assert_equal ~msg:"merge3 11" (merge3 [2] [3] [1]) [1;2;3];
    assert_equal ~msg:"merge3 12" (merge3 [3] [1] [2]) [1;2;3];
    assert_equal ~msg:"merge3 13" (merge3 [3] [2] [1]) [1;2;3];
    assert_equal ~msg:"merge3 14" (merge3 [1;4] [2;5] [3;6;7]) [1;2;3;4;5;6;7];
    assert_equal ~msg:"merge3 15" (merge3 [2;5] [1;4] [3;6;7]) [1;2;3;4;5;6;7];
    assert_equal ~msg:"merge3 16" (merge3 [2;5] [3;6;7] [1;4]) [1;2;3;4;5;6;7];
    assert_equal ~msg:"merge3 16" (merge3 [2;5] [3;5;7] [1;4]) [1;2;3;4;5;5;7];
    assert_equal ~msg:"merge3 17" (merge3 [1;2;4] [] [3;5;6;7]) [1;2;3;4;5;6;7];
  );

  "Problem B.1.c: merge_sort3" >:: (fun _ ->
    assert_equal ~msg:"merge_sort3 1" (merge_sort3 []) [];
    assert_equal ~msg:"merge_sort3 2" (merge_sort3 [1]) [1];
    assert_equal ~msg:"merge_sort3 3" 
      (merge_sort3 [1;2;3;1]) [1;1;2;3];

    for i = 1 to 100 do
      let lst = random_list () in
      let slst = merge_sort3 lst in
        if not (is_sorted slst) then
          begin
            Printf.printf 
              "\n\nmerge_sort3 fails on %s\n%!"
              (string_of_int_list lst);
            assert_bool "merge_sort3 random" false
          end
    done;
  );

  "Problem B.2.a: smallest_index" >:: (fun _ ->
    assert_raises_invalid_arg "smallest_index 1" (fun () -> smallest_index []);
    assert_equal ~msg:"smallest_index 2" (smallest_index [1]) 0;
    assert_equal ~msg:"smallest_index 3" (smallest_index [2;1]) 1;
    assert_equal ~msg:"smallest_index 4" (smallest_index [2;1;3]) 1;
    assert_equal ~msg:"smallest_index 5" (smallest_index [2;1;1]) 1;
    assert_equal ~msg:"smallest_index 6" (smallest_index [2;1;3;1;4;5;0]) 6;
  );

  "Problem B.2.b: flip_n" >:: (fun _ ->
    assert_equal ~msg:"flip_n 1" (flip_n 0 []) [];
    assert_equal ~msg:"flip_n 2" (flip_n 0 [1;2;3;4;5]) [1;2;3;4;5];
    assert_raises_invalid_arg "flip_n 3" (fun () -> flip_n 1 []);
    assert_equal ~msg:"flip_n 4" (flip_n 1 [1;2;3;4;5]) [1;2;3;4;5];
    assert_equal ~msg:"flip_n 5" (flip_n 2 [1;2;3;4;5]) [2;1;3;4;5];
    assert_equal ~msg:"flip_n 6" (flip_n 3 [1;2;3;4;5]) [3;2;1;4;5];
    assert_equal ~msg:"flip_n 7" (flip_n 4 [1;2;3;4;5]) [4;3;2;1;5];
    assert_equal ~msg:"flip_n 8" (flip_n 5 [1;2;3;4;5]) [5;4;3;2;1];
    assert_raises_invalid_arg "flip_n 9" (fun () -> flip_n 6 [1;2;3;4;5]);
  );

  "Problem B.2.c: block_sort1" >:: (fun _ ->
    assert_equal ~msg:"block_sort1 1" (block_sort1 []) [];
    assert_equal ~msg:"block_sort1 2" (block_sort1 [1]) [1];
    assert_equal ~msg:"block_sort1 3" (block_sort1 [1;1;1]) [1;1;1];
    assert_equal ~msg:"block_sort1 4" (block_sort1 [1;2;3]) [1;2;3];
    assert_equal ~msg:"block_sort1 5" (block_sort1 [3;2;1]) [1;2;3];
    assert_equal ~msg:"block_sort1 6" 
      (block_sort1 [5;4;3;2;1;2;3;4;5])
      [1;2;3;4;5;2;3;4;5];
    assert_equal ~msg:"block_sort1 7" 
      (block_sort1 [2;2;3;3;1;1;4;4;5;5])
      [1;3;3;2;2;1;4;4;5;5];
  );

  "Problem B.2.d: block_sort_r" >:: (fun _ ->
    assert_equal ~msg:"block_sort_r 1" (block_sort_r []) [];
    assert_equal ~msg:"block_sort_r 2" (block_sort_r [1]) [1];
    assert_equal ~msg:"block_sort_r 3" (block_sort_r [1;1]) [1;1];
    assert_equal ~msg:"block_sort_r 4" (block_sort_r [1;2]) [1;2];
    assert_equal ~msg:"block_sort_r 5" (block_sort_r [2;1]) [1;2];
    assert_equal ~msg:"block_sort_r 6" (block_sort_r [5;4;3;2;1]) [1;2;3;4;5];
    assert_equal ~msg:"block_sort_r 7" 
      (block_sort_r [5;4;3;2;1;2;3;4;5]) [1;2;2;3;3;4;4;5;5];

    for i = 1 to 100 do
      let lst = random_list () in
      let slst1 = List.sort compare lst in
      let slst2 = block_sort_r lst in
        if slst1 <> slst2  then
          begin
            Printf.printf 
              "\n\nList.sort and block_sort_r give different results on %s\n%!"
              (string_of_int_list lst);
            assert_bool "block_sort_r random 1" false
          end
    done;

    for i = 1 to 100 do
      let lst = random_list () in
      let slst1 = block_sort lst in
      let slst2 = block_sort_r lst in
        if slst1 <> slst2  then
          begin
            Printf.printf 
              "\n\nblock_sort and block_sort_r give different results on %s\n%!"
              (string_of_int_list lst);
            assert_bool "block_sort_r random 2" false
          end
    done
  );

  "Problem B.2.d: block_sort_i" >:: (fun _ ->
    assert_equal ~msg:"block_sort_i 1" (block_sort_i []) [];
    assert_equal ~msg:"block_sort_i 2" (block_sort_i [1]) [1];
    assert_equal ~msg:"block_sort_i 3" (block_sort_i [1;1]) [1;1];
    assert_equal ~msg:"block_sort_i 4" (block_sort_i [1;2]) [1;2];
    assert_equal ~msg:"block_sort_i 5" (block_sort_i [2;1]) [1;2];
    assert_equal ~msg:"block_sort_i 6" (block_sort_i [5;4;3;2;1]) [1;2;3;4;5];
    assert_equal ~msg:"block_sort_i 7" 
      (block_sort_i [5;4;3;2;1;2;3;4;5]) [1;2;2;3;3;4;4;5;5];

    for i = 1 to 100 do
      let lst = random_list () in
      let slst1 = List.sort compare lst in
      let slst2 = block_sort_i lst in
        if slst1 <> slst2  then
          begin
            Printf.printf 
              "\n\nList.sort and block_sort_i give different results on %s\n%!"
              (string_of_int_list lst);
            assert_bool "block_sort_i random 1" false
          end
    done;

    for i = 1 to 100 do
      let lst = random_list () in
      let slst1 = block_sort lst in
      let slst2 = block_sort_i lst in
        if slst1 <> slst2  then
          begin
            Printf.printf 
              "\n\nblock_sort and block_sort_i give different results on %s\n%!"
              (string_of_int_list lst);
            assert_bool "block_sort_i random 2" false
          end
    done
  );

  (* NOTE: `linrec` itself is not tested directly. *)

  "Problem B.3.b: insert_r" >:: (fun _ ->
    assert_equal ~msg:"insert_r 1" (insert_r 1 []) [1];
    assert_equal ~msg:"insert_r 2" (insert_r 1 [2;3;4;5]) [1;2;3;4;5];
    assert_equal ~msg:"insert_r 3" (insert_r 3 [1;2;4;5]) [1;2;3;4;5];
    assert_equal ~msg:"insert_r 4" (insert_r 3 [1;2;3;4;5]) [1;2;3;3;4;5];

    for i = 1 to 100 do
      let lst = random_list () in
      let slst = List.sort compare lst in  (* known sorted list *)
      let m = Random.int 100 + 1 in
      let slst2 = insert_r m slst in
        if not (is_sorted slst2) then
          begin
            Printf.printf 
              "\n\ninsert_r fails on %d inserted into %s\n%!"
              m (string_of_int_list slst);
            assert_bool "linrec insert_r random" false
          end
    done;
  );

  "Problem B.3.c: insertion sort" >:: (fun _ ->
    assert_equal ~msg:"insertion sort 1" (insertion_sort []) [];
    assert_equal ~msg:"insertion sort 2" (insertion_sort [1]) [1];
    assert_equal ~msg:"insertion sort 3" 
      (insertion_sort [1;2;3;1]) [1;1;2;3];

    for i = 1 to 100 do
      let lst = random_list () in
      let slst = insertion_sort lst in
        if not (is_sorted slst) then
          begin
            Printf.printf 
              "\n\ninsertion sort fails on %s\n%!"
              (string_of_int_list lst);
            assert_bool "insertion sort random" false
          end
    done;
  );

  (* NOTE: `binrec` itself is not tested directly. *)

  "Problem B.4.b: quicksort" >:: (fun _ ->
    assert_equal ~msg:"quicksort 1" (quicksort []) [];
    assert_equal ~msg:"quicksort 2" (quicksort [1]) [1];
    assert_equal ~msg:"quicksort 3" (quicksort [1;2;3;1]) [1;1;2;3];

    for i = 1 to 100 do
      let lst = random_list () in
      let slst = quicksort lst in
        if not (is_sorted slst) then
          begin
            Printf.printf 
              "\n\nquicksort fails on %s\n%!"
              (string_of_int_list lst);
            assert_bool "quicksort random" false
          end
    done;
  );

  (* NOTE: `tailrec` itself is not tested directly. *)

  "Problem B.5.b: insert_i" >:: (fun _ ->
    assert_equal ~msg:"insert_i 1" (insert_i 1 []) [1];
    assert_equal ~msg:"insert_i 2" (insert_i 1 [2;3;4;5]) [1;2;3;4;5];
    assert_equal ~msg:"insert_i 3" (insert_i 3 [1;2;4;5]) [1;2;3;4;5];
    assert_equal ~msg:"insert_i 4" (insert_i 3 [1;2;3;4;5]) [1;2;3;3;4;5];

    for i = 1 to 100 do
      let lst = random_list () in
      let slst = List.sort compare lst in  (* known sorted list *)
      let m = Random.int 100 + 1 in
      let slst2 = insert_i m slst in
        if not (is_sorted slst2) then
          begin
            Printf.printf 
              "\n\ninsert_i fails on %d inserted into %s\n%!"
              m (string_of_int_list slst);
            assert_bool "insert_i random" false
          end
    done;
  );

  "Problem B.5.c: insertion_sort_i" >:: (fun _ ->
    assert_equal ~msg:"insertion sort 1" (insertion_sort_i []) [];
    assert_equal ~msg:"insertion sort 2" (insertion_sort_i [1]) [1];
    assert_equal ~msg:"insertion sort 3" 
      (insertion_sort_i [1;2;3;1]) [1;1;2;3];

    for i = 1 to 100 do
      let lst = random_list () in
      let slst = insertion_sort_i lst in
        if not (is_sorted slst) then
          begin
            Printf.printf 
              "\n\ninsertion sort fails on %s\n%!"
              (string_of_int_list lst);
            assert_bool "insertion sort random" false
          end
    done;
  );

  (* ---------------------------------------------------------------------- 
   * Part C.
   * ---------------------------------------------------------------------- *)

  "Problem C.1: member" >:: (fun _ ->
    assert_false "member 0" (member 0 t0);

    assert_true  "member 1a" (member 50 t1_u);
    assert_true  "member 1b" (member 40 t1_u);
    assert_false "member 1c" (member  0 t1_u);

    assert_true  "member 1d" (member 50 t1);
    assert_true  "member 1e" (member 40 t1);
    assert_false "member 1f" (member  0 t1);

    assert_true  "member 2a" (member 50 t2_u);
    assert_true  "member 2b" (member 40 t2_u);
    assert_true  "member 2c" (member 30 t2_u);
    assert_false "member 2d" (member 100 t2_u);

    assert_true  "member 2e" (member 50 t2_u2);
    assert_true  "member 2f" (member 40 t2_u2);
    assert_true  "member 2g" (member 30 t2_u2);
    assert_false "member 2h" (member 100 t2_u2);

    assert_true  "member 2i" (member 50 t2);
    assert_true  "member 2j" (member 40 t2);
    assert_true  "member 2k" (member 30 t2);
    assert_false "member 2l" (member 100 t2);

    assert_true  "member 3a" (member 40 t3_u);
    assert_true  "member 3b" (member 30 t3_u);
    assert_true  "member 3c" (member 33 t3_u);
    assert_true  "member 3d" (member 36 t3_u);
    assert_true  "member 3e" (member 50 t3_u);
    assert_false "member 3f" (member 70 t3_u);

    assert_true  "member 3g" (member 30 t3_left_u);
    assert_true  "member 3h" (member 33 t3_left_u);
    assert_true  "member 3i" (member 36 t3_left_u);
    assert_false "member 3j" (member 70 t3_left_u);

    assert_true  "member 3k" (member 30 t3_left);
    assert_true  "member 3l" (member 33 t3_left);
    assert_true  "member 3m" (member 36 t3_left);
    assert_false "member 3n" (member 70 t3_left);

  );

  "Problem C.2: skew" >:: (fun _ -> 
    assert_equal ~msg:"skew 0" t0 (skew t0);

    assert_equal ~msg:"skew 1" t1 (skew t1_u);

    assert_equal ~msg:"skew 2a" t2_u2 (skew t2_u);
    assert_equal ~msg:"skew 2b" t2_u2 (skew t2_u2);
    assert_equal ~msg:"skew 2c" t2 (skew t2);

    assert_equal ~msg:"skew 3a" t3_u (skew t3_u);
    assert_equal ~msg:"skew 3b" t3_left_u (skew t3_left_u);
    assert_equal ~msg:"skew 3c" t3_left (skew t3_left);
  );

  "Problem C.2: split" >:: (fun _ -> 
    assert_equal ~msg:"split 0" t0 (split t0);

    assert_equal ~msg:"split 1a" t1_u (split t1_u);
    assert_equal ~msg:"split 1b" t1 (split t1);

    assert_equal ~msg:"split 2a" t2_u (split t2_u);
    assert_equal ~msg:"split 2b" t2 (split t2_u2);
    assert_equal ~msg:"split 2c" t2 (split t2);

    assert_equal ~msg:"split 3a" t3_u (split t3_u);
    assert_equal ~msg:"split 3b" t3_left (split t3_left_u);
    assert_equal ~msg:"split 3c" t3_left (split t3_left);
  );

  "Problem C.3: insert" >:: (fun _ -> 
    assert_equal ~msg:"insert 1"  tt1  (insert 50 tt0);
    assert_equal ~msg:"insert 1a" tt1  (insert 50 tt1);

    assert_equal ~msg:"insert 2"  tt2  (insert 40 tt1);
    assert_not_equal "insert 2a"  tt2  (insert 30 tt1);

    assert_equal ~msg:"insert 3"  tt3  (insert 30 tt2);

    assert_equal ~msg:"insert 4"  tt4  (insert 33 tt3);

    assert_equal ~msg:"insert 5"  tt5  (insert 36 tt4);

    assert_equal ~msg:"insert 6"  tt6  (insert 60 tt5);

    assert_equal ~msg:"insert 7"  tt7  (insert 70 tt6);

    assert_equal ~msg:"insert 8"  tt8  (insert 65 tt7);

    assert_equal ~msg:"insert 9"  tt9  (insert 62 tt8);

    assert_equal ~msg:"insert 10" tt10 (insert 64 tt9);

    for i = 1 to 100 do
      let lst = random_list () in
      let t = tree_of_list lst in
        if not (is_aa_tree t) then
          begin
            Printf.printf "\n\n(tree_of_list %s) generates invalid AA tree.\n\n%!"
              (string_of_int_list lst);
            assert_bool "insert random" false
          end
    done
  );

]

let _ = run_test_tt_main all_tests

