(* klotski.ml: core functionality of the Klotski game. *)
(* Student name:                *)
(* CMS cluster login name:      *)

(* ---------------------------------------------------------------------- 
 * Types.
 * ---------------------------------------------------------------------- *)

type loc = int * int
type dir = Up | Down | Left | Right
type move = char * dir * int

module LocM =
  struct
    type t = loc
    let compare = Stdlib.compare
  end

module LocSet : Set.S with type elt = loc = Set.Make(LocM)

(* Sets of LocSets.  Used locally only. *)

module LocSetM =
  struct
    type t = LocSet.t
    let compare = LocSet.compare
  end

module LocSetSet = Set.Make(LocSetM)

module CharM =
  struct
    type t = char
    let compare = Stdlib.compare
  end

module CharMap : Map.S with type key = char = Map.Make(CharM)

type piece = LocSet.t
type t = { pieces : piece CharMap.t ; unoccupied : LocSet.t }

(* ---------------------------------------------------------------------- 
 * Functions.
 * ---------------------------------------------------------------------- *)

(* Create a board from a string. *)
let read s = 
  let rec iter p u r c =
    match () with
      | _ when r = 5 -> { pieces = p; unoccupied = u }
      | _ when c = 4 -> iter p u (r + 1) 0 
      | _ -> 
        let i = r * 4 + c in
        let ch = s.[i] in
          if ch = '.'  (* unoccupied location; add to unoccupied set *)
            then iter p (LocSet.add (r, c) u) r (c + 1)
            else  (* occupied; add to appropriate piece set *)
              try
                let cs  = CharMap.find ch p in     (* old piece set *)
                let cs' = LocSet.add (r, c) cs in  (* add new location *)
                let p'  = CharMap.add ch cs' p in  (* store back into map *)
                  iter p' u r (c + 1)
              with
                Not_found ->  (* new piece; create a new piece set *)
                  let cs = LocSet.singleton (r, c) in
                  let p' = CharMap.add ch cs p in
                    iter p' u r (c + 1)
  in
    if String.length s <> 20
      then failwith "read: invalid input string length"
      else iter CharMap.empty LocSet.empty 0 0

(* Convert the board to a string representation suitable for printing. *)
let show b = 
  let string_of_char_list = function
    | [a;b;c;d] -> Printf.sprintf "%c%c%c%c" a b c d
    | _ -> failwith "invalid char list"
  in
  let char_at board loc =
    let rec iter = function
      | [] -> raise Not_found
      | (c, locs) :: t -> 
        if LocSet.mem loc locs then c else iter t
    in
    if LocSet.mem loc board.unoccupied
      then '.'
      else iter (CharMap.bindings board.pieces)
  in
  (String.concat "\n"
     (List.map (fun r ->
        string_of_char_list
          (List.map (char_at b) 
            (List.map (fun c -> (r, c)) [0; 1; 2; 3])))
        [0; 1; 2; 3; 4])) ^ "\n"

let is_solved (b : t) = 
  let is_solved_2by2 _ locs = 
    LocSet.equal locs (LocSet.of_list [(3, 1); (3, 2); (4, 1); (4, 2)])
  in CharMap.exists is_solved_2by2 b.pieces

let compare (b1 : t) (b2 : t) = 
  let b1_blocks = LocSetSet.of_list (List.map snd (CharMap.bindings b1.pieces))
  and b2_blocks = LocSetSet.of_list (List.map snd (CharMap.bindings b2.pieces))
  in LocSetSet.compare b1_blocks b2_blocks

let remove c ({ pieces = p; unoccupied = u } as b) = 
  if not (CharMap.mem c p) then b else
  let locs = CharMap.find c p in
    {pieces = CharMap.remove c p; unoccupied = LocSet.union u locs}

let add (c, p) { pieces = ps; unoccupied = u } = 
  if CharMap.mem c ps then None else
  if not (LocSet.subset p u) then None else
    Some {pieces = CharMap.add c p ps; unoccupied = LocSet.diff u p}

(* Helper function that creates an int list from lower to upper (inclusive) *)
let rec inclusive_range lower upper = 
  if (lower > upper) then [] else 
  if lower = upper
    then lower :: []
    else lower :: (inclusive_range (lower + 1) upper)

let make_move (c, d, i) ({ pieces = ps; unoccupied = u } as b) = 
  (* Gets all locations that will be occupied in defined inclusive range *)
  let get_locations_in_range starting_locations upper lower =
    let range = inclusive_range upper lower in 
    (* Gets all locations that will be passed through in the defined
     * inclusive range for a single location that the block used to occupy *)
    let get_passing_locs (y, x) = 
      let loc_dir_mag num = 
        match d with
        | Up -> (y - num, x)
        | Down -> (y + num, x)
        | Left -> (y, x - num)
        | Right -> (y, x + num)
      in LocSet.of_list (List.map loc_dir_mag range)
    in List.fold_left LocSet.union LocSet.empty
        (List.map get_passing_locs (LocSet.elements starting_locations))
  in
  (* Obvious cases where Move can't be made *)
  if i < 1 || not (CharMap.mem c ps) then None else
  (* Gets the current locations of the moving piece and removes it *)
  let old_locs = CharMap.find c ps and temp_b = remove c b in
  (* Gets the intermediate and final locations of the piece *)
  let intermediate_locs = get_locations_in_range old_locs 1 (i - 1) 
  and final_locs = get_locations_in_range old_locs i i in
  (* Move can't be made unless all the intermediate and 
   * final locations are unoccupied *)
  if not (LocSet.subset (LocSet.union intermediate_locs final_locs) 
        temp_b.unoccupied) 
    then None 
    else add (c, final_locs) temp_b

let next b =
  let func_per_piece (label, locs) = 
    let func_per_dir d = 
      let func_per_mag magnitude = 
        make_move (label, d, magnitude) b 
      in
      if d = Up || d = Down 
        then List.filter_map func_per_mag (inclusive_range 1 4)
        else List.filter_map func_per_mag (inclusive_range 1 3)
    in List.concat_map func_per_dir [Up; Down; Left; Right]
  in List.concat_map func_per_piece (CharMap.bindings b.pieces)

(* Function to interactively test the "next" function. 
 * Useful for debugging. *)
let test_next b =
  let bs = next b in
    begin
      print_string (show b ^ "\n");
      List.iter 
        (fun b -> print_string ("----\n\n" ^ show b ^ "\n"))
        bs
    end

