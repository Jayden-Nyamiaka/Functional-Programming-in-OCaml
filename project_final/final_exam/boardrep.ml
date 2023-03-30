type loc = int * int
type move = Up | Down | Left | Right

(* Helper functions to make working with locations easier *)
let loc_to_idx (row, col) size = row * size + col
let idx_to_loc idx size = 
  let col = idx / size in
  let row = idx - col * size in
    (col, row)

(* Helper function to return the new hole position for a move 
 *    Raises excptn (Invalid_move) if the move can't be made *)
let get_new_hole dir hole size exptn = 
  let check_bound n =
    if n < 0 || n >= size
      then raise exptn
      else n
  in 
    match (dir, hole) with 
      | (Up, (y, x)) -> (check_bound(y - 1), x)
      | (Down, (y, x)) -> (check_bound(y + 1), x)
      | (Left, (y, x)) -> (y, check_bound(x - 1))
      | (Right, (y, x)) -> (y, check_bound(x + 1))

module type BoardRep =
  sig
    type t

    exception Invalid_move
    exception Invalid_location

    val init      : int -> t
    val load      : int -> int list -> t
    val get_size  : t -> int
    val get_hole  : t -> loc
    val get       : t -> loc -> int
    val make_move : t -> move -> t
    val show      : t -> unit
  end

(* ---------------------------------------------------------------------- 
 * Helper functions.
 * ---------------------------------------------------------------------- *)

(* Make a display function given board accessors. *)
let make_show get get_size b =
  let size = get_size b in
    begin
      Printf.printf "\n%!";
      for row = 0 to size - 1 do
        for col = 0 to size - 1 do
          Printf.printf "%3d" (get b (row, col))
        done;
        Printf.printf "\n";
      done;
      Printf.printf "\n%!"
    end

(* ---------------------------------------------------------------------- 
 * Modules.
 * ---------------------------------------------------------------------- *)

module OrderedLoc =
  struct
    type t = loc
    let compare = Stdlib.compare
  end

module ArrayRep : BoardRep =
  struct
    type t = 
      { 
        acontents : int array;
        size : int;
        hole : loc
      }

    exception Invalid_move
    exception Invalid_location

    let init size = 
      if size < 2 then
        failwith "ERROR: init: size must be at least 2"
      else
        { size = size; 
          hole = (size - 1, size - 1);
          acontents =
            let capacity = size * size in
            let initialize i = 
              if i = capacity - 1 
                then 0
                else i + 1 
            in Array.init capacity initialize }

    let load size lst =
      if size < 2 then
        failwith "ERROR: load: size must be at least 2"
      else
        let capacity = size * size in
        if List.length lst <> capacity
          then failwith "invalid list length"
          else
            let b = Array.make capacity (-1) in
            (* Adds all the elements in the list to the board array 
             *    Fails if an elem is duplicated or outside [0, size*size-1]
             *    Returns the loc on the board of the hole (elem = 0) *)
            let rec iter rest i hl =
              match rest with 
                | [] -> hl
                | v :: t ->
                  if v < 0 || v >= capacity || Array.mem v b
                    then failwith "invalid list contents"
                    else 
                      let new_hl = if v = 0 then idx_to_loc i size else hl
                      in b.(i) <- v; iter t (i + 1) new_hl
            in 
            { size = size; 
              hole = iter lst 0 ((-1), (-1));
              acontents = b}

    let get_size b = b.size

    let get_hole b = b.hole

    let get { acontents; size = s; _ } (r, c) = 
      if r < 0 || r > s - 1 || c < 0 || c > s - 1 then
        raise Invalid_location
      else
        try
          acontents.(r * s + c)
        with (Invalid_argument _) ->
          raise Invalid_location

    let make_move b m =      
      let new_hl = get_new_hole m b.hole b.size Invalid_move in
      let mov_idx = loc_to_idx new_hl b.size 
      and hol_idx = loc_to_idx b.hole b.size
      and new_board = Array.copy b.acontents in
        new_board.(hol_idx) <- new_board.(mov_idx);
        new_board.(mov_idx) <- 0;
       { b with hole = new_hl; acontents = new_board }

    let show = make_show get get_size
  end

module MapRep : BoardRep =
  struct
    module LocMap = Map.Make(OrderedLoc)

    type t = 
      { 
        mcontents : int LocMap.t;
        size : int;
        hole : loc
      }

    exception Invalid_move
    exception Invalid_location

    let init size =
      if size < 2 then
        failwith "ERROR: init: size must be at least 2"
      else
        { size = size; 
          hole = (size - 1, size - 1);
          mcontents =
            let capacity = size * size in
            let rec iter b n = 
              if n = capacity then b else
              let new_b = 
                let lctn = idx_to_loc n size in
                if n = capacity - 1 
                  then LocMap.add lctn 0 b
                  else LocMap.add lctn (n + 1) b
              in iter new_b (n + 1)
            in iter LocMap.empty 0 }

    let load size lst =
      if size < 2 then
        failwith "ERROR: load: size must be at least 2"
      else
        let capacity = size * size in
        if List.length lst <> capacity
          then failwith "invalid list length"
          else
            (* Returns a 2-tuple of the board map with all elements 
             * in the list added to b and the location of the hole (elem = 0)
             *  Fails if an elem is duplicated or outside [0, size*size-1] *)
            let rec iter b rest i hl =
              let lctn = idx_to_loc i size in
              match rest with 
                | [] -> (b, hl)
                | v :: t ->
                  if v < 0 || v >= capacity 
                      || LocMap.exists (fun _ n -> n = v) b
                    then failwith "invalid list contents"
                    else
                      let new_hl = if v = 0 then lctn else hl
                      in iter (LocMap.add lctn v b) t (i + 1) new_hl
            in 
            let board_hole_tup = iter LocMap.empty lst 0 ((-1), (-1)) in
            { size = size; 
              hole = snd board_hole_tup;
              mcontents = fst board_hole_tup }

    let get_size b = b.size

    let get_hole b = b.hole

    let get { mcontents; _ } l = 
      try
        LocMap.find l mcontents
      with Not_found ->
        raise Invalid_location

    let make_move b m =
      let new_hl = get_new_hole m b.hole b.size Invalid_move in
      let mov_n = LocMap.find new_hl b.mcontents in
      let new_map = LocMap.add new_hl 0 (LocMap.add b.hole mov_n b.mcontents)
      in { b with hole = new_hl; mcontents = new_map }

    let show = make_show get get_size
  end

