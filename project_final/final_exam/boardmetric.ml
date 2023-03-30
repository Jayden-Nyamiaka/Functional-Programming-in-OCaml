open Boardrep

(* Helper function to get the next location for traversing the board 
 *    Returns (-1, -1) if there's no next location (last pos on board) *)
let next_loc (y, x) size =
  if x = size - 1 then
    if y = size - 1 then
      ((-1), (-1)) 
    else (y + 1, 0)
  else (y, x + 1)

module type BoardMetric =
  sig
    type t

    val distance : t -> t -> int
  end

module Hamming(B : BoardRep) : BoardMetric with type t = B.t =
  struct
    type t = B.t

    let distance b1 b2 =
      let size = B.get_size b2 in
      if B.get_size b1 <> size
        then failwith "incompatible board sizes"
        else
          let rec iter lctn dif =
            if fst lctn = (-1) then dif else
            let val2 = B.get b2 lctn in
            if val2 <> 0 && val2 <> B.get b1 lctn 
              then iter (next_loc lctn size) (dif + 1) 
              else iter (next_loc lctn size) dif
          in iter (0, 0) 0
  end

module Manhattan(B : BoardRep) : BoardMetric with type t = B.t =
  struct
    type t = B.t

    let distance b1 b2 =
      let size = B.get_size b2 in
      if B.get_size b1 <> size
        then failwith "incompatible board sizes"
        else
          let capacity = size * size in
          let arr1 = Array.make capacity (0,0) 
          and arr2 = Array.make capacity (0,0) in
          let rec store_locs lctn =
            if fst lctn = (-1) then () 
            else
              begin
                arr1.(B.get b1 lctn) <- lctn;
                arr2.(B.get b2 lctn) <- lctn;
                store_locs (next_loc lctn size)
              end
          in store_locs (0, 0);
          let rec sum_difs i sum_dif =
            if i = capacity then sum_dif else
            let curr_dif =
              match (arr1.(i), arr2.(i)) with
                | ((x1, y1), (x2, y2)) -> abs (x1 - x2) + abs (y1 - y2)
            in sum_difs (i + 1) (sum_dif + curr_dif)
          in sum_difs 1 0 
  end
