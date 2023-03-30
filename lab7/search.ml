(* search.ml: search strategies *)
(* Student name:                *)
(* CMS cluster login name:      *)

module type Storage =
  sig
    type 'a t
    exception Empty

    val create : unit -> 'a t
    val push : 'a -> 'a t -> unit
    val pop : 'a t -> 'a
    val is_empty : 'a t -> bool
  end

module type Domain =
  sig
    type t
    val show : t -> string
    val is_solved : t -> bool
    val compare : t -> t -> int
    val next : t -> t list
  end

module Search (S : Storage) (D : Domain) =
  struct
    module DS = Set.Make(D)

    (* Storage.t: data structure to store histories- Stack or Queue *)
    (* Domain.t:  data structure used as the board- Klotski *)
    (* DS.t:      data structure to ensure no repeat boards- KlotskiSet *)
    
    let search init = 
      let histories = S.create () in
      S.push [init] histories;
      let rec visit_solve visited_boards =
        if S.is_empty histories then raise Not_found else
        let history = S.pop histories in
        let current_board = List.hd history in
        if DS.mem current_board visited_boards
          then visit_solve visited_boards
          else 
            if D.is_solved current_board 
              then history
              else
                let push_child child = S.push (child :: history) histories
                in List.iter push_child (D.next current_board);
                visit_solve (DS.add current_board visited_boards)
      in visit_solve DS.empty

    let show_history hist =
      (String.concat "\n----\n\n" (List.map D.show (List.rev hist))) ^ "\n"
  end

