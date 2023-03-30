open Pqueue

module type Task =
  sig
    type t

    val compare : t -> t -> int
    val eval    : t -> t -> int
    val next    : t -> t list
    val display : t -> string
  end

module AStar (T : Task) =
  struct
    (* Raised when no more task states are left on the queue. *)
    exception No_solution

    (* Raised when a solution is encountered while searching. *)
    exception Solved of T.t list

    (* The state of a search stored in the priority queue *)
    type qstate = {
      tstate : T.t;  (* a task state *)

      (* the list of task states that preceded this one in the search *)
      history : T.t list;  

      (* the number of moves to get to this state; this is just the length
         of the history list *)
      nmoves : int;   

      (* the overall fitness: a function of the tstate evaluation and nmoves *)
      fitness : int    
    }

    (* Make the priority queue.  Compare queue states by fitness. *)
    module Tqueue = MakePriorityQueue(struct
        type t = qstate
        let compare s1 s2 = Stdlib.compare s1.fitness s2.fitness
      end)

    (* A map of the best quality scores for each evaluated tstate. *)
    module Tmap = Map.Make(T)

    (* The state of the solver as a whole. *)
    type t = {
      queue : Tqueue.t;
      best  : int Tmap.t
    }

    (* The solver function. *)
    let solve goal init =
      let init_eval  = T.eval goal init in
      let init_state =
        {
           tstate  = init;
           history = [];
           nmoves  = 0;
           fitness = init_eval;
        }
      in
      let init_queue  = Tqueue.insert (Tqueue.empty) init_state in
      let init_best   = Tmap.empty in
      let init_solver = { queue = init_queue; best = init_best } in

      (* 
         The main solving loop using the priority queue.
       
         Invariant: tstates on the queue are not solved.

         1) Pop a qstate off the queue.  If the queue is empty,
            there is no solution.  Raise a No_solution exception.

         2) Check if the tstate in the popped qstate is a key in the `best`
            map: If it isn't, add it to the `best` map as a key with the number
            of moves leading to the task state (the `nmoves` field of the queue
            state) as the value.  If it is, compare it to the move count in the
            map. If the new number of moves is smaller than the one in the map,
            replace the binding in the map.  Otherwise, this task has been
            already searched for with a smaller (or at least no larger) number
            of moves, so there is no point in searching it again, so discard it
            and restart the loop.

         3) Assuming we got this far, compute the next qstates.
            Check to see if any of them contains a tstate which is a solution;
            if so, compute the final list of tasks and raise a Solved exception
            to break out of the loop.
            Otherwise, add them back into the queue and continue.  
       *)

      let rec iter { queue; best }  =
        (* Pops the most fit state from the priority queue - 
         * If queue is empty, Empty will be caught then raise No_solution *)
        let state_w_queue = Tqueue.pop_min queue in
        let state = fst state_w_queue and new_q = snd state_w_queue in
        let board = state.tstate in
        (* Updates best map *)
        let new_best = 
          match (Tmap.find_opt board best) with
            | None -> Tmap.add board state.nmoves best
            | Some shortest ->
              if state.nmoves < shortest
                then Tmap.add board state.nmoves best
                else Tmap.empty
        in
        if new_best = Tmap.empty then iter { queue = new_q; best = best } else
        (* Executes steps to find solution from state *)
        let new_history = board :: state.history
        and next_boards = T.next board in
        let eval_check next_brd = 
          let score = T.eval goal next_brd in
          if score = 0 then 
            raise (Solved (List.rev (next_brd :: new_history)))
            else score
        in
        let rec add_next_states boards q = 
          match boards with
            | [] -> q
            | brd :: rest ->
              let new_state = 
                {
                  fitness = eval_check brd + state.nmoves + 1;
                  tstate  = brd;
                  history = new_history;
                  nmoves  = state.nmoves + 1;
                }
              in add_next_states rest (Tqueue.insert q new_state)
        in iter { best = new_best; 
                  queue = add_next_states next_boards new_q }
      in
        (* The main part of the function starts evaluation here. *)
        if init_eval = 0 then
          [init]  (* handle the case when the initial state is solved. *)
        else
          try
            iter init_solver
          with 
            | Tqueue.Empty -> raise No_solution
            | Solved tlist -> tlist
  end

