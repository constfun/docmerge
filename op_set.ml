open Base

(* TODO: Can we cross-compile this file and use it instead of the op_set.js file in automerge to run tests? *)
(* TODO: How does automerge persist data? *)
(* TODO: Somewhat ironically, OCaml data structures are mutable, while automerge uses Immutable.js
         It might be easier to source some immutable data structures for the initial translation. *)
(* TODO: How ju juse Base properly? *)


module OpSet = struct
  type actor = string (* GUID *)
  type seq = int
  type change = {
    actor: actor;
    seq: seq;
    (* List of depended op sequences by actor. *)
    deps: seq Map.M(String).t;
  }

  type t = {
    actor: actor;
    seq: seq;
    (* All observed actor clocks. *)
    (* As you receieve new ops, the corresponding actor clock is updated. *)
    clock: seq Map.M(String).t;
    queue: change Queue.t;
  }

  (* Returns true if all changes that causally precede the given change *)
  (* have already been applied in `opSet`. *)
  (* All changes are causally (and totally) ordered using lamport timestamps *)
  (* When a new op lands in the op set, check if all preceeding ops have been applied *)
  (* If we store ops in Irmin, causality is enforced by history, aka the Merkle DAG. *)
  (* TODO: rename change to op? *)
  let causaly_ready clock change =
    Map.for_alli change.deps (fun ~key:depActor ~data:depSeq ->
        match Map.find clock depActor with
        | Some depClock ->
          if String.equal depActor change.actor then depClock >= change.seq - 1
          else depClock >= depSeq
        | None -> Int.equal depSeq 1
      )

  let apply_change t change =
    (t, 42 (* is a diff *))

  (* Simon says...

    do drain op/change queue
      if change ready
        apply and accumulate diffs
      else
        put change into new queue
    stop when new queue size == starting queue size
      (ie. no ops were ready to apply)
    otherwise recurse to retry ops that weren't ready

  *)
  let rec apply_queued_ops t diffs =
    let (new_t, diffs) = Queue.fold t.queue
        ~init:({t with queue = (Queue.of_list [])}, [])
        ~f:(fun (t, diffs) change ->
            if causaly_ready t.clock change then
              let (t, diff) = apply_change t change in
              (t, diff :: diffs)
            else (
              Queue.enqueue t.queue change;
              (t, diffs)
            )
          ) in
    if phys_equal (Queue.length new_t.queue) (Queue.length t.queue) then
      (new_t, diffs)
    else
      apply_queued_ops new_t diffs

  (* TODO: Maintain local undo history *)
  let add_change t change (* isUndoable *) =
    Queue.enqueue t.queue change;
    apply_queued_ops t []
end

