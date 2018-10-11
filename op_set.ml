open Base

(* TODO: Can we cross-compile this file, use is instead of the op_set.js file in automerge and run tests? *)
(* TODO: How does automerge persist data? *)
(* TODO: Somewhat ironically, OCaml data structures are mutable, while automerge uses Immutable.js
         It might be easier to source some immutable data structures for the initial translation. *)


module OpSet = struct
  type time (* Lamport timestamp *)
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
    states: int list;
    history: int list;
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
    ()

  let apply_queued_ops t =
    let {clock; queue} = t in
    match Queue.dequeue queue with
    | Some change ->
      if causaly_ready clock change then
        apply_change t change
      else ()
    | None -> ()

  (* TODO: Should return new opset + diff *)
  let add_change t change (* isUndoable *) =
    Queue.enqueue t.queue change;
    (* Maintain local undo history *)
    apply_queued_ops t
end

