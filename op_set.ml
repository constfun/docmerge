(* TODO: Can we cross-compile this file and use it instead of the op_set.js file in automerge to run tests? *)
(* TODO: How does automerge persist data? *)

type exn +=
  | Inconsistent_reuse_of_sequence

module ActorMap = CCMap.Make(CCString)
module SeqMap = CCMap.Make(CCInt)

module OpSet = struct
  type actor = string (* GUID *)
  type seq = int

  type action =
    | MakeMap
    | MakeList
    | MakeText
    | Ins
    | Set
    | Del
    | Link

  type op = {
    action: action;
    actor: actor;
    seq: seq;
  }

  type change = {
    actor: actor;
    seq: seq;
    (* List of depended op sequences by actor. *)
    deps: seq ActorMap.t;
    ops: op list;
  }

  type state = {
    change: change;
    allDeps: seq ActorMap.t;
  }


  type t = {
    actor: actor;
    seq: seq;
    (* All observed actor clocks. *)
    (* As you receieve new ops, the corresponding actor clock is updated. *)
    clock: seq ActorMap.t;
    queue: change CCFQueue.t;
    (* List of states for every actor for every seq *)
    states: state list ActorMap.t
  }

  (* Returns true if all changes that causally precede the given change *)
  (* have already been applied in `opSet`. *)
  (* All changes are causally (and totally) ordered using lamport timestamps *)
  (* When a new op lands in the op set, check if all preceeding ops have been applied *)
  (* If we store ops in Irmin, causality is enforced by history, aka the Merkle DAG. *)
  (* TODO: rename change to op? *)
  let causaly_ready t (change : change) =
    change.deps
    |> ActorMap.update change.actor (function
        | Some seq -> Some (seq - 1)
        | None -> None)
    |> ActorMap.for_all (fun depActor depSeq ->
        match ActorMap.find_opt depActor t.clock with
        | Some depClock -> depClock >= depSeq
        | None -> depSeq >= 0)

  (*
     All change ops + allDeps of every actor state at current seq?
     Something like that... ie
     For every actor op that a change depends on
     get all deps of the actors current state?
  *)
  let transitive_deps t baseDeps =
    ActorMap.fold (fun depActor depSeq deps ->
        if depSeq <= 0 then deps else
        match ActorMap.find_opt depActor t.states with
        | Some states -> (
          match List.nth_opt states (depSeq - 1) with
          | Some state -> (
            ActorMap.merge (fun _ l r ->
              match (l, r) with
              | (Some l, Some r) -> Some (max l r)
              | (Some l, None) -> Some l
              | (None, Some r) -> Some r
              | (None, None) -> None
            ) deps state.allDeps
            |> ActorMap.update depActor (fun _ -> Some depSeq)
          )
          | None -> deps
        )
        | None -> deps
      ) baseDeps (ActorMap.empty)

  let apply_change t (change: change) =
    (* Prior state by sequence *)
    let prior = match ActorMap.find_opt change.actor t.states with
      | Some s -> s
      | None -> []
    in
    if change.seq <= List.length prior then (
      match List.nth_opt prior (change.seq - 1) with
      | Some state when state.change == change -> raise Inconsistent_reuse_of_sequence
      | _ -> (t, [])
    ) else
      let allDeps = ActorMap.add change.actor (change.seq - 1) change.deps
      |> transitive_deps t in
      let new_prior = List.append prior [{change; allDeps}] in
      let t = ActorMap.add change.actor new_prior t.states in


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
  let rec apply_queued_ops diffs t =
    let (new_t, diffs) = CCFQueue.fold (fun (t, diffs) change ->
        if causaly_ready t change then
          let (t, diff) = apply_change t change in
          (t, diff :: diffs)
        else
          ({t with queue = (CCFQueue.cons change t.queue)}, diffs)
      ) (t, diffs) t.queue
    in
    if (CCFQueue.size new_t.queue) == (CCFQueue.size t.queue) then
      (new_t, diffs)
    else
      apply_queued_ops diffs new_t


  (* TODO: Maintain local undo history *)
  let add_change t change (* isUndoable *) =
    {t with queue = (CCFQueue.cons change t.queue)}
    |> apply_queued_ops []
end

