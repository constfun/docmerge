(* TODO: Can we cross-compile this file and use it instead of the op_set.js file in automerge to run tests? *)
(* TODO: How does automerge persist data? *)

type exn +=
  | Inconsistent_reuse_of_sequence
  | Not_supported

module ActorMap = CCMap.Make(CCString)
module SeqMap = CCMap.Make(CCInt)
module ObjectIdMap = CCMap.Make(CCString)
module ObjectIdSet = CCSet.Make(CCString)

module OpSet = struct
  type actor = string (* GUID *)
  type seq = int
  type obj_id = string

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
    obj: obj_id;
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

  type edit_action =
    | Create

  type edit_type =
    | Map
    | Text
    | List

  type edit = {
    _type: edit_type;
    action: edit_action;
    obj: obj_id;
  }

  type obj = {
    _init: op;
    _inbound: unit;
    _elem_ids: int list option;
  }

  type t = {
    actor: actor;
    seq: seq;
    deps: seq ActorMap.t;
    (* All observed actor clocks. *)
    (* As you receieve new ops, the corresponding actor clock is updated. *)
    clock: seq ActorMap.t;
    queue: change CCFQueue.t;
    (* List of states for every actor for every seq *)
    states: state list ActorMap.t;
    history: change list;
    by_object: obj ObjectIdMap.t;
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

  let apply_make t (op : op) =
    let (edit, obj) =
      match op.action with
      | MakeMap ->
        let e = {
          action = Create;
          _type = Map;
          obj = op.obj;
        } in
        let o = {
          _init = op;
          _inbound = ();
          _elem_ids = None;
        } in
        (e, o)
      | MakeText ->
        let e = {
          action = Create;
          _type = Text;
          obj =  op.obj;
        } in
        let o = {
          _init = op;
          _inbound = ();
          _elem_ids = Some [];
        } in
        (e, o)
      | MakeList ->
        let e = {
          action = Create;
          _type = List;
          obj = op.obj;
        } in
        let o = {
          _init = op;
          _inbound = ();
          _elem_ids = Some [];
        } in
        (e, o)
      | _ -> raise Not_supported
    in
    let by_object = ObjectIdMap.add op.obj obj t.by_object in
    ({t with by_object}, [edit])

  let apply_insert t op =
    (t, [])

  let apply_assign t op mem =
    (t, [])

  let apply_ops t ops =
    let (t, all_diffs, _) = List.fold_left (fun (t, all_diffs, new_objs) (op : op) ->
        match op.action with
        | MakeMap | MakeList | MakeText ->
          let new_objs = ObjectIdSet.add op.obj new_objs in
          let (t, diffs) = apply_make t op in
          (t, List.append all_diffs diffs, new_objs)
        | Ins ->
          let (t, diffs) = apply_insert t op in
          (t, List.append all_diffs diffs, new_objs)
        | Set | Del | Link ->
          let (t, diffs) = apply_assign t op (ObjectIdSet.mem op.obj) in
          (t, List.append all_diffs diffs, new_objs)
      ) (t, [], ObjectIdSet.empty) ops
    in
    (t, all_diffs)

  let apply_change t (change: change) =
    (* Prior state by sequence *)
    let prior = ActorMap.get_or ~default:[] change.actor t.states in
    if change.seq <= List.length prior then (
      match List.nth_opt prior (change.seq - 1) with
      | Some state when state.change == change -> raise Inconsistent_reuse_of_sequence
      | _ -> (t, [])
    ) else
      let allDeps = ActorMap.add change.actor (change.seq - 1) change.deps
      |> transitive_deps t in
      let new_prior = List.append prior [{change; allDeps}] in
      let t = {t with states = ActorMap.add change.actor new_prior t.states} in
      (* NOTE: The original code sets actor and sequence equal to change actor and seq, for each op.
               We choose to keep the actor and seq attached to every op in the data type. *)
      let (t, diffs) = apply_ops t change.ops in
      let remaining_deps =
        ActorMap.filter (fun depActor depSeq ->
          depSeq > (ActorMap.get_or depActor ~default:0 allDeps)
        ) t.deps
        |> ActorMap.add change.actor change.seq
      in
      let clock = ActorMap.add change.actor change.seq t.clock in
      let history = List.append t.history [change] in
      ({t with deps = remaining_deps; clock; history}, diffs)

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

