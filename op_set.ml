(* TODO: Can we cross-compile this file and use it instead of the op_set.js file in automerge to run tests? *)
(* TODO: How does automerge persist data? *)

type exn +=
  | Inconsistent_reuse_of_sequence
  | Not_supported
  | Modification_of_unknown_object
  | Duplicate_list_element_id

module ActorMap = CCMap.Make (CCString)
module SeqMap = CCMap.Make (CCInt)
module ObjectIdMap = CCMap.Make (CCString)
module ObjectIdSet = CCSet.Make (CCString)
module ElemIdMap = CCMap.Make (CCString)
module KeyMap = CCMap.Make (CCString)

module OpSet = struct
  type actor = string

  (* GUID *)
  type seq = int

  type obj_id = string

  type elem_id = string

  type key = string

  type action = MakeMap | MakeList | MakeText | Ins | Set | Del | Link

  type op =
    { key: key
    ; action: action
    ; actor: actor
    ; seq: seq
    ; obj: obj_id
    ; elem: int
    ; value: string option }

  module OpSet = CCSet.Make (struct
    type t = op

    let compare op1 op2 =
      (* TODO: compare lamport clocks *)
      0
  end)

  type change =
    { actor: actor
    ; seq: seq
    ; (* List of depended op sequences by actor. *)
      deps: seq ActorMap.t
    ; ops: op list }

  type state = {change: change; allDeps: seq ActorMap.t}

  type edit_action = Create

  type edit_type = Map | Text | List

  type edit = {_type: edit_type; action: edit_action; obj: obj_id}

  type ref = {action: action; obj: obj_id; key: key; value: unit option}

  (* TODO: Needs to be actual map. *)
  type obj_aux =
    { _max_elem: int
    ; _following: op list KeyMap.t
    ; _init: op
    ; _inbound: OpSet.t
    ; _elem_ids: int list option
    ; _insertion: op ElemIdMap.t }

  type obj = op list KeyMap.t * obj_aux

  type t =
    { actor: actor
    ; seq: seq
    ; undo_local: ref list option
    ; undo_pos: int
    ; undo_stack: ref list list
    ; redo_stack: ref list list
    ; deps: seq ActorMap.t
    ; (* All observed actor clocks. *)
      (* As you receieve new ops, the corresponding actor clock is updated. *)
      clock: seq ActorMap.t
    ; queue: change CCFQueue.t
    ; (* List of states for every actor for every seq *)
      states: state list ActorMap.t
    ; history: change list
    ; by_object: obj ObjectIdMap.t }

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
         | None -> None )
    |> ActorMap.for_all (fun depActor depSeq ->
           match ActorMap.find_opt depActor t.clock with
           | Some depClock -> depClock >= depSeq
           | None -> depSeq >= 0 )

  (*
     All change ops + allDeps of every actor state at current seq?
     Something like that... ie
     For every actor op that a change depends on
     get all deps of the actors current state?
  *)
  let transitive_deps t baseDeps =
    ActorMap.fold
      (fun depActor depSeq deps ->
        if depSeq <= 0 then deps
        else
          match ActorMap.find_opt depActor t.states with
          | Some states -> (
            match List.nth_opt states (depSeq - 1) with
            | Some state ->
                ActorMap.merge
                  (fun _ l r ->
                    match (l, r) with
                    | Some l, Some r -> Some (max l r)
                    | Some l, None -> Some l
                    | None, Some r -> Some r
                    | None, None -> None )
                  deps state.allDeps
                |> ActorMap.update depActor (fun _ -> Some depSeq)
            | None -> deps )
          | None -> deps )
      baseDeps ActorMap.empty

  let apply_make t (op : op) =
    let edit, obj_aux =
      match op.action with
      | MakeMap ->
          let e = {action= Create; _type= Map; obj= op.obj} in
          let o =
            { _max_elem= 0
            ; _following= KeyMap.empty
            ; _insertion= ElemIdMap.empty
            ; _init= op
            ; _inbound= OpSet.empty
            ; _elem_ids= None }
          in
          (e, o)
      | MakeText ->
          let e = {action= Create; _type= Text; obj= op.obj} in
          let o =
            { _max_elem= 0
            ; _following= KeyMap.empty
            ; _insertion= ElemIdMap.empty
            ; _init= op
            ; _inbound= OpSet.empty
            ; _elem_ids= Some [] }
          in
          (e, o)
      | MakeList ->
          let e = {action= Create; _type= List; obj= op.obj} in
          let o =
            { _max_elem= 0
            ; _following= KeyMap.empty
            ; _insertion= ElemIdMap.empty
            ; _init= op
            ; _inbound= OpSet.empty
            ; _elem_ids= Some [] }
          in
          (e, o)
      | _ -> raise Not_supported
    in
    let by_object =
      ObjectIdMap.add op.obj (KeyMap.empty, obj_aux) t.by_object
    in
    ({t with by_object}, [edit])

  let apply_insert t (op : op) =
    let elem_id = op.actor ^ ":" ^ CCInt.to_string op.elem in
    ( match ObjectIdMap.find_opt op.obj t.by_object with
    | Some (_, obj_aux) ->
        if ElemIdMap.mem elem_id obj_aux._insertion then
          raise Duplicate_list_element_id
        else ()
    | None -> raise Modification_of_unknown_object ) ;
    let by_object =
      ObjectIdMap.update op.obj
        (function
          | Some (obj, obj_aux) ->
              let _following =
                KeyMap.update op.key
                  (function
                    | Some l -> Some (List.append l [op]) | None -> Some [])
                  obj_aux._following
              in
              let _max_elem = max op.elem obj_aux._max_elem in
              let _insertion = ElemIdMap.add elem_id op obj_aux._insertion in
              Some (obj, {obj_aux with _following; _max_elem; _insertion})
          | None -> None)
        t.by_object
    in
    let t = {t with by_object} in
    (t, [])

  (* Returns true if the two operations are concurrent, that is, they happened without being aware of
  each other (neither happened before the other). Returns false if one supersedes the other. *)
  let is_concurrent t (op1 : op) (op2 : op) =
    let actor1, seq1 = (op1.actor, op1.seq) in
    let actor2, seq2 = (op2.actor, op2.seq) in
    let clock1 =
      (CCList.nth (ActorMap.find actor1 t.states) (seq1 - 1)).allDeps
    in
    let clock2 =
      (CCList.nth (ActorMap.find actor2 t.states) (seq2 - 1)).allDeps
    in
    ActorMap.get_or actor2 ~default:0 clock1 < seq2
    && ActorMap.get_or actor1 ~default:0 clock2 < seq1

  (* Processes a 'set', 'del', or 'link' operation *)
  let apply_assign t (op : op) is_top_level =
    if not (ObjectIdMap.mem op.obj t.by_object) then
      raise Modification_of_unknown_object
    else
      let t =
        match t.undo_local with
        | Some uloc when is_top_level ->
            let obj_map, obj_aux = ObjectIdMap.find op.obj t.by_object in
            let undo_ops =
              KeyMap.get_or op.key ~default:[] obj_map
              |> CCList.map (fun (op : op) ->
                     { action= op.action
                     ; obj= op.obj
                     ; key= op.key
                     ; value= op.value } )
            in
            let undo_ops =
              if CCList.is_empty undo_ops then
                [{action= Del; obj= op.obj; key= op.key; value= None}]
              else undo_ops
            in
            let uloc = CCList.concat [uloc; undo_ops] in
            {t with undo_local= Some uloc}
        | _ -> t
      in
      let overwritten, remaining =
        let obj_map, obj_aux = ObjectIdMap.find op.obj t.by_object in
        let refs = KeyMap.get_or op.key ~default:[] obj_map in
        CCList.fold_left
          (fun (over, rem) ref ->
            if is_concurrent t ref op then (over, ref :: rem)
            else (ref :: over, rem) )
          ([], []) refs
      in
      (* If any links were overwritten, remove them from the index of inbound links *)
      let overwritten_links = CCList.filter (fun (op: op) -> match op.action with Link -> true | _ -> false) overwritten in
      let t =
        CCList.fold_left (fun t (op : op) ->
            let by_object =
              ObjectIdMap.update
                (CCOpt.get_exn op.value)
                (function
                  | Some (obj_map, obj_aux) ->
                    Some (obj_map, {obj_aux with _inbound= OpSet.remove op obj_aux._inbound})
                  | None -> raise Not_found
                )
                t.by_object
            in
            {t with by_object}
          ) t overwritten_links
      in
      (t, [])

  let apply_ops t ops =
    let t, all_diffs, _ =
      List.fold_left
        (fun (t, all_diffs, new_objs) (op : op) ->
          match op.action with
          | MakeMap | MakeList | MakeText ->
              let new_objs = ObjectIdSet.add op.obj new_objs in
              let t, diffs = apply_make t op in
              (t, List.append all_diffs diffs, new_objs)
          | Ins ->
              let t, diffs = apply_insert t op in
              (t, List.append all_diffs diffs, new_objs)
          | Set | Del | Link ->
              let t, diffs =
                apply_assign t op (ObjectIdSet.mem op.obj new_objs)
              in
              (t, List.append all_diffs diffs, new_objs) )
        (t, [], ObjectIdSet.empty) ops
    in
    (t, all_diffs)

  let apply_change t (change : change) =
    (* Prior state by sequence *)
    let prior = ActorMap.get_or ~default:[] change.actor t.states in
    if change.seq <= List.length prior then
      match List.nth_opt prior (change.seq - 1) with
      | Some state when state.change == change ->
          raise Inconsistent_reuse_of_sequence
      | _ -> (t, [])
    else
      let allDeps =
        ActorMap.add change.actor (change.seq - 1) change.deps
        |> transitive_deps t
      in
      let new_prior = List.append prior [{change; allDeps}] in
      let t = {t with states= ActorMap.add change.actor new_prior t.states} in
      (* NOTE: The original code sets actor and sequence equal to change actor and seq, for each op.
               We choose to keep the actor and seq attached to every op in the data type. *)
      let t, diffs = apply_ops t change.ops in
      let remaining_deps =
        ActorMap.filter
          (fun depActor depSeq ->
            depSeq > ActorMap.get_or depActor ~default:0 allDeps )
          t.deps
        |> ActorMap.add change.actor change.seq
      in
      let clock = ActorMap.add change.actor change.seq t.clock in
      let history = List.append t.history [change] in
      ({t with deps= remaining_deps; clock; history}, diffs)

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
    let new_t, diffs =
      CCFQueue.fold
        (fun (t, diffs) change ->
          if causaly_ready t change then
            let t, diff = apply_change t change in
            (t, diff :: diffs)
          else ({t with queue= CCFQueue.cons change t.queue}, diffs) )
        (t, diffs) t.queue
    in
    if CCFQueue.size new_t.queue == CCFQueue.size t.queue then (new_t, diffs)
    else apply_queued_ops new_t diffs

  let push_undo_history t =
    let undo_stack =
      CCList.append
        (CCList.take t.undo_pos t.undo_stack)
        [CCOpt.get_exn t.undo_local]
    in
    { t with
      undo_stack; undo_pos= t.undo_pos + 1; redo_stack= []; undo_local= None }

  let add_change t change isUndoable =
    let t = {t with queue= CCFQueue.cons change t.queue} in
    if isUndoable then
      let t = {t with undo_local= Some []} in
      let d, diffs = apply_queued_ops t [] in
      let t = push_undo_history t in
      (t, diffs)
    else apply_queued_ops t []
end
