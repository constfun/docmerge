(* TODO: Can we cross-compile this file and use it instead of the op_set.js file in automerge to run tests? *)
(* TODO: How does automerge persist data? *)

type exn +=
  | Inconsistent_reuse_of_sequence
  | Not_supported
  | Modification_of_unknown_object
  | Duplicate_list_element_id
  | Unknown_action_type
  | Missing_index_for_list_element

module ActorMap = CCMap.Make (CCString)
module SeqMap = CCMap.Make (CCInt)
module ObjectIdMap = CCMap.Make (CCString)
module ObjectIdSet = CCSet.Make (CCString)
module ElemIdMap = CCMap.Make (CCString)
module KeyMap = CCMap.Make (CCString)

module OpSetBackend = struct
  let root_id = "00000000-0000-0000-0000-000000000000"

  type actor = string

  (* GUID *)
  type seq = int

  type obj_id = string

  type key = string

  type action = MakeMap | MakeList | MakeText | Ins | Set | Del | Link

  type value = Value of string | Link of {obj: value}

  type elem_id = key * value option

  (* Ineficient but simple implementation of skip list from original *)
  module SkipList = struct
    type t = elem_id list

    let empty = []

    let insert_index index k v (t : t) = CCList.insert_at_idx index (k, v) t

    let index_of k t =
      match CCList.find_idx (fun (itmk, _) -> itmk == k) t with
      | Some (idx, _) -> Some idx
      | None -> None

    let set_value k v (t : t) =
      match index_of k t with
      | Some idx -> CCList.set_at_idx idx (k, v) t
      | None -> raise Not_found

    let remove_index index (t : t) = CCList.remove_at_idx index t
  end

  type op =
    { key: key
    ; action: action
    ; actor: actor
    ; seq: seq
    ; obj: obj_id
    ; elem: int
    ; value: string option }

  type lamport_op = {actor: actor; elem: int}

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

  type edit_action = Create | Insert | Remove | Set

  type edit_type = Map | Text | List

  type conflict = {actor: actor; value: string option; link: bool}

  type edit =
    { _type: edit_type
    ; action: edit_action
    ; elem_id__key: key option
    ; key: string option
    ; value: value option
    ; obj: obj_id
    ; link: bool
    ; index: int option
    ; conflicts: conflict list option
    ; path: [`IntPath of int | `StrPath of key] list option }

  type ref = {action: action; obj: obj_id; key: key; value: value option}

  type obj_aux =
    { _max_elem: int
    ; _following: op list KeyMap.t
    ; _init: op
    ; _inbound: OpSet.t
    ; _elem_ids: SkipList.t option
    ; _insertion: op ElemIdMap.t }

  type obj = op list KeyMap.t * obj_aux

  type t =
    { states:
        state list ActorMap.t
        (* List of states for every actor for every seq *)
    ; history: change list
    ; by_object: obj ObjectIdMap.t
    ; clock:
        seq ActorMap.t
        (* All observed actor clocks. *)
        (* As you receieve new ops, the corresponding actor clock is updated. *)
    ; deps: seq ActorMap.t
    ; undo_pos: int
    ; undo_stack: ref list list
    ; redo_stack: ref list list
    ; queue: change CCFQueue.t
    ; undo_local: ref list option }

  (* Helpers not found in original *)
  let get_obj_aux t obj_id = CCOpt.map snd (ObjectIdMap.get obj_id t.by_object)

  let get_obj_aux_exn t obj_id = CCOpt.get_exn (get_obj_aux t obj_id)

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
          let e =
            { action= Create
            ; _type= Map
            ; obj= op.obj
            ; key= None
            ; index= None
            ; path= None
            ; conflicts= None
            ; link= false
            ; elem_id__key= None
            ; value= None }
          in
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
          let e =
            { action= Create
            ; _type= Text
            ; obj= op.obj
            ; index= None
            ; conflicts= None
            ; path= None
            ; link= false
            ; key= None
            ; elem_id__key= None
            ; value= None }
          in
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
          let e =
            { action= Create
            ; key= None
            ; _type= List
            ; conflicts= None
            ; obj= op.obj
            ; index= None
            ; path= None
            ; link= false
            ; elem_id__key= None
            ; value= None }
          in
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

  let get_conflicts (ops : op list) =
    match ops with
    | _ :: ops_rest ->
        CCList.fold_left
          (fun conflicts (op : op) ->
            let link = match op.action with Link -> true | _ -> false in
            let conf : conflict = {actor= op.actor; value= op.value; link} in
            CCOpt.map (fun cs -> cs @ [conf]) conflicts )
          (Some []) ops_rest
    | [] -> None

  (* Returns the path from the root object to the given objectId, as an array of string keys
     (for ancestor maps) and integer indexes (for ancestor lists). If there are several paths
     to the same object, returns one of the paths arbitrarily. If the object is not reachable
     from the root, returns null. *)
  let rec get_path t obj_id path =
    if obj_id == root_id then path
    else
      match ObjectIdMap.get obj_id t.by_object with
      | None -> None
      | Some (_, obj_aux) -> (
        match OpSet.choose_opt obj_aux._inbound with
        | None -> None
        | Some ref -> (
          match ObjectIdMap.get ref.obj t.by_object with
          | None -> None
          | Some (_, obj_aux) -> (
            match obj_aux._init.action with
            | MakeList | MakeText -> (
                let elem_ids = CCOpt.get_exn obj_aux._elem_ids in
                match SkipList.index_of ref.key elem_ids with
                | None -> None
                | Some index ->
                    get_path t ref.obj
                      (CCOpt.map (fun p -> `IntPath index :: p) path) )
            | _ ->
                get_path t ref.obj
                  (CCOpt.map (fun p -> `StrPath ref.key :: p) path) ) ) )

  let patch_list (t : t) obj_id index (elem_id__key : key)
      (action : edit_action) (ops : op list option) =
    let _type =
      let _, obj_aux = ObjectIdMap.get obj_id t.by_object |> CCOpt.get_exn in
      match obj_aux._init.action with MakeText -> Text | _ -> List
    in
    let first_op = CCOpt.flat_map (fun ops -> CCList.nth_opt ops 0) ops in
    let elem_ids = CCOpt.get_exn (get_obj_aux_exn t obj_id)._elem_ids in
    let value = CCOpt.flat_map (fun (fop : op) -> fop.value) first_op in
    let value = CCOpt.map (fun v -> Value v) value in
    let path = get_path t obj_id (Some []) in
    let edit : edit =
      { action
      ; _type
      ; obj= obj_id
      ; index= Some index
      ; key= None
      ; path
      ; link= false
      ; value= None
      ; conflicts= None
      ; elem_id__key= None }
    in
    let edit, value =
      match first_op with
      | Some fop when fop.action == Link ->
          ( {edit with link= true}
          , Some (Link {obj= Value (CCOpt.get_exn fop.value)}) )
      | _ -> (edit, value)
    in
    let elem_ids, edit =
      match action with
      | Insert ->
          let elem_ids =
            SkipList.insert_index index (CCOpt.get_exn first_op).key value
              elem_ids
          in
          (elem_ids, {edit with elem_id__key= Some elem_id__key; value})
      | Set ->
          let elem_ids =
            SkipList.set_value (CCOpt.get_exn first_op).key value elem_ids
          in
          (elem_ids, {edit with value})
      | Remove -> (SkipList.remove_index index elem_ids, edit)
      | Create -> raise Unknown_action_type
    in
    let edit =
      match ops with
      | Some ops when List.length ops > 1 ->
          {edit with conflicts= get_conflicts ops}
      | _ -> edit
    in
    let by_object =
      ObjectIdMap.update obj_id
        (function
          | Some (obj_map, obj_aux) ->
              Some (obj_map, {obj_aux with _elem_ids= Some elem_ids})
          | None -> raise Not_found)
        t.by_object
    in
    ({t with by_object}, [edit])

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

  let get_field_ops t obj_id (key : key) =
    let obj_map, _ = CCOpt.get_exn (ObjectIdMap.get key t.by_object) in
    KeyMap.get_or key obj_map ~default:[]

  let get_parent t obj_id key =
    if key == "_head" then None
    else
      let open CCOpt.Infix in
      let insertion =
        ObjectIdMap.get obj_id t.by_object
        >>= fun (_, obj_aux) ->
        ElemIdMap.get key obj_aux._insertion >|= fun op -> op.key
      in
      match insertion with
      | None -> raise Missing_index_for_list_element
      | Some k -> Some k

  let lamport_compare op1 op2 =
    if op1.elem < op2.elem then -1
    else if op1.elem > op2.elem then 1
    else if op1.actor < op2.actor then -1
    else if op1.actor > op2.actor then 1
    else 0

  let insertions_after t obj_id (parent_id : key option)
      (child_id : key option) =
    let child_key =
      match child_id with
      | Some child_id ->
          (* Child id is of the format `actor:elem_digits` *)
          let regx = Str.regexp "^\\(.*\\):\\(\\d+\\)$" in
          if Str.string_match regx child_id 0 then
            let actor = Str.matched_group 1 child_id in
            let elem = int_of_string (Str.matched_group 2 child_id) in
            Some {actor; elem}
          else None
      | None -> None
    in
    let following =
      match get_obj_aux t obj_id with
      | Some obj_aux -> (
        match parent_id with
        | None -> []
        | Some parent_id ->
            KeyMap.get_or ~default:[] parent_id obj_aux._following )
      | None -> []
    in
    CCList.filter
      (fun (op : op) -> match op.action with Ins -> true | _ -> false)
      following
    |> CCList.filter (fun (op : op) ->
           match child_key with
           | None -> true
           | Some child_key ->
               lamport_compare {actor= op.actor; elem= op.elem} child_key < 0
       )
    |> CCList.sort (fun (op1 : op) (op2 : op) ->
           lamport_compare
             {actor= op1.actor; elem= op1.elem}
             {actor= op2.actor; elem= op2.elem} )
    |> CCList.rev
    |> CCList.map (fun (op : op) -> op.actor ^ ":" ^ string_of_int op.elem)

  (*  Given the ID of a list element, returns the ID of the immediate predecessor list element, *)
  (*  or null if the given list element is at the head. *)
  let get_previous t obj_id key =
    let parent_id = get_parent t obj_id key in
    let children = insertions_after t obj_id parent_id None in
    if CCList.hd children == key then
      match parent_id with Some "_head" -> None | _ -> parent_id
    else
      (* In the original code, there seems to be a bug here, where prev_id will still be undefined when fist child is equal to key.
         We replicate the behavior anyway to preserve the semantics. *)
      let prev_id =
        match CCList.find_idx (fun child -> child == key) children with
        | Some (idx, _) ->
            if idx == 0 then None else Some (CCList.nth children (idx - 1))
        | None -> CCList.last_opt children
      in
      let rec loop children prev_id =
        let children = insertions_after t obj_id prev_id None in
        if CCList.is_empty children then prev_id
        else loop children (CCList.last_opt children)
      in
      loop children prev_id

  let update_list_element t obj_id (elem_id__key : key) =
    let ops = get_field_ops t obj_id elem_id__key in
    let _, {_elem_ids} = ObjectIdMap.find obj_id t.by_object in
    let elem_ids = CCOpt.get_exn _elem_ids in
    let index = SkipList.index_of elem_id__key elem_ids in
    match index with
    | Some index ->
        if CCList.is_empty ops then
          patch_list t obj_id index elem_id__key Remove None
        else patch_list t obj_id index elem_id__key Set (Some ops)
    | None ->
        (* Deleting a non-existent element = no-op *)
        if CCList.is_empty ops then (t, [])
        else
          let rec loop prev_id =
            match get_previous t obj_id prev_id with
            | None -> -1
            | Some prev_id -> (
              match SkipList.index_of prev_id elem_ids with
              | Some index -> index
              | None -> loop prev_id )
          in
          (* Index can be -1 here, this feels like an error, but we keep going to preserve semantics *)
          let index = loop elem_id__key in
          patch_list t obj_id (index + 1) elem_id__key Insert (Some ops)

  let update_map_key t obj_id key =
    let ops = get_field_ops t obj_id key in
    let path = get_path t obj_id (Some []) in
    let edit =
      if CCList.is_empty ops then
        { action= Remove
        ; key= Some key
        ; _type= Map
        ; conflicts= None
        ; obj= obj_id
        ; index= None
        ; path
        ; link= false
        ; elem_id__key= None
        ; value= None }
      else
        let fst = CCList.hd ops in
        let value = CCOpt.map (fun s -> Value s) fst.value in
        let conflicts =
          if CCList.length ops > 1 then get_conflicts ops else None
        in
        { action= Set
        ; _type= Map
        ; obj= obj_id
        ; key= Some key
        ; path
        ; value
        ; link= fst.action == Link
        ; conflicts
        ; index= None
        ; elem_id__key= None }
    in
    (t, [edit])

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
                     ; value= CCOpt.map (fun v -> Value v) op.value } )
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
      let overwritten_links =
        CCList.filter
          (fun (op : op) -> match op.action with Link -> true | _ -> false)
          overwritten
      in
      let t =
        CCList.fold_left
          (fun t (op : op) ->
            let by_object =
              ObjectIdMap.update (CCOpt.get_exn op.value)
                (function
                  | Some (obj_map, obj_aux) ->
                      Some
                        ( obj_map
                        , { obj_aux with
                            _inbound= OpSet.remove op obj_aux._inbound } )
                  | None -> raise Not_found)
                t.by_object
            in
            {t with by_object} )
          t overwritten_links
      in
      let t =
        match op.action with
        | Link ->
            let by_object =
              ObjectIdMap.update (CCOpt.get_exn op.value)
                (function
                  | Some (obj_map, obj_aux) ->
                      Some
                        ( obj_map
                        , {obj_aux with _inbound= OpSet.add op obj_aux._inbound}
                        )
                  | None -> raise Not_found)
                t.by_object
            in
            {t with by_object}
        | _ -> t
      in
      let remaining =
        match op.action with
        | Del -> remaining
        | _ -> CCList.append remaining [op]
      in
      let remaining =
        CCList.sort
          (fun (op1 : op) (op2 : op) -> String.compare op1.actor op2.actor)
          remaining
        |> CCList.rev
      in
      let by_object =
        ObjectIdMap.update op.obj
          (function
            | Some (obj_map, obj_aux) ->
                Some (ObjectIdMap.add op.key remaining obj_map, obj_aux)
            | None -> raise Not_found)
          t.by_object
      in
      let t = {t with by_object} in
      let obj_type =
        (snd (ObjectIdMap.find op.obj t.by_object))._init.action
      in
      match obj_type with
      | MakeList | MakeText -> update_list_element t op.obj op.key
      | _ -> update_map_key t op.obj op.key

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

  let init () =
    { states= ActorMap.empty
    ; history= []
    ; by_object= ObjectIdMap.empty
    ; clock= ActorMap.empty
    ; deps= ActorMap.empty
    ; undo_pos= 0
    ; undo_stack= []
    ; redo_stack= []
    ; queue= CCFQueue.empty
    ; undo_local= None }
end
