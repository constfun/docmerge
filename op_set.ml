open Sexplib.Conv
open Datastructures

(* TODO: Can we cross-compile this file and use it instead of the op_set.js file in automerge to run tests? *)
(* TODO: How does automerge persist data? *)

(* let ( $ ) f g x = f (g x) *)

type exn +=
  | Inconsistent_reuse_of_sequence
  | Not_supported
  | Modification_of_unknown_object
  | Duplicate_list_element_id
  | Unknown_object_type
  | Unknown_action_type
  | Missing_index_for_list_element
  | Accessing_unefined_element_index

let log msg conv sexp =
  Format.printf "DEBUG: %s %a\n%!" msg Sexplib.Sexp.pp_hum (conv sexp)

module ActorMap = CCMapMake (CCString)
module SeqMap = CCMapMake (CCInt)
module ObjectIdMap = CCMapMake (CCString)
module ObjectIdSet = CCSet.Make (CCString)
module ElemIdMap = CCMapMake (CCString)
module KeyMap = CCMapMake (CCString)
module KeySet = CCSet.Make (CCString)
module OpMap = CCMapMake (CCString)
module DiffMap = CCMapMake (CCString)
module ChildMap = CCMapMake (CCString)

module OpSetBackend = struct
  let root_id = "00000000-0000-0000-0000-000000000000"

  type actor = string [@@deriving sexp_of]

  (* GUID *)
  type seq = int [@@deriving sexp_of]

  type obj_id = string [@@deriving sexp_of]

  type key = string [@@deriving sexp_of]

  type action = MakeMap | MakeList | MakeText | Ins | Set | Del | Link
  [@@deriving sexp]

  type op_val = BoolValue of bool | StrValue of string | NumberValue of float
  [@@deriving sexp_of]

  type materialized = TypedValue of op_val | LinkValue of {obj_id: string}

  type value = Value of op_val | Link of {obj: value} [@@deriving sexp_of]

  type elem_id = key * value option [@@deriving sexp_of]

  (* Ineficient but simple implementation of skip list from original *)
  module SkipList = struct
    type t = elem_id list

    let empty = []

    let insert_index index k v (t : t) = CCList.insert_at_idx index (k, v) t

    let index_of k t =
      match CCList.find_idx (fun (itmk, _) -> String.equal itmk k) t with
      | Some (idx, _) -> Some idx
      | None -> None

    let key_of idx t =
      match CCList.nth_opt t idx with
      | Some (key, _) -> Some key
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
    ; elem: int option
    ; value: op_val option }
  [@@deriving sexp_of]

  type change_op =
    { key: key option
    ; action: action
    ; obj: obj_id
    ; elem: int option
    ; value: op_val option }
  [@@deriving sexp_of]

  type lamport_op = {actor: actor; elem: int}

  let lamport_compare op1 op2 =
    if op1.elem < op2.elem then -1
    else if op1.elem > op2.elem then 1
    else if op1.actor < op2.actor then -1
    else if op1.actor > op2.actor then 1
    else 0

  (* The original uses a single object for all ops, and discriminates op type based on op.action.
     This is unsafe since the invariant that, for example, op.elem must be defined for Ins operations is implicit in the API and not actually enforced.
     We at least make the elem field optional, to encode its potentially undefined nature.
     We use the get_op_elem function to access the elem field and catch the invariant violation at runtime.
  *)
  let get_op_elem (op : op) =
    match op.elem with
    | Some idx -> idx
    | None -> raise Accessing_unefined_element_index

  module OpSet = struct
    include CCSetMake (struct
      type t = op

      (* TODO: Is this the right compare fun? Should we be comparing seq instead of elem? *)
      let compare (op1 : op) (op2 : op) =
        let lop1 = {actor= op1.actor; elem= get_op_elem op1} in
        let lop2 = {actor= op1.actor; elem= get_op_elem op2} in
        lamport_compare lop1 lop2

      let sexp_of_t = sexp_of_op
    end)
  end

  type change =
    { actor: actor
    ; seq: seq
    ; (* List of depended op sequences by actor. *)
      deps: seq ActorMap.t
    ; ops: change_op list }
  [@@deriving sexp_of]

  type state = {change: change; allDeps: seq ActorMap.t} [@@deriving sexp_of]

  type edit_action = Create | Insert | Remove | Set

  type edit_type = Map | Text | List

  type conflict = {actor: actor; value: op_val option; link: bool}
  [@@deriving sexp_of]

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
    ; _elem_ids: SkipList.t option sexp_opaque
    ; _insertion: op ElemIdMap.t }
  [@@deriving sexp_of]

  type obj = op list KeyMap.t * obj_aux [@@deriving sexp_of]

  type diff_type = DiffMap | DiffList | DiffText [@@deriving sexp_of]

  type diff_action = DiffCreate | DiffSet [@@deriving sexp_of]

  type diff =
    { obj: string
    ; type_: diff_type
    ; action: diff_action
    ; key: key option
    ; value: op_val option
    ; link: bool
    ; conflicts: conflict list option }
  [@@deriving sexp_of]

  type child = string [@@deriving sexp_of]

  type context = diff list DiffMap.t * child list ChildMap.t
  [@@deriving sexp_of]

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
    ; undo_stack: ref list list sexp_opaque
    ; redo_stack: ref list list sexp_opaque
    ; queue: change CCFQueueWithSexp.t
    ; undo_local: ref list option sexp_opaque }
  [@@deriving sexp_of]

  (* Helpers not found in original *)
  let get_obj_aux t obj_id = CCOpt.map snd (ObjectIdMap.get obj_id t.by_object)

  let get_obj_aux_exn t obj_id = CCOpt.get_exn (get_obj_aux t obj_id)

  let get_op_value_as_string_exn = function
    | StrValue s -> s
    | BoolValue _ | NumberValue _ -> raise (Invalid_argument "op.value")

  let get_obj_action t obj_id = (get_obj_aux_exn t obj_id)._init.action

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
    let elem_id = op.actor ^ ":" ^ CCInt.to_string (get_op_elem op) in
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
              let _max_elem = max (get_op_elem op) obj_aux._max_elem in
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
    if String.equal obj_id root_id then path
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
      | Some fop when fop.action = Link ->
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
          print_endline "GET" ;
          log "SET" sexp_of_op (CCOpt.get_exn first_op) ;
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
    match ObjectIdMap.get obj_id t.by_object with
    | Some (obj_map, _) -> KeyMap.get_or key obj_map ~default:[]
    | None -> []

  let get_parent t obj_id (key : key option) =
    match key with
    | None -> None
    | Some key when String.equal key "_head" -> None
    | Some key -> (
        let open CCOpt.Infix in
        let insertion =
          ObjectIdMap.get obj_id t.by_object
          >>= fun (_, obj_aux) ->
          ElemIdMap.get key obj_aux._insertion >|= fun op -> op.key
        in
        match insertion with
        | None -> raise Missing_index_for_list_element
        | Some k -> Some k )

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
               lamport_compare
                 {actor= op.actor; elem= get_op_elem op}
                 child_key
               < 0 )
    |> CCList.sort (fun (op1 : op) (op2 : op) ->
           lamport_compare
             {actor= op1.actor; elem= get_op_elem op1}
             {actor= op2.actor; elem= get_op_elem op2} )
    |> CCList.rev
    |> CCList.map (fun (op : op) ->
           op.actor ^ ":" ^ string_of_int (get_op_elem op) )

  (*  Given the ID of a list element, returns the ID of the immediate predecessor list element, *)
  (*  or null if the given list element is at the head. *)
  let get_previous t obj_id key =
    let parent_id = get_parent t obj_id (Some key) in
    let children = insertions_after t obj_id parent_id None in
    log "CHILDREN" (sexp_of_list sexp_of_string) children ;
    if CCList.length children > 0 && String.equal (CCList.hd children) key then
      match parent_id with Some "_head" -> None | _ -> parent_id
    else
      (* In the original code, there seems to be a bug here, where prev_id will still be undefined when fist child is equal to key.
         We replicate the behavior anyway to preserve the semantics. *)
      let prev_id =
        match
          CCList.find_idx (fun child -> String.equal child key) children
        with
        | Some (idx, _) ->
            if idx = 0 then None else Some (CCList.nth children (idx - 1))
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
        ; link= fst.action = Link
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
        let obj_map, _ = ObjectIdMap.find op.obj t.by_object in
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
              ObjectIdMap.update
                (get_op_value_as_string_exn (CCOpt.get_exn op.value))
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
              ObjectIdMap.update
                (get_op_value_as_string_exn (CCOpt.get_exn op.value))
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
                apply_assign t op (not (ObjectIdSet.mem op.obj new_objs))
              in
              (t, List.append all_diffs diffs, new_objs) )
        (t, [], ObjectIdSet.empty) ops
    in
    (t, all_diffs)

  let apply_change t (change : change) =
    (* Prior state by sequence *)
    (* log "apply_change" (CCFQueueWithSexp.sexp_of_t sexp_of_change) t.queue; *)
    let prior = ActorMap.get_or ~default:[] change.actor t.states in
    if change.seq <= List.length prior then
      match List.nth_opt prior (change.seq - 1) with
      | Some state when state.change = change ->
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
      let ops =
        CCList.map
          (fun (ch_op : change_op) ->
            { actor= change.actor
            ; seq= change.seq
            ; action= ch_op.action (* TODO: Op key should be an option. *)
            ; key= (match ch_op.key with Some k -> k | None -> "")
            ; obj= ch_op.obj
            ; elem= ch_op.elem
            ; value= ch_op.value } )
          change.ops
      in
      let t, diffs = apply_ops t ops in
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
    let t, diffs, queue =
      CCFQueue.fold
        (fun (t, diffs, queue) change ->
          if causaly_ready t change then (
            let t, diff = apply_change t change in
            log "causally ready"
              (CCFQueueWithSexp.sexp_of_t sexp_of_change)
              t.queue ;
            (t, CCList.concat [diffs; diff], queue) )
          else (
            log "not causally ready" sexp_of_change change ;
            (t, diffs, CCFQueue.snoc t.queue change) ) )
        (t, diffs, CCFQueueWithSexp.empty)
        t.queue
    in
    if CCInt.equal (CCFQueue.size queue) (CCFQueue.size t.queue) then (t, diffs)
    else apply_queued_ops {t with queue} diffs

  let push_undo_history t =
    let undo_stack =
      CCList.append
        (CCList.take t.undo_pos t.undo_stack)
        [CCOpt.get_exn t.undo_local]
    in
    { t with
      undo_stack; undo_pos= t.undo_pos + 1; redo_stack= []; undo_local= None }

  let add_change t change isUndoable =
    let t = {t with queue= CCFQueue.snoc t.queue change} in
    if isUndoable then
      let t = {t with undo_local= Some []} in
      let t, diffs = apply_queued_ops t [] in
      let t = push_undo_history t in
      (t, diffs)
    else apply_queued_ops t []

  let init () =
    let root_op =
      { key= ""
      ; action= Set
      ; actor= ""
      ; seq= 0
      ; obj= ""
      ; elem= None
      ; value= None }
    in
    let root_obj =
      ( KeyMap.empty
      , { _max_elem= 0
        ; _following= KeyMap.empty
        ; _init= root_op
        ; _inbound= OpSet.empty
        ; _elem_ids= None
        ; _insertion= ElemIdMap.empty } )
    in
    { states= ActorMap.empty
    ; history= []
    ; by_object= ObjectIdMap.add root_id root_obj ObjectIdMap.empty
    ; clock= ActorMap.empty
    ; deps= ActorMap.empty
    ; undo_pos= 0
    ; undo_stack= []
    ; redo_stack= []
    ; queue= CCFQueue.empty
    ; undo_local= None }

  (* The following form the public API *)

  let get_missing_changes t have_deps =
    let all_deps = transitive_deps t have_deps in
    ActorMap.mapi
      (fun actor states ->
        CCList.drop (ActorMap.get_or ~default:0 actor all_deps) states )
      t.states
    |> ActorMap.values |> CCList.of_seq |> CCList.flatten
    |> CCList.map (fun state -> state.change)

  let get_changes_for_actor t ?(after_seq = 0) for_actor =
    ActorMap.filter (fun actor states -> String.equal actor for_actor) t.states
    |> ActorMap.map (fun states -> CCList.drop after_seq states)
    |> ActorMap.values |> CCList.of_seq |> CCList.flatten
    |> CCList.map (fun state -> state.change)

  let get_missing_deps t =
    CCFQueue.fold
      (fun missing (change : change) ->
        let deps = ActorMap.add change.actor (change.seq - 1) change.deps in
        ActorMap.fold
          (fun depActor depSeq missing ->
            if ActorMap.get_or depActor t.clock ~default:0 < depSeq then
              let curr = ActorMap.get_or depActor missing ~default:0 in
              ActorMap.add depActor (max depSeq curr) missing
            else missing )
          missing deps )
      ActorMap.empty t.queue

  (* I dont think this function is needed, since, unlike the original which uses underscore fields to keep auxulary information, we use a separate `obj_aux` record. *)
  let valid_field_name key = key != "" && Str.first_chars key 1 != "_"

  let is_field_present t obj_id key =
    valid_field_name key && not (CCList.is_empty (get_field_ops t obj_id key))

  let unpack_value parent_id patch_diff children value =
    match value with
    | LinkValue l ->
        let patch_diff =
          {patch_diff with link= true; value= Some (StrValue l.obj_id)}
        in
        let children =
          ChildMap.update parent_id
            (function
              | Some childs -> Some (CCList.append childs [l.obj_id])
              | None -> raise (Invalid_argument "child id"))
            children
        in
        (patch_diff, children)
    | TypedValue v -> ({patch_diff with value= Some v}, children)

  let unpack_conflict_value parent_id (conflict : conflict) children value =
    match value with
    | LinkValue l ->
        let patch_diff =
          {conflict with link= true; value= Some (StrValue l.obj_id)}
        in
        let children =
          ChildMap.update parent_id
            (function
              | Some childs -> Some (CCList.append childs [l.obj_id])
              | None -> raise (Invalid_argument "child id"))
            children
        in
        (patch_diff, children)
    | TypedValue v -> ({conflict with value= Some v}, children)

  let unpack_conflicts_for_key key parent_id (patch_diff : diff) children
      conflicts =
    match conflicts with
    | Some all_conf -> (
      match KeyMap.get key all_conf with
      | Some cs ->
          let conflicts, children =
            OpMap.fold
              (fun actor value (conflicts, children) ->
                let conflict = {actor; link= false; value= None} in
                let conflict, children =
                  unpack_conflict_value parent_id conflict children value
                in
                (CCList.append conflicts [conflict], children) )
              cs ([], children)
          in
          ({patch_diff with conflicts= Some conflicts}, children)
      | None -> (patch_diff, children) )
    | None -> (patch_diff, children)

  let get_object_fields t obj_id =
    let open CCOpt.Infix in
    ObjectIdMap.get obj_id t.by_object
    >|= fst >|= KeyMap.keys >|= CCList.of_seq
    >|= CCList.filter (fun key -> is_field_present t obj_id key)
    >|= KeySet.of_list

  let rec instantiate_object t obj_id (diffs, children) =
    match DiffMap.find_opt obj_id diffs with
    | Some _ -> (diffs, children, LinkValue {obj_id})
    | None ->
        let is_root = String.equal obj_id root_id in
        let obj_typ = get_obj_action t obj_id in
        let context =
          (DiffMap.add obj_id [] diffs, ChildMap.add obj_id [] children)
        in
        let diffs, children =
          if is_root then instantiate_map t obj_id context
          else
            match obj_typ with
            | MakeMap -> instantiate_map t obj_id context
            | MakeList -> instantiate_list t obj_id "list" context
            | MakeText -> instantiate_list t obj_id "text" context
            | _ -> raise Unknown_object_type
        in
        (diffs, children, LinkValue {obj_id})

  and instantiate_list t obj_id typ ((diffs, children) : context) =
    (diffs, children)

  and instantiate_map t obj_id ((diffs, children) : context) =
    let patch_diffs = DiffMap.find obj_id diffs in
    let patch_diffs =
      if not (CCString.equal obj_id root_id) then
        CCList.append patch_diffs
          [ { conflicts= None
            ; value= None
            ; link= false
            ; obj= obj_id
            ; type_= DiffMap
            ; action= DiffCreate
            ; key= None } ]
      else patch_diffs
    in
    let diffs, children, conflicts =
      get_object_conflicts t obj_id (diffs, children)
    in
    let diffs, children, patch_diffs =
      match get_object_fields t obj_id with
      | Some fields ->
          KeySet.fold
            (fun key (diffs, children, patch_diffs) ->
              let patch_diff =
                { conflicts= None
                ; value= None
                ; link= false
                ; obj= obj_id
                ; type_= DiffMap
                ; action= DiffSet
                ; key= Some key }
              in
              (* unpack value *)
              let patch_diff, children =
                match get_object_field t obj_id key (diffs, children) with
                | _, children, Some mat_value ->
                    let patch_diff, children =
                      unpack_value obj_id patch_diff children mat_value
                    in
                    (patch_diff, children)
                | _ -> raise (Invalid_argument "obj key")
              in
              let patch_diff, children =
                unpack_conflicts_for_key key obj_id patch_diff children
                  conflicts
              in
              (diffs, children, CCList.append patch_diffs [patch_diff]) )
            fields
            (diffs, children, patch_diffs)
      | None -> (diffs, children, patch_diffs)
    in
    (DiffMap.add obj_id patch_diffs diffs, children)

  and get_object_conflicts t obj_id (diffs, children) =
    let open CCOpt.Infix in
    let filtered =
      ObjectIdMap.get obj_id t.by_object
      >|= fst
      >|= KeyMap.filter (fun key field ->
              valid_field_name key
              && CCList.length (get_field_ops t obj_id key) > 1 )
    in
    match filtered with
    | Some fil ->
        let diffs, children, conflicts =
          KeyMap.fold
            (fun key field (diffs, children, res) ->
              let diffs, children, conflicts =
                CCList.fold_left
                  (fun (diffs, children, conflicts) (op : op) ->
                    let diffs, children, (materialized : materialized option) =
                      get_op_value t op (diffs, children)
                    in
                    let conflicts =
                      match materialized with
                      | Some mat -> OpMap.add op.actor mat conflicts
                      | None -> conflicts
                    in
                    (diffs, children, conflicts) )
                  (diffs, children, OpMap.empty)
                  (CCList.drop 1 field)
              in
              (diffs, children, KeyMap.add key conflicts res) )
            fil
            (diffs, children, KeyMap.empty)
        in
        (diffs, children, Some conflicts)
    | None -> (diffs, children, None)

  and get_op_value t (op : op) ((diffs, children) : context) =
    let value =
      CCOpt.flat_map
        (fun value ->
          match op.action with
          | Set -> Some (diffs, children, TypedValue value)
          | Link ->
              Some
                (instantiate_object t
                   (get_op_value_as_string_exn value)
                   (diffs, children))
          | _ -> None )
        op.value
    in
    match value with
    | Some (diffs, children, value) -> (diffs, children, Some value)
    | None -> (diffs, children, None)

  and get_object_field t obj_id key ((diffs, children) : context) =
    if not (valid_field_name key) then (diffs, children, None)
    else
      match get_field_ops t obj_id key with
      | [] -> (diffs, children, None)
      | hd :: _ -> get_op_value t hd (diffs, children)

  type patch = {
    can_undo: bool;
    can_redo: bool;
    clock: seq ActorMap.t;

    deps: seq ActorMap.t;
    diffs: diff list;
  } [@@deriving sexp_of]

  let rec make_patch t (obj_id : string) patch_diffs (diffs, children) =
    let patch_diffs =
      CCList.fold_left
        (fun patch_diffs child_id ->
          make_patch t child_id patch_diffs (diffs, children) )
        patch_diffs
        (ChildMap.find obj_id children)
    in
    CCList.append patch_diffs (DiffMap.find obj_id diffs)

  let get_patch t =
    let diffs, children, _ =
      instantiate_object t root_id (DiffMap.empty, ChildMap.empty)
    in
    let patch_diffs = make_patch t root_id [] (diffs, children) in
    {
      can_undo= t.undo_pos > 0;
      can_redo= not (CCList.is_empty t.redo_stack);
      clock= t.clock;
      deps= t.deps;
      diffs= patch_diffs;
    }

  let list_length t obj_id =
    let open CCOpt.Infix in
    get_obj_aux t obj_id >>= fun obj_aux -> obj_aux._elem_ids >|= CCList.length

  let get_next t obj_id key =
    match insertions_after t obj_id key None with
    | hd :: _ -> Some hd
    | [] ->
        let rec find_ancestor (key : key option) =
          match get_parent t obj_id key with
          | None -> None
          | Some ancestor -> (
            match insertions_after t obj_id (Some ancestor) key with
            | hd :: _ -> Some hd
            | [] -> find_ancestor (Some ancestor) )
        in
        find_ancestor key

  type iterator_mode = Keys | Values | Entries | Elems | Conflicts

  type iterator_val =
    | KeyValue of int
    | ValueValue of value option
    | EntryValue of int * value option
    | ElemValue of int * string
    | ConflictValue of op OpMap.t

  type iterator_res = {done_: bool; value: iterator_val option}

  type iterator = {next: unit -> iterator_res option}

  (* let list_iterator t list_id mode context = *)
  (*   let elem = ref (Some "_head") in *)
  (*   let index = ref (-1) in *)
  (*   let next () = *)
  (*     let rec _next _ = *)
  (*       elem := get_next t list_id !elem ; *)
  (*       match !elem with *)
  (*       | None -> Some {done_= true; value= None} *)
  (*       | Some elem' -> ( *)
  (*         match get_field_ops t list_id elem' with *)
  (*         | [] -> None *)
  (*         | hd :: tl as ops -> ( *)
  (*             let value = get_op_value t hd context in *)
  (*             index := !index + 1 ; *)
  (*             match mode with *)
  (*             | Keys -> Some {done_= false; value= Some (KeyValue !index)} *)
  (*             | Values -> Some {done_= false; value= Some (ValueValue value)} *)
  (*             | Entries -> *)
  (*                 Some {done_= false; value= Some (EntryValue (!index, value))} *)
  (*             | Elems -> *)
  (*                 Some {done_= false; value= Some (ElemValue (!index, elem'))} *)
  (*             | Conflicts -> *)
  (*                 let conflict = *)
  (*                   if CCList.length ops > 1 then *)
  (*                     Some *)
  (*                       (CCList.foldi *)
  (*                          (fun op_map idx op -> OpMap.add idx op op_map) *)
  (*                          OpMap.empty tl) *)
  (*                   else None *)
  (*                 in *)
  (*                 let conflict = *)
  (*                   CCOpt.map (fun c -> ConflictValue c) conflict *)
  (*                 in *)
  (*                 Some {done_= false; value= conflict} ) ) *)
  (*     in *)
  (*     _next !elem *)
  (*   in *)
  (*   (1* TODO: Symbol.iterator *1) *)
  (*   {next} *)

  let get_clock ({clock}:t) = clock

  let get_deps ({deps}:t) = deps

  let can_undo {undo_pos} = undo_pos > 0

  let can_redo {redo_stack} = not (CCList.is_empty redo_stack)
end
