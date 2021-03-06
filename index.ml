open Js_of_ocaml
open Sexplib.Conv
open Op_set
open Datastructures

let ( $ ) f g x = f (g x)

type exn +=
  | Not_supported
  | Unknown_request_type
  | Nothing_to_be_undone
  | Unexpected_operation_type_in_undo_history
  | Last_change_was_not_an_undo
  | Cannot_diff_two_states_that_have_diverged

let freeze (o : 'a) : 'a =
  Js.Unsafe.fun_call (Js.Unsafe.js_expr "Object.freeze") [|Js.Unsafe.inject o|]

type t = {op_set: OpSetBackend.t}

module BE = OpSetBackend

let clock t = OpSetBackend.get_clock t.op_set

let list_to_js_array lis =
  let arr = new%js Js.array_length (List.length lis) in
  CCList.iteri (fun i el -> Js.array_set arr i (Js.Unsafe.inject el)) lis ;
  arr

let obj_set ?conv name value obj_kv =
  match conv with
  | Some conv -> CCArray.append obj_kv [|(name, Js.Unsafe.inject (conv value))|]
  | None -> CCArray.append obj_kv [|(name, Js.Unsafe.inject value)|]

let obj_set_optdef conv name value obj_kv =
  match value with
  | Some v -> CCArray.append obj_kv [|(name, Js.Unsafe.inject (conv v))|]
  | None -> obj_kv

let obj_set_opt conv name value obj_kv =
  match value with
  | Some v -> CCArray.append obj_kv [|(name, Js.Unsafe.inject (conv v))|]
  | None -> CCArray.append obj_kv [|(name, Js.Unsafe.inject Js.null)|]

let edit_action_to_js_edit_action v =
  Js.string
    OpSetBackend.(
      match v with
      | DiffCreate -> "create"
      | DiffSet -> "set"
      | DiffInsert -> "insert"
      | DiffRemove -> "remove")

let type_to_js_type v =
  Js.string
    OpSetBackend.(
      match v with DiffMap -> "map" | DiffText -> "text" | DiffList -> "list")

let path_to_js_path v =
  list_to_js_array
    (CCList.map
       OpSetBackend.(
         function
         | `StrPath s -> Js.Unsafe.inject (Js.string s)
         | `IntPath i -> Js.Unsafe.inject (Js.number_of_float (float_of_int i)))
       v)

let actor_to_js_actor actor = Js.Unsafe.inject (Js.string actor)

let action_to_js_action a =
  Js.string
    OpSetBackend.(
      match a with
      | Op.MakeMap -> "makeMap"
      | MakeText -> "makeText"
      | MakeList -> "makeList"
      | Link -> "link"
      | Ins -> "ins"
      | Del -> "del"
      | Set -> "set")

let op_val_to_js_value = function
  | Op.BoolValue b -> Js.Unsafe.inject (Js.bool b)
  | StrValue s -> Js.Unsafe.inject (Js.string s)
  | NumberValue n -> Js.Unsafe.inject (Js.number_of_float n)
  | Null -> Js.Unsafe.inject Js.null

let rec value_to_js_value (value : OpSetBackend.value) =
  match value with
  | Value s -> op_val_to_js_value s
  | Link l -> Js.Unsafe.inject (Js.string l.obj)

let conflicts_to_js_conflicts (v : OpSetBackend.conflict list) =
  list_to_js_array
    (CCList.map
       (fun (confl : OpSetBackend.conflict) ->
         CCArray.empty
         |> obj_set ~conv:actor_to_js_actor "actor" confl.actor
         |> obj_set_optdef Js.bool "link"
              (match confl.link with Some true -> confl.link | _ -> None)
         |> obj_set_optdef value_to_js_value "value" confl.value
         |> Js.Unsafe.obj )
       v)

let number_of_int i = Js.number_of_float (float_of_int i)

let obj_set_path (edit : OpSetBackend.diff) obj_kv =
  match edit.action with
  | OpSetBackend.DiffSet | OpSetBackend.DiffRemove | OpSetBackend.DiffInsert -> (
    match edit.path with
    | Some v ->
        CCArray.append obj_kv [|("path", Js.Unsafe.inject (path_to_js_path v))|]
    | None -> CCArray.append obj_kv [|("path", Js.Unsafe.inject Js.null)|] )
  | _ -> obj_kv

let edit_to_js_edit (edit : OpSetBackend.diff) =
  CCArray.empty
  |> obj_set ~conv:edit_action_to_js_edit_action "action" edit.action
  |> obj_set ~conv:Js.string "obj" edit.obj
  |> obj_set_optdef Js.string "key" edit.key
  |> obj_set_optdef value_to_js_value "value" edit.value
  |> obj_set ~conv:type_to_js_type "type" edit.type_
  |> obj_set_optdef Js.bool "link" edit.link
  |> obj_set_path edit
  |> obj_set_optdef conflicts_to_js_conflicts "conflicts" edit.conflicts
  |> obj_set_optdef number_of_int "index" edit.index
  |> obj_set_optdef Js.string "elemId" edit.elem_id
  |> Js.Unsafe.obj

let change_op_to_js_change_op (op : Op.t) =
  CCArray.empty
  |> obj_set "action" (action_to_js_action op.action)
  |> obj_set_optdef Js.string "key"
       (if String.equal op.key "" then None else Some op.key)
  |> obj_set_optdef (Js.number_of_float $ float_of_int) "elem" op.elem
  |> obj_set_optdef op_val_to_js_value "value" op.value
  |> obj_set "obj" (Js.string op.obj)
  |> Js.Unsafe.obj

let js_obj_of_actor_map conv m =
  (* string, any array *)
  let kv =
    CCArray.of_list (ActorMap.to_list m)
    |> CCArray.map (fun (k, v) -> (k, Js.Unsafe.inject (conv v)))
  in
  Js.Unsafe.obj kv

let js_number_of_int i = Js.number_of_float (float_of_int i)

let change_to_js_change (change : Change.t) =
  let module C = Change in
  CCArray.empty
  |> obj_set ~conv:Js.string "actor" (C.actor change)
  |> obj_set ~conv:(Js.number_of_float $ float_of_int) "seq" (C.seq change)
  |> obj_set
       ~conv:(js_obj_of_actor_map js_number_of_int)
       "deps" (C.deps change)
  |> obj_set_optdef Js.string "message" (C.message change)
  |> obj_set
       ~conv:(Js.array $ CCArray.of_list $ CCList.map change_op_to_js_change_op)
       "ops" (C.ops change)
  |> Js.Unsafe.obj

module ToJs = struct
  let require_module s =
    Js.Unsafe.fun_call
      (Js.Unsafe.js_expr "require")
      [|Js.Unsafe.inject (Js.string s)|]

  let number = Js.number_of_float $ float_of_int

  let clock (clock : BE.seq ActorMap.t) = js_obj_of_actor_map number clock

  let change_list lis = CCList.map change_to_js_change lis |> list_to_js_array

  let missing_deps deps =
    ActorMap.fold
      (fun k v acc ->
        (k, Js.Unsafe.inject (Js.number_of_float (float_of_int v))) :: acc )
      deps []
    |> CCArray.of_list |> Js.Unsafe.obj

  let ref (ref : BE.ref) =
    CCArray.empty
    |> obj_set "action" (action_to_js_action ref.action)
    |> obj_set ~conv:Js.string "key" ref.key
    |> obj_set_optdef (Js.number_of_float $ float_of_int) "elem" ref.elem
    |> obj_set_optdef op_val_to_js_value "value" ref.value
    |> obj_set "obj" (Js.string ref.obj)
    |> Js.Unsafe.obj

  let imm_Map _kv =
    let immutable = require_module "immutable" in
    let _Map = (Js.Unsafe.coerce immutable) ##. Map in
    Js.Unsafe.(fun_call (inject _Map) [|inject _kv|])

  let imm_List _arr =
    let immutable = require_module "immutable" in
    let _List = (Js.Unsafe.coerce immutable) ##. List in
    Js.Unsafe.(fun_call (inject _List) [|inject _arr|])

  let imm o =
    let is_imm = (Js.Unsafe.coerce o)##.toJS in
    Js.Optdef.case is_imm
      (fun () -> o)
      (fun _ -> Js.Unsafe.(meth_call (inject o) "toJS" [||]))
end

module FromJs = struct
  let from_imm imm_obj = Js.Unsafe.(meth_call (inject imm_obj) "toJS" [||])

  let clock _clock =
    let _simple = from_imm _clock in
    let _actors = Js.object_keys _simple |> Js.to_array in
    CCArray.fold
      (fun clock _actor ->
        let _seq = Js.Unsafe.(get (inject _simple) _actor) in
        let seq : BE.seq = int_of_float (Js.float_of_number _seq) in
        ActorMap.add (Js.to_string _actor) seq clock )
      ActorMap.empty _actors
end

(* Constructs a patch object from the current node state `state` and the list *)
(* of object modifications `diffs`. *)
let make_patch t diffs =
  let clock = OpSetBackend.get_clock t.op_set in
  object%js
    val clock = js_obj_of_actor_map js_number_of_int clock

    val deps =
      js_obj_of_actor_map js_number_of_int (OpSetBackend.get_deps t.op_set)

    val canUndo = Js.bool (OpSetBackend.can_undo t.op_set)

    val canRedo = Js.bool (OpSetBackend.can_redo t.op_set)

    val diffs = diffs
  end

let init () = {op_set= OpSetBackend.init ()}

let apply t changes undoable =
  (* BE.LLog.change_list "apply changes" changes ; *)
  let t, diffs =
    CCList.fold_left
      (fun (t, diffs) change ->
        let op_set, new_diffs =
          OpSetBackend.add_change t.op_set change undoable
        in
        ({op_set}, CCList.concat [diffs; new_diffs]) )
      (t, []) changes
  in
  (* BE.LLog.t_states "apply" t.op_set.states ; *)
  (* BE.LLog.edit_list "apply diffs" diffs ; *)
  (t, diffs)

let apply_changes t changes = apply t changes false

let _apply_changes t js_changes =
  let js_changes = ToJs.imm js_changes in
  let changes =
    CCArray.to_list (Js.to_array js_changes) |> CCList.map Change.of_js
  in
  let t, diffs = apply_changes t changes in
  let js_diffs = list_to_js_array (CCList.map edit_to_js_edit diffs) in
  let js_patch = make_patch t js_diffs in
  let ret = new%js Js.array_length 2 in
  Js.array_set ret 0 (Js.Unsafe.inject t) ;
  Js.array_set ret 1 (Js.Unsafe.inject js_patch) ;
  ret

let undo t change =
  let undo_pos = t.op_set.undo_pos in
  if undo_pos < 1 then raise Nothing_to_be_undone
  else
    match CCList.nth_opt t.op_set.undo_stack (undo_pos - 1) with
    | None -> raise Nothing_to_be_undone
    | Some undo_ops ->
        let change_ops =
          CCList.map
            (fun (un_op : BE.ref) ->
              ( { action= un_op.action
                ; key= un_op.key
                ; obj= un_op.obj
                ; elem= None
                ; value= un_op.value }
                : Op.t ) )
            undo_ops
        in
        let change = Change.set_ops change change_ops in
        let op_set = t.op_set in
        let redo_ops =
          CCList.fold_left
            (fun redo_ops (op : BE.ref) ->
              match op.action with
              | Op.Del | Set | Link ->
                  let field_ops = BE.get_field_ops op_set op.obj op.key in
                  if CCList.is_empty field_ops then
                    CCList.append redo_ops
                      [ ( { action= Op.Del
                          ; obj= op.obj
                          ; key= op.key
                          ; value= None
                          ; elem= None }
                          : BE.ref ) ]
                  else
                    CCList.fold_left
                      (fun redo_ops ((meta, field_op) : BE.op_with_meta) ->
                        CCList.append redo_ops
                          [ ( { action= field_op.action
                              ; key= field_op.key
                              ; obj= field_op.obj
                              ; elem= field_op.elem
                              ; value= field_op.value }
                              : BE.ref ) ] )
                      redo_ops field_ops
              | _ -> raise Unexpected_operation_type_in_undo_history )
            [] undo_ops
        in
        let op_set =
          { op_set with
            undo_pos= undo_pos - 1
          ; redo_stack= CCList.append op_set.redo_stack [redo_ops] }
        in
        let new_op_set, diffs = BE.add_change op_set change false in
        let t = {op_set= new_op_set} in
        (t, diffs)

let redo t (change : Change.t) =
  let redo_ops = CCList.last_opt t.op_set.redo_stack in
  match redo_ops with
  | None -> raise Last_change_was_not_an_undo
  | Some redo_ops ->
      let change_ops =
        CCList.map
          (fun (un_op : BE.ref) ->
            ( { action= un_op.action
              ; key= un_op.key
              ; obj= un_op.obj
              ; elem= None
              ; value= un_op.value }
              : Op.t ) )
          redo_ops
      in
      let change = Change.set_ops change change_ops in
      let op_set = t.op_set in
      let op_set =
        { op_set with
          undo_pos= op_set.undo_pos + 1
        ; redo_stack=
            CCList.remove_at_idx
              (CCList.length op_set.redo_stack - 1)
              op_set.redo_stack }
      in
      let new_op_set, diffs = BE.add_change op_set change false in
      ({op_set= new_op_set}, diffs)

let apply_local_change t js_change =
  let change = Change.of_js js_change in
  let request_type = Js.to_string (Js.Unsafe.coerce js_change)##.requestType in
  let t, diffs =
    if CCString.equal request_type "change" then apply t [change] true
    else if CCString.equal request_type "undo" then undo t change
    else if CCString.equal request_type "redo" then redo t change
    else raise Unknown_request_type
  in
  let js_diffs = list_to_js_array (CCList.map edit_to_js_edit diffs) in
  let js_patch = make_patch t js_diffs in
  (Js.Unsafe.coerce js_patch)##.actor := (Js.Unsafe.coerce js_change)##.actor ;
  (Js.Unsafe.coerce js_patch)##.seq := (Js.Unsafe.coerce js_change)##.seq ;
  let ret = new%js Js.array_length 2 in
  Js.array_set ret 0 (Js.Unsafe.inject t) ;
  Js.array_set ret 1 (Js.Unsafe.inject js_patch) ;
  ret

let diff_to_js_diff (diff : OpSetBackend.diff) =
  let action =
    Js.string
      ( match diff.action with
      | DiffSet -> "set"
      | DiffCreate -> "create"
      | DiffInsert -> "insert"
      | DiffRemove -> "remove" )
  in
  let type_ =
    Js.string
      ( match diff.type_ with
      | DiffMap -> "map"
      | DiffText -> "text"
      | DiffList -> "list" )
  in
  CCArray.empty |> obj_set "action" action
  |> obj_set_optdef Js.string "key" diff.key
  |> obj_set ~conv:Js.string "obj" diff.obj
  |> obj_set "type" type_
  |> obj_set_optdef Js.bool "link" diff.link
  |> obj_set_optdef value_to_js_value "value" diff.value
  |> obj_set_optdef (Js.number_of_float $ float_of_int) "index" diff.index
  |> obj_set_optdef Js.string "elemId" diff.elem_id
  |> obj_set_optdef conflicts_to_js_conflicts "conflicts" diff.conflicts
  |> Js.Unsafe.obj

let get_patch t =
  let patch = OpSetBackend.get_patch t.op_set in
  let diffs = list_to_js_array (CCList.map diff_to_js_diff patch.diffs) in
  object%js
    val canUndo = Js.bool patch.can_undo

    val canRedo = Js.bool patch.can_redo

    val clock = js_obj_of_actor_map js_number_of_int patch.clock

    val deps = js_obj_of_actor_map js_number_of_int patch.deps

    val diffs = diffs
  end

let merge local remote =
  let changes =
    OpSetBackend.get_missing_changes remote.op_set
      (OpSetBackend.get_clock local.op_set)
  in
  let t, diffs = apply_changes local changes in
  let js_diffs = list_to_js_array (CCList.map edit_to_js_edit diffs) in
  let js_patch = make_patch t js_diffs in
  let ret = new%js Js.array_length 2 in
  Js.array_set ret 0 (Js.Unsafe.inject t) ;
  Js.array_set ret 1 (Js.Unsafe.inject js_patch) ;
  ret

let get_changes_for_actor t js_actor_id =
  OpSetBackend.get_changes_for_actor t.op_set (Js.to_string js_actor_id)
  |> CCList.map change_to_js_change
  |> CCArray.of_list |> Js.array

let less_or_equal clk1 clk2 =
  let keys =
    CCList.of_seq (ActorMap.keys clk1) @ CCList.of_seq (ActorMap.keys clk2)
  in
  CCList.fold_left
    (fun result key ->
      result
      && ActorMap.get_or ~default:0 key clk1
         <= ActorMap.get_or ~default:0 key clk2 )
    true keys

let get_changes old_state new_state =
  let old_clock = clock old_state in
  if not (less_or_equal old_clock new_state.op_set.clock) then
    raise Cannot_diff_two_states_that_have_diverged
  else
    (* function lessOrEqual(clock1, clock2) { *)
    (*   return clock1.keySeq().concat(clock2.keySeq()).reduce( *)
    (*     (result, key) => (result && clock1.get(key, 0) <= clock2.get(key, 0)), *)
    (*     true) *)
    (* } *)
    BE.get_missing_changes new_state.op_set old_clock
    |> CCList.map change_to_js_change
    |> list_to_js_array

let get_missing_changes t _clock =
  let clock = FromJs.clock _clock in
  let changes = BE.get_missing_changes t.op_set clock in
  ToJs.change_list changes

let get_missing_deps t =
  let deps = BE.get_missing_deps t.op_set in
  ToJs.missing_deps deps

let get_clock t =
  let immutable = ToJs.require_module "immutable" in
  let map = (Js.Unsafe.coerce immutable) ##. Map in
  let clock = ToJs.clock (OpSetBackend.get_clock t.op_set) in
  Js.Unsafe.fun_call (Js.Unsafe.inject map) [|Js.Unsafe.inject clock|]

let get_history t =
  let h = BE.get_history t.op_set in
  (* BE.LLog.change_list "h" h ; *)
  h |> CCList.map change_to_js_change |> list_to_js_array

let get_undo_stack t =
  let undo_stack = BE.get_undo_stack t.op_set in
  let lis_of_arrs =
    ToJs.imm_List
      (list_to_js_array
         (CCList.map
            (fun un ->
              ToJs.imm_List (list_to_js_array (CCList.map ToJs.ref un)) )
            undo_stack))
  in
  lis_of_arrs

let get_redo_stack t =
  let redo_stack = BE.get_redo_stack t.op_set in
  let lis_of_arrs =
    ToJs.imm_List
      (list_to_js_array
         (CCList.map
            (fun un ->
              ToJs.imm_List (list_to_js_array (CCList.map ToJs.ref un)) )
            redo_stack))
  in
  lis_of_arrs

let _ =
  Js.export "init" init ;
  Js.export "applyChanges" _apply_changes ;
  Js.export "applyLocalChange" apply_local_change ;
  Js.export "getPatch" get_patch ;
  Js.export "merge" merge ;
  Js.export "getChangesForActor" get_changes_for_actor ;
  Js.export "getChanges" get_changes ;
  Js.export "getMissingChanges" get_missing_changes ;
  Js.export "getMissingDeps" get_missing_deps ;
  Js.export "getClock" get_clock ;
  Js.export "getHistory" get_history ;
  Js.export "getUndoStack" get_undo_stack ;
  Js.export "getRedoStack" get_redo_stack
