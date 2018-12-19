open Op_set
open Datastructures

let ( $ ) f g x = f (g x)

type exn += Not_supported

let freeze (o : 'a) : 'a =
  Js.Unsafe.fun_call (Js.Unsafe.js_expr "Object.freeze") [|Js.Unsafe.inject o|]

type t = {op_set: OpSetBackend.t}

let actor_map_of_js_obj js_obj =
  Js.to_array (Js.object_keys js_obj)
  |> CCArray.fold
       (fun amap js_actor ->
         let value = Js.Unsafe.get js_obj js_actor in
         ActorMap.add (Js.to_string js_actor) value amap )
       ActorMap.empty

let js_obj_of_actor_map conv m =
  (* string, any array *)
  let kv =
    CCArray.of_list (ActorMap.to_list m)
    |> CCArray.map (fun (k, v) -> (k, Js.Unsafe.inject (conv v)))
  in
  Js.Unsafe.obj kv

let js_number_of_int i = Js.number_of_float (float_of_int i)

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

let int_of_js_number n = int_of_float (Js.float_of_number n)

let array_to_list arr = CCArray.to_list (Js.to_array arr)

let action_from_str : Js.js_string Js.t -> OpSetBackend.action =
 fun js_s ->
  let s = Js.to_string js_s in
  if String.equal s "set" then OpSetBackend.Set
  else if String.equal s "del" then OpSetBackend.Del
  else if String.equal s "makeMap" then OpSetBackend.MakeMap
  else if String.equal s "makeList" then OpSetBackend.MakeList
  else if String.equal s "link" then OpSetBackend.Link
  else if String.equal s "ins" then OpSetBackend.Ins
  else raise Not_supported

let op_val_to_js_value = function
  | OpSetBackend.BoolValue b -> Js.Unsafe.inject (Js.bool b)
  | OpSetBackend.StrValue s -> Js.Unsafe.inject (Js.string s)
  | OpSetBackend.NumberValue n -> Js.Unsafe.inject (Js.number_of_float n)

let rec value_to_js_value (value : OpSetBackend.value) =
  match value with
  | Value s -> op_val_to_js_value s
  | Link l ->
      Js.Unsafe.inject
        (object%js
           val obj = value_to_js_value l.obj
        end)

let rec js_value_to_op_val js_value =
  let typ = Js.to_string (Js.typeof js_value) in
  match typ with
  | "string" ->
      OpSetBackend.StrValue (Js.to_string (Js.Unsafe.coerce js_value))
  | "boolean" ->
      OpSetBackend.BoolValue (Js.to_bool (Js.Unsafe.coerce js_value))
  | "number" ->
      OpSetBackend.NumberValue (Js.float_of_number (Js.Unsafe.coerce js_value))
  | _ -> raise Not_supported

let to_op_list arr =
  array_to_list arr
  |> CCList.map (fun js_op ->
         ( { action= action_from_str js_op##.action
           ; key= Js.Optdef.(to_option (map js_op##.key Js.to_string))
           ; elem=
               Js.Optdef.(
                 to_option (map js_op##.elem (int_of_float $ Js.to_float)))
           ; value=
               Js.Optdef.(to_option (map js_op##.value js_value_to_op_val))
           ; obj= Js.to_string js_op##.obj }
           : OpSetBackend.change_op ) )

let list_to_js_array lis =
  let arr = new%js Js.array_length (List.length lis) in
  CCList.iteri (fun i el -> Js.array_set arr i (Js.Unsafe.inject el)) lis ;
  arr

let obj_set conv name value obj_kv =
  CCArray.append obj_kv [|(name, Js.Unsafe.inject (conv value))|]

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
      | Create -> "create"
      | Set -> "set"
      | Insert -> "insert"
      | Remove -> "remove")

let type_to_js_type v =
  Js.string
    OpSetBackend.(
      match v with Map -> "map" | Text -> "text" | List -> "list")

let path_to_js_path v =
  list_to_js_array
    (CCList.map
       OpSetBackend.(
         function
         | `StrPath s -> Js.Unsafe.inject (Js.string s)
         | `IntPath i -> Js.Unsafe.inject (Js.number_of_float (float_of_int i)))
       v)

let actor_to_js_actor actor = Js.Unsafe.inject (Js.string actor)

let conflicts_to_js_conflicts (v : OpSetBackend.conflict list) =
  list_to_js_array
    (CCList.map
       (fun (confl : OpSetBackend.conflict) ->
         CCArray.empty
         |> obj_set actor_to_js_actor "actor" confl.actor
         |> obj_set_optdef op_val_to_js_value "value" confl.value
         |> Js.Unsafe.obj )
       v)

let number_of_int i = Js.number_of_float (float_of_int i)

let obj_set_path (edit : OpSetBackend.edit) obj_kv =
  match edit.action with
  | OpSetBackend.Set | OpSetBackend.Remove | OpSetBackend.Insert -> (
    match edit.path with
    | Some v ->
        CCArray.append obj_kv [|("path", Js.Unsafe.inject (path_to_js_path v))|]
    | None -> CCArray.append obj_kv [|("path", Js.Unsafe.inject Js.null)|] )
  | _ -> obj_kv

let edit_to_js_edit (edit : OpSetBackend.edit) =
  CCArray.empty
  |> obj_set edit_action_to_js_edit_action "action" edit.action
  |> obj_set Js.string "obj" edit.obj
  |> obj_set_optdef Js.string "key" edit.key
  |> obj_set_optdef value_to_js_value "value" edit.value
  |> obj_set type_to_js_type "type" edit._type
  |> obj_set_optdef Js.bool "link" (if edit.link then Some edit.link else None)
  |> obj_set_path edit
  |> obj_set_optdef conflicts_to_js_conflicts "conflicts" edit.conflicts
  |> obj_set_optdef number_of_int "index" edit.index
  |> obj_set_optdef Js.string "elemId" edit.elem_id__key
  |> Js.Unsafe.obj

let apply t changes undoable =
  let changes = Js.to_array changes in
  let t, diffs =
    CCArray.fold_left
      (fun (t, diffs) js_change ->
        let change : OpSetBackend.change =
          { actor= Js.to_string js_change##.actor
          ; seq= int_of_js_number js_change##.seq
          ; deps= actor_map_of_js_obj js_change##.deps
          ; ops= to_op_list js_change##.ops }
        in
        let op_set, new_diffs =
          OpSetBackend.add_change t.op_set change undoable
        in
        ({op_set}, CCList.concat [diffs; new_diffs]) )
      (t, []) changes
  in
  let js_diffs = list_to_js_array (CCList.map edit_to_js_edit diffs) in
  let js_patch = make_patch t js_diffs in
  let ret = new%js Js.array_length 2 in
  Js.array_set ret 0 (Js.Unsafe.inject t) ;
  Js.array_set ret 1 (Js.Unsafe.inject js_patch) ;
  ret

let apply_changes t changes = apply t changes false


let diff_to_js_diff (diff:OpSetBackend.diff) =
  let action = Js.string (match diff.action with
    | DiffSet -> "set"
    | DiffCreate -> "create"
    )
  in
  let type_ = Js.string (match diff.type_ with
    | DiffMap -> "map"
    | DiffText -> "text"
    | DiffList -> "list"
    )
  in
  object%js
    val action = action
    val key = (Js.Optdef.option (CCOpt.map Js.string diff.key))
    val obj = Js.string diff.obj
    val type_ = type_
    val value = (Js.Optdef.option (CCOpt.map op_val_to_js_value diff.value))
  end

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

(*TODO*)

let _ =
  Js.export "init" init ;
  Js.export "applyChanges" apply_changes ;
  Js.export "getPatch" get_patch

(* Js.export "getMissingChanges" OpSetBackend.get_missing_changes ; *)
(* Js.export "getChangesForActor" OpSetBackend.get_changes_for_actor ; *)
(* Js.export "getMissingDeps" OpSetBackend.get_missing_deps ; *)
(* Js.export "getObjectFields" OpSetBackend.get_object_fields ; *)
(* Js.export "getObjectField" OpSetBackend.get_object_field ; *)
(* Js.export "getObjectConflicts" OpSetBackend.get_object_conflicts ; *)
(* Js.export "getFieldOps" OpSetBackend.get_field_ops ; *)
(* Js.export "listElemByIndex" OpSetBackend.list_elem_by_index ; *)
(* Js.export "listLength" OpSetBackend.list_length ; *)
(* Js.export "listIterator" OpSetBackend.list_iterator ; *)
(* Js.export "root_id" OpSetBackend.root_id *)
