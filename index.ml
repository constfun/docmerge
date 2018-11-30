open Op_set

type exn +=
  | Not_supported

let freeze (o : 'a) : 'a =
  Js.Unsafe.fun_call (Js.Unsafe.js_expr "Object.freeze") [|Js.Unsafe.inject o|]

type t = {op_set: OpSetBackend.t}


let actor_map_of_js_obj js_obj =
  Js.to_array (Js.object_keys js_obj)
  |> CCArray.fold (fun amap js_actor ->
      let value = Js.Unsafe.get js_obj js_actor in
      ActorMap.add (Js.to_string js_actor) value amap
    )
  ActorMap.empty

let js_obj_of_actor_map conv m =
  (* string, any array *)
  let kv =
    CCArray.of_list (ActorMap.to_list m)
    |> CCArray.map (fun (k, v) -> k, Js.Unsafe.inject (conv v))
  in
  Js.Unsafe.obj kv

let js_number_of_int i = Js.number_of_float (float_of_int i)

(* Constructs a patch object from the current node state `state` and the list *)
(* of object modifications `diffs`. *)
let make_patch t diffs =
  let clock =OpSetBackend.get_clock t.op_set in
  object%js
    val clock = js_obj_of_actor_map js_number_of_int clock
    val deps = js_obj_of_actor_map js_number_of_int (OpSetBackend.get_deps t.op_set)
    val canUndo = Js.bool (OpSetBackend.can_undo t.op_set)
    val canRedo = Js.bool (OpSetBackend.can_redo t.op_set)
    val diffs = diffs
  end

let init () = {op_set= OpSetBackend.init ()}

let addChange {op_set} = freeze (OpSetBackend.add_change op_set)

let int_of_js_number n =
  int_of_float (Js.float_of_number n)

let array_to_list arr =
  CCArray.to_list (Js.to_array arr)

let action_from_str : Js.js_string Js.t -> OpSetBackend.action = fun js_s ->
  let s = Js.to_string js_s in
  if String.equal s "set" then OpSetBackend.Set
  else raise Not_supported

let to_op_list arr =
  array_to_list arr
  |> CCList.map (fun js_op ->
      ({
        action = action_from_str js_op##.action;
        key = Js.to_string js_op##.key;
        elem = Js.Optdef.to_option js_op##.elem;
        value = Js.Optdef.to_option js_op##.value;
        obj = Js.to_string js_op##.obj;
      } : OpSetBackend.change_op)
    )

      (* assert.deepEqual(patch1, { *)
      (*   canUndo: false, canRedo: false, clock: {[actor]: 1}, deps: {[actor]: 1}, *)
      (*   diffs: [{action: 'set', obj: ROOT_ID, path: [], type: 'map', key: 'bird', value: 'magpie'}] *)
      (* }) *)

let list_to_js_array lis =
  let arr = new%js Js.array_length (List.length lis) in
  CCList.iteri (fun i el -> Js.array_set arr i (Js.Unsafe.inject el)) lis;
  arr

let obj_set conv name value obj_kv =
  CCArray.append obj_kv [|name, (Js.Unsafe.inject (conv value))|]

let obj_set_opt conv name value obj_kv =
  match value with
  | Some v -> CCArray.append obj_kv [|name, (Js.Unsafe.inject (conv v))|]
  | None -> obj_kv

let rec value_to_js_value = function
  | OpSetBackend.Value s -> Js.Unsafe.inject (Js.string s)
  | OpSetBackend.Link l -> Js.Unsafe.inject (Js.Unsafe.obj [|
      "obj", value_to_js_value l.obj
    |])

let edit_action_to_js_edit_action v =
  Js.string OpSetBackend.(match v with
      | Create -> "create"
      | Set -> "set"
      | Insert -> "insert"
      | Remove -> "remove"
    )

let edit_to_js_edit (edit: OpSetBackend.edit) =
  CCArray.empty
  |> obj_set edit_action_to_js_edit_action "action" edit.action
  |> obj_set_opt Js.string "key" edit.key
  |> obj_set_opt value_to_js_value "value" edit.value
  |> Js.Unsafe.obj

let apply t changes undoable =
  let changes = Js.to_array changes in
  let t, diffs = CCArray.fold_left (fun (t, diffs) js_change ->
      let change : OpSetBackend.change = {
        actor = Js.to_string js_change##.actor;
        seq =  int_of_js_number js_change##.seq;
        deps = actor_map_of_js_obj js_change##.deps;
        ops = to_op_list js_change##.ops;
      } in
      let op_set, new_diffs = OpSetBackend.add_change t.op_set change false in
      Log.log_str ("DLEN "  ^ (string_of_int (CCList.length new_diffs)));
      {op_set}, CCList.concat [diffs; new_diffs]
    ) (t, []) changes in
  let js_diffs = list_to_js_array (CCList.map edit_to_js_edit diffs) in
  let js_patch = make_patch t js_diffs in
  Log.log_str ("LEN " ^ (string_of_int (CCList.length diffs)));
  Log.log "JS_PATCH" js_patch;
  let ret = new%js Js.array_length 2 in
  Js.array_set ret 0 (Js.Unsafe.inject t);
  Js.array_set ret 1 (Js.Unsafe.inject js_patch);
  ret

let apply_changes t changes =
  apply t changes false

let _ =
  Js.export "init" init ;
  Js.export "applyChanges" apply_changes ;

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
