open Op_set

let freeze (o : 'a) : 'a =
  Js.Unsafe.fun_call (Js.Unsafe.js_expr "Object.freeze") [|Js.Unsafe.inject o|]

type t = {op_set: OpSetBackend.t}



(* Constructs a patch object from the current node state `state` and the list *)
(* of object modifications `diffs`. *)
let make_patch t diffs =
  object%js
    val clock = OpSetBackend.get_clock t.op_set
    val deps = OpSetBackend.get_deps t.op_set
    val canUndo = OpSetBackend.can_undo t.op_set
    val canRedo = OpSetBackend.can_redo t.op_set
    val diffs = diffs
  end

let init () = freeze {op_set= OpSetBackend.init ()}

let addChange {op_set} = freeze (OpSetBackend.add_change op_set)


   (* const change1 = { *)
   (*                         actor, *)
   (*                         seq: 1, *)
   (*                         deps: {}, *)
   (*                         ops: [ *)
   (*      {action: 'set', obj: ROOT_ID, key: 'bird', value: 'magpie'} *)
   (*    ]} *)


  (* type op = *)
   (*  { key: key *)
   (*  ; action: action *)
   (*  ; actor: actor *)
   (*  ; seq: seq *)
   (*  ; obj: obj_id *)
   (*  ; elem: int *)
   (*  ; value: string option } *)

  (* type change = *)
   (*  { actor: actor *)
   (*  ; seq: seq *)
   (*  ; (1* List of depended op sequences by actor. *1) *)
   (*    deps: seq ActorMap.t *)
   (*  ; ops: op list } *)

let log : string -> 'a Js.t -> unit = fun msg o ->
  Js.Unsafe.fun_call (Js.Unsafe.js_expr "console.log") [|
    Js.Unsafe.inject (Js.string msg);
    Js.Unsafe.inject o;
  |]

let make_actor_map js_obj =
  Js.to_array (Js.object_keys js_obj)
  |> CCArray.fold (fun amap js_actor ->
      let value = Js.Unsafe.get js_obj js_actor in
      ActorMap.add (Js.to_string js_actor) value amap
    )
  ActorMap.empty

let apply t changes undoable =
  let changes = Js.to_array changes in
  let t, diffs = CCArray.fold_left (fun (t, diffs) js_change ->
      log "JS_CHANGE##deps" js_change;
      let change : OpSetBackend.change = {
        actor = js_change##.actor;
        seq = js_change##.seq;
        deps = make_actor_map js_change##.deps;
        ops = [];
      } in
      let op_set, new_diffs = OpSetBackend.add_change t.op_set change false in
      {op_set}, CCList.append diffs [new_diffs]
    ) (t, []) changes in
  (t, make_patch t diffs)

let apply_changes t changes =
  apply t changes false

let _ =
  Js.export "init" OpSetBackend.init ;
  Js.export "applyChanges" apply_changes ;
  Js.export "getMissingChanges" OpSetBackend.get_missing_changes ;
  Js.export "getChangesForActor" OpSetBackend.get_changes_for_actor ;
  Js.export "getMissingDeps" OpSetBackend.get_missing_deps ;
  Js.export "getObjectFields" OpSetBackend.get_object_fields ;
  Js.export "getObjectField" OpSetBackend.get_object_field ;
  Js.export "getObjectConflicts" OpSetBackend.get_object_conflicts ;
  Js.export "getFieldOps" OpSetBackend.get_field_ops ;
  Js.export "listElemByIndex" OpSetBackend.list_elem_by_index ;
  Js.export "listLength" OpSetBackend.list_length ;
  Js.export "listIterator" OpSetBackend.list_iterator ;
  Js.export "root_id" OpSetBackend.root_id
