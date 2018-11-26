open Op_set

type exn +=
  | Not_supported

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

let int_of_js_number n =
  int_of_float (Js.float_of_number n)

let actor_map_of_js_obj js_obj =
  Js.to_array (Js.object_keys js_obj)
  |> CCArray.fold (fun amap js_actor ->
      let value = Js.Unsafe.get js_obj js_actor in
      ActorMap.add (Js.to_string js_actor) value amap
    )
  ActorMap.empty

let array_to_list arr =
  CCArray.to_list (Js.to_array arr)

let action_from_str : Js.js_string Js.t -> OpSetBackend.action = fun js_s ->
  let s = Js.to_string js_s in
  if s == "set" then OpSetBackend.Set
  else raise Not_supported

let to_op_list arr =
  array_to_list arr
  |> CCList.map (fun js_op ->
      ({
        actor = Js.to_string js_op##.actor;
        action = action_from_str js_op##.action;
        key = Js.to_string js_op##.key;
        seq = int_of_js_number js_op##.seq; (* option? *)
        elem = int_of_js_number js_op##.elem; (* option? *)
        value = None; (* TODO *)
        obj = Js.to_string js_op##.obj;
      } : OpSetBackend.op)
    )

      (* let {action: 'set', obj: ROOT_ID, key: 'bird', value: 'magpie'} *)

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
      {op_set}, CCList.append diffs [new_diffs]
    ) (t, []) changes in
  (t, make_patch t diffs)

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
