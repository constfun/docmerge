open Sexplib.Conv
open Op_set
open Datastructures

let ( $ ) f g x = f (g x)

type exn += Not_supported | Unknown_request_type

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

let array_to_list arr = CCArray.to_list (Js.to_array arr)

let js_action_to_action : Js.js_string Js.t -> OpSetBackend.action =
 fun js_s ->
  let s = Js.to_string js_s in
  if String.equal s "set" then OpSetBackend.Set
  else if String.equal s "del" then OpSetBackend.Del
  else if String.equal s "makeMap" then OpSetBackend.MakeMap
  else if String.equal s "makeList" then OpSetBackend.MakeList
  else if String.equal s "makeText" then OpSetBackend.MakeText
  else if String.equal s "link" then OpSetBackend.Link
  else if String.equal s "ins" then OpSetBackend.Ins
  else raise Not_supported

let action_to_js_action a =
  Js.string
    OpSetBackend.(
      match a with
      | MakeMap -> "makeMap"
      | MakeText -> "makeText"
      | MakeList -> "makeList"
      | Link -> "link"
      | Ins -> "ins"
      | Del -> "del"
      | Set -> "set")

let op_val_to_js_value = function
  | BE.BoolValue b -> Js.Unsafe.inject (Js.bool b)
  | BE.StrValue s -> Js.Unsafe.inject (Js.string s)
  | BE.NumberValue n -> Js.Unsafe.inject (Js.number_of_float n)
  | BE.Null -> Js.Unsafe.inject Js.null

let rec value_to_js_value (value : OpSetBackend.value) =
  match value with
  | Value s -> op_val_to_js_value s
  | Link l -> Js.Unsafe.inject (Js.string l.obj)

let rec js_value_to_op_val js_value =
  Js.Opt.case js_value
    (fun () -> OpSetBackend.Null)
    (fun js_value ->
      let typ = Js.to_string (Js.typeof js_value) in
      match typ with
      | "string" -> BE.StrValue (Js.to_string (Js.Unsafe.coerce js_value))
      | "boolean" -> BE.BoolValue (Js.to_bool (Js.Unsafe.coerce js_value))
      | "number" ->
          BE.NumberValue (Js.float_of_number (Js.Unsafe.coerce js_value))
      | _ -> raise Not_supported )

let to_op_list arr =
  array_to_list arr
  |> CCList.map (fun js_op ->
         ( { action= js_action_to_action js_op##.action
           ; key= Js.Optdef.(to_option (map js_op##.key Js.to_string))
           ; elem=
               Js.Optdef.(
                 to_option (map js_op##.elem (int_of_float $ Js.to_float)))
           ; value=
               Js.Optdef.(to_option (map js_op##.value js_value_to_op_val))
           ; obj= Js.to_string js_op##.obj }
           : OpSetBackend.change_op ) )

let conflicts_to_js_conflicts (v : OpSetBackend.conflict list) =
  list_to_js_array
    (CCList.map
       (fun (confl : OpSetBackend.conflict) ->
         let arr =
           CCArray.empty |> obj_set ~conv:actor_to_js_actor "actor" confl.actor
         in
         BE.LLog.value "confl.value" (CCOpt.get_exn confl.value) ;
         let arr =
           match confl.value with
           | Some (BE.Link _) ->
               print_endline "link" ;
               CCArray.append arr [|("link", Js.Unsafe.inject (Js.bool true))|]
           | _ -> print_endline "no link" ; arr
         in
         arr
         |> obj_set_optdef value_to_js_value "value" confl.value
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
  |> obj_set ~conv:edit_action_to_js_edit_action "action" edit.action
  |> obj_set ~conv:Js.string "obj" edit.obj
  |> obj_set_optdef Js.string "key" edit.key
  |> obj_set_optdef value_to_js_value "value" edit.value
  |> obj_set ~conv:type_to_js_type "type" edit._type
  |> obj_set_optdef Js.bool "link" (if edit.link then Some edit.link else None)
  |> obj_set_path edit
  |> obj_set_optdef conflicts_to_js_conflicts "conflicts" edit.conflicts
  |> obj_set_optdef number_of_int "index" edit.index
  |> obj_set_optdef Js.string "elemId" edit.elem_id__key
  |> Js.Unsafe.obj

let change_op_to_js_change_op (op : OpSetBackend.change_op) =
  CCArray.empty
  |> obj_set "action" (action_to_js_action op.action)
  |> obj_set_optdef Js.string "key" op.key
  |> obj_set_optdef (Js.number_of_float $ float_of_int) "elem" op.elem
  |> obj_set_optdef op_val_to_js_value "value" op.value
  |> obj_set "obj" (Js.string op.obj)
  |> Js.Unsafe.obj

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

let int_of_js_number n = int_of_float (Js.float_of_number n)

let js_change_to_change js_change : OpSetBackend.change =
  { actor= Js.to_string js_change##.actor
  ; seq= int_of_js_number js_change##.seq
  ; deps= actor_map_of_js_obj js_change##.deps
  ; ops= to_op_list js_change##.ops
  ; message=
      Js.Optdef.case js_change##.message
        (fun () -> None)
        (fun msg -> Some (Js.to_string msg)) }

let js_number_of_int i = Js.number_of_float (float_of_int i)

let change_to_js_change (change : OpSetBackend.change) =
  CCArray.empty
  |> obj_set ~conv:Js.string "actor" change.actor
  |> obj_set ~conv:(Js.number_of_float $ float_of_int) "seq" change.seq
  |> obj_set ~conv:(js_obj_of_actor_map js_number_of_int) "deps" change.deps
  |> obj_set_optdef Js.string "message" change.message
  |> obj_set
       ~conv:(Js.array $ CCArray.of_list $ CCList.map change_op_to_js_change_op)
       "ops" change.ops
  |> Js.Unsafe.obj

module ToJs = struct
  let require_module s =
    Js.Unsafe.fun_call
      (Js.Unsafe.js_expr "require")
      [|Js.Unsafe.inject (Js.string s)|]

  let number = Js.number_of_float $ float_of_int

  let clock (clock : BE.seq ActorMap.t) = js_obj_of_actor_map number clock

  let change_list lis = CCList.map change_to_js_change lis |> list_to_js_array

  let imm_Map _kv =
    let immutable = require_module "immutable" in
    let _Map = (Js.Unsafe.coerce immutable) ##. Map in
    Js.Unsafe.(fun_call (inject _Map) [|inject _kv|])

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
  (* BE.LLog.edit_list "apply diffs" diffs ; *)
  (t, diffs)

let apply_changes t changes = apply t changes false

let _apply_changes t js_changes =
  let js_changes = ToJs.imm js_changes in
  let changes =
    CCArray.to_list (Js.to_array js_changes) |> CCList.map js_change_to_change
  in
  let t, diffs = apply_changes t changes in
  let js_diffs = list_to_js_array (CCList.map edit_to_js_edit diffs) in
  let js_patch = make_patch t js_diffs in
  let ret = new%js Js.array_length 2 in
  Js.array_set ret 0 (Js.Unsafe.inject t) ;
  Js.array_set ret 1 (Js.Unsafe.inject js_patch) ;
  ret

let apply_local_change t js_change =
  let change = js_change_to_change js_change in
  let request_type = Js.to_string js_change##.requestType in
  let t, diffs =
    if CCString.equal request_type "change" then apply t [change] true
    else raise Unknown_request_type
  in
  let js_diffs = list_to_js_array (CCList.map edit_to_js_edit diffs) in
  let js_patch = make_patch t js_diffs in
  (Js.Unsafe.coerce js_patch)##.actor := js_change##.actor ;
  (Js.Unsafe.coerce js_patch)##.seq := js_change##.seq ;
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
      | DiffInsert -> "insert" )
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
  (* BE.LLog.edit_list "diffs" diffs ; *)
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

let get_changes old_state new_state =
  let old_clock = clock old_state in
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

let get_clock t =
  let immutable = ToJs.require_module "immutable" in
  let map = (Js.Unsafe.coerce immutable) ##. Map in
  let clock = ToJs.clock (OpSetBackend.get_clock t.op_set) in
  Js.Unsafe.fun_call (Js.Unsafe.inject map) [|Js.Unsafe.inject clock|]

let get_history t =
  let h = BE.get_history t.op_set in
  (* BE.LLog.change_list "h" h ; *)
  h |> CCList.map change_to_js_change |> list_to_js_array

let _ =
  Js.export "init" init ;
  Js.export "applyChanges" _apply_changes ;
  Js.export "applyLocalChange" apply_local_change ;
  Js.export "getPatch" get_patch ;
  Js.export "merge" merge ;
  Js.export "getChangesForActor" get_changes_for_actor ;
  Js.export "getChanges" get_changes ;
  Js.export "getMissingChanges" get_missing_changes ;
  Js.export "getClock" get_clock ;
  Js.export "getHistory" get_history
