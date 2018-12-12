module ActorMap : CCMap.S with type key = string

module SeqMap : CCMap.S

module ObjectIdMap : CCMap.S

module ObjectIdSet : CCSet.S

module ElemIdMap : CCMap.S

module KeyMap : CCMap.S

module KeySet : CCSet.S

module OpMap : CCMap.S

module OpSetBackend : sig
  type t

  type actor = string

  type seq = int

  type obj_id = string

  type key = string

  type action = MakeMap | MakeList | MakeText | Ins | Set | Del | Link

  type op_val =
    | BoolValue of bool
    | StrValue of string

  type value = Value of op_val | Link of {obj: value}

  type op =
    { key: key
    ; action: action
    ; actor: actor
    ; seq: seq
    ; obj: obj_id
    ; elem: int option
    ; value: op_val option }

  type change_op =
    {key: key option; action: action; obj: obj_id; elem: int option; value: op_val option}

  type change = {actor: actor; seq: seq; deps: seq ActorMap.t; ops: change_op list}

  type state = {change: change; allDeps: seq ActorMap.t}

  type edit_action = Create | Insert | Remove | Set

  type edit_type = Map | Text | List

  type conflict = {actor: actor; value: op_val option; link: bool}

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

  type context = {instantiate_object: t -> obj_id -> value}

  type iterator_mode = Keys | Values | Entries | Elems | Conflicts

  type iterator_val =
    | KeyValue of int
    | ValueValue of value option
    | EntryValue of int * value option
    | ElemValue of int * string
    | ConflictValue of op OpMap.t

  type iterator_res = {done_: bool; value: iterator_val option}

  type iterator = {next: unit -> iterator_res option}

  val init : unit -> t

  val add_change : t -> change -> bool -> t * edit list

  val get_missing_changes : t -> seq ActorMap.t -> change list

  val get_changes_for_actor : t -> ?after_seq:int -> actor -> change list

  val get_missing_deps : t -> seq ActorMap.t

  val get_object_fields : t -> obj_id -> KeySet.t option

  val get_object_field : t -> obj_id -> key -> context -> value option

  val get_object_conflicts :
    t -> obj_id -> context -> (actor * value option) OpMap.t KeyMap.t option

  val get_field_ops : t -> obj_id -> key -> op list

  val list_elem_by_index : t -> obj_id -> int -> context -> value option

  val list_length : t -> obj_id -> int option

  val list_iterator : t -> obj_id -> iterator_mode -> context -> iterator

  val root_id : string

  (* Auxulary funs required to not break encapsulation between index.js and op_set.js *)
  val get_clock: t -> seq ActorMap.t
  val get_deps: t -> seq ActorMap.t
  val can_undo: t -> bool
  val can_redo: t -> bool
end
