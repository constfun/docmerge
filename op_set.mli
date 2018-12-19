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

  type op_val = BoolValue of bool | StrValue of string | NumberValue of float

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
    { key: key option
    ; action: action
    ; obj: obj_id
    ; elem: int option
    ; value: op_val option }

  type change =
    {actor: actor; seq: seq; deps: seq ActorMap.t; ops: change_op list}

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

  val init : unit -> t

  val add_change : t -> change -> bool -> t * edit list

  val get_missing_changes : t -> seq ActorMap.t -> change list

  val get_changes_for_actor : t -> ?after_seq:int -> actor -> change list

  val get_missing_deps : t -> seq ActorMap.t

  val get_field_ops : t -> obj_id -> key -> op list

  val list_length : t -> obj_id -> int option

  val root_id : string

  type diff_type = DiffMap | DiffList | DiffText

  type diff_action = DiffCreate | DiffSet

  type diff =
    { obj: string
    ; type_: diff_type
    ; action: diff_action
    ; key: key option
    ; value: op_val option
    ; link: bool
    ; conflicts: conflict list option }

  type patch = {can_undo: bool; diffs: diff list}

  val get_patch : t -> patch

  (* Auxulary funs required to not break encapsulation between index.js and op_set.js *)

  val get_clock : t -> seq ActorMap.t

  val get_deps : t -> seq ActorMap.t

  val can_undo : t -> bool

  val can_redo : t -> bool
end
