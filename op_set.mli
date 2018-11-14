type exn +=
    Inconsistent_reuse_of_sequence
  | Not_supported
  | Modification_of_unknown_object
  | Duplicate_list_element_id
  | Unknown_action_type
  | Missing_index_for_list_element


module ActorMap : CCMap.S
module SeqMap : CCMap.S
module ObjectIdMap : CCMap.S
module ObjectIdSet : CCSet.S
module ElemIdMap : CCMap.S
module KeyMap : CCMap.S

module OpSetBackend :
  sig
    type t
    type actor = string
    type seq = int
    type obj_id = string
    type key = string
    type action = MakeMap | MakeList | MakeText | Ins | Set | Del | Link
    type value = Value of string | Link of { obj : value; }
    type elem_id = key * value option

    type op = {
      key : key;
      action : action;
      actor : actor;
      seq : seq;
      obj : obj_id;
      elem : int;
      value : string option;
    }

    type change = {
      actor : actor;
      seq : seq;
      deps : seq ActorMap.t;
      ops : op list;
    }

    type state = { change : change; allDeps : seq ActorMap.t; }
    type edit_action = Create | Insert | Remove | Set
    type edit_type = Map | Text | List
    type conflict = { actor : actor; value : string option; link : bool; }
    type edit = {
      _type : edit_type;
      action : edit_action;
      elem_id__key : key option;
      key : string option;
      value : value option;
      obj : obj_id;
      link : bool;
      index : int option;
      conflicts : conflict list option;
      path : [ `IntPath of int | `StrPath of key ] list option;
    }
    type obj

    val init : unit -> t
    val add_change : t -> change -> bool -> t * edit list list
    val get_missing_changes : t -> seq ActorMap.t -> change list
    (* get_changes_for_actor *)
    (* get_missing_deps *)
    (* get_object_fields *)
    (* get_object_field *)
    (* get_object_conflicts *)
    val get_field_ops : t -> obj_id -> key -> op list
    (* list_elem_by_index *)
    (* list_length *)
    (* list_iterator *)
    val root_id : string
  end
