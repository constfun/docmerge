type exn +=
    Inconsistent_reuse_of_sequence
  | Not_supported
  | Modification_of_unknown_object
  | Duplicate_list_element_id
  | Unknown_action_type
  | Missing_index_for_list_element
module ActorMap :
  sig
    type key = CCString.t
    type 'a t = 'a Map.Make(CCString).t
    val empty : 'a t
    val is_empty : 'a t -> bool
    val mem : key -> 'a t -> bool
    val add : key -> 'a -> 'a t -> 'a t
    val singleton : key -> 'a -> 'a t
    val remove : key -> 'a t -> 'a t
    val merge :
      (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val for_all : (key -> 'a -> bool) -> 'a t -> bool
    val exists : (key -> 'a -> bool) -> 'a t -> bool
    val filter : (key -> 'a -> bool) -> 'a t -> 'a t
    val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
    val cardinal : 'a t -> int
    val bindings : 'a t -> (key * 'a) list
    val min_binding : 'a t -> key * 'a
    val max_binding : 'a t -> key * 'a
    val choose : 'a t -> key * 'a
    val split : key -> 'a t -> 'a t * 'a option * 'a t
    val find : key -> 'a t -> 'a
    val find_last : (key -> bool) -> 'a t -> key * 'a
    val find_last_opt : (key -> bool) -> 'a t -> (key * 'a) option
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
    val get : key -> 'a t -> 'a option
    val get_or : key -> 'a t -> default:'a -> 'a
    val update : key -> ('a option -> 'a option) -> 'a t -> 'a t
    val choose_opt : 'a t -> (key * 'a) option
    val min_binding_opt : 'a t -> (key * 'a) option
    val max_binding_opt : 'a t -> (key * 'a) option
    val find_opt : key -> 'a t -> 'a option
    val find_first : (key -> bool) -> 'a t -> key * 'a
    val find_first_opt : (key -> bool) -> 'a t -> (key * 'a) option
    val merge_safe :
      f:(key ->
         [ `Both of 'a * 'b | `Left of 'a | `Right of 'b ] -> 'c option) ->
      'a t -> 'b t -> 'c t
    val union : (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
    val of_seq : (key * 'a) CCMap.sequence -> 'a t
    val add_seq : 'a t -> (key * 'a) CCMap.sequence -> 'a t
    val to_seq : 'a t -> (key * 'a) CCMap.sequence
    val of_list : (key * 'a) list -> 'a t
    val add_list : 'a t -> (key * 'a) list -> 'a t
    val keys : 'a t -> key CCMap.sequence
    val values : 'a t -> 'a CCMap.sequence
    val to_list : 'a t -> (key * 'a) list
    val pp :
      ?start:string ->
      ?stop:string ->
      ?arrow:string ->
      ?sep:string ->
      key CCMap.printer -> 'a CCMap.printer -> 'a t CCMap.printer
  end
module SeqMap :
  sig
    type key = CCInt.t
    type 'a t = 'a Map.Make(CCInt).t
    val empty : 'a t
    val is_empty : 'a t -> bool
    val mem : key -> 'a t -> bool
    val add : key -> 'a -> 'a t -> 'a t
    val singleton : key -> 'a -> 'a t
    val remove : key -> 'a t -> 'a t
    val merge :
      (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val for_all : (key -> 'a -> bool) -> 'a t -> bool
    val exists : (key -> 'a -> bool) -> 'a t -> bool
    val filter : (key -> 'a -> bool) -> 'a t -> 'a t
    val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
    val cardinal : 'a t -> int
    val bindings : 'a t -> (key * 'a) list
    val min_binding : 'a t -> key * 'a
    val max_binding : 'a t -> key * 'a
    val choose : 'a t -> key * 'a
    val split : key -> 'a t -> 'a t * 'a option * 'a t
    val find : key -> 'a t -> 'a
    val find_last : (key -> bool) -> 'a t -> key * 'a
    val find_last_opt : (key -> bool) -> 'a t -> (key * 'a) option
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
    val get : key -> 'a t -> 'a option
    val get_or : key -> 'a t -> default:'a -> 'a
    val update : key -> ('a option -> 'a option) -> 'a t -> 'a t
    val choose_opt : 'a t -> (key * 'a) option
    val min_binding_opt : 'a t -> (key * 'a) option
    val max_binding_opt : 'a t -> (key * 'a) option
    val find_opt : key -> 'a t -> 'a option
    val find_first : (key -> bool) -> 'a t -> key * 'a
    val find_first_opt : (key -> bool) -> 'a t -> (key * 'a) option
    val merge_safe :
      f:(key ->
         [ `Both of 'a * 'b | `Left of 'a | `Right of 'b ] -> 'c option) ->
      'a t -> 'b t -> 'c t
    val union : (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
    val of_seq : (key * 'a) CCMap.sequence -> 'a t
    val add_seq : 'a t -> (key * 'a) CCMap.sequence -> 'a t
    val to_seq : 'a t -> (key * 'a) CCMap.sequence
    val of_list : (key * 'a) list -> 'a t
    val add_list : 'a t -> (key * 'a) list -> 'a t
    val keys : 'a t -> key CCMap.sequence
    val values : 'a t -> 'a CCMap.sequence
    val to_list : 'a t -> (key * 'a) list
    val pp :
      ?start:string ->
      ?stop:string ->
      ?arrow:string ->
      ?sep:string ->
      key CCMap.printer -> 'a CCMap.printer -> 'a t CCMap.printer
  end
module ObjectIdMap :
  sig
    type key = CCString.t
    type 'a t = 'a Map.Make(CCString).t
    val empty : 'a t
    val is_empty : 'a t -> bool
    val mem : key -> 'a t -> bool
    val add : key -> 'a -> 'a t -> 'a t
    val singleton : key -> 'a -> 'a t
    val remove : key -> 'a t -> 'a t
    val merge :
      (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val for_all : (key -> 'a -> bool) -> 'a t -> bool
    val exists : (key -> 'a -> bool) -> 'a t -> bool
    val filter : (key -> 'a -> bool) -> 'a t -> 'a t
    val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
    val cardinal : 'a t -> int
    val bindings : 'a t -> (key * 'a) list
    val min_binding : 'a t -> key * 'a
    val max_binding : 'a t -> key * 'a
    val choose : 'a t -> key * 'a
    val split : key -> 'a t -> 'a t * 'a option * 'a t
    val find : key -> 'a t -> 'a
    val find_last : (key -> bool) -> 'a t -> key * 'a
    val find_last_opt : (key -> bool) -> 'a t -> (key * 'a) option
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
    val get : key -> 'a t -> 'a option
    val get_or : key -> 'a t -> default:'a -> 'a
    val update : key -> ('a option -> 'a option) -> 'a t -> 'a t
    val choose_opt : 'a t -> (key * 'a) option
    val min_binding_opt : 'a t -> (key * 'a) option
    val max_binding_opt : 'a t -> (key * 'a) option
    val find_opt : key -> 'a t -> 'a option
    val find_first : (key -> bool) -> 'a t -> key * 'a
    val find_first_opt : (key -> bool) -> 'a t -> (key * 'a) option
    val merge_safe :
      f:(key ->
         [ `Both of 'a * 'b | `Left of 'a | `Right of 'b ] -> 'c option) ->
      'a t -> 'b t -> 'c t
    val union : (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
    val of_seq : (key * 'a) CCMap.sequence -> 'a t
    val add_seq : 'a t -> (key * 'a) CCMap.sequence -> 'a t
    val to_seq : 'a t -> (key * 'a) CCMap.sequence
    val of_list : (key * 'a) list -> 'a t
    val add_list : 'a t -> (key * 'a) list -> 'a t
    val keys : 'a t -> key CCMap.sequence
    val values : 'a t -> 'a CCMap.sequence
    val to_list : 'a t -> (key * 'a) list
    val pp :
      ?start:string ->
      ?stop:string ->
      ?arrow:string ->
      ?sep:string ->
      key CCMap.printer -> 'a CCMap.printer -> 'a t CCMap.printer
  end
module ObjectIdSet :
  sig
    type elt = CCString.t
    type t = Set.Make(CCString).t
    val empty : t
    val is_empty : t -> bool
    val mem : elt -> t -> bool
    val add : elt -> t -> t
    val singleton : elt -> t
    val remove : elt -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val diff : t -> t -> t
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val subset : t -> t -> bool
    val iter : (elt -> unit) -> t -> unit
    val map : (elt -> elt) -> t -> t
    val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val filter : (elt -> bool) -> t -> t
    val partition : (elt -> bool) -> t -> t * t
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt
    val max_elt : t -> elt
    val choose : t -> elt
    val split : elt -> t -> t * bool * t
    val find : elt -> t -> elt
    val min_elt_opt : t -> elt option
    val max_elt_opt : t -> elt option
    val choose_opt : t -> elt option
    val find_opt : elt -> t -> elt option
    val find_first : (elt -> bool) -> t -> elt
    val find_first_opt : (elt -> bool) -> t -> elt option
    val find_last : (elt -> bool) -> t -> elt
    val find_last_opt : (elt -> bool) -> t -> elt option
    val of_seq : elt CCSet.sequence -> t
    val add_seq : t -> elt CCSet.sequence -> t
    val to_seq : t -> elt CCSet.sequence
    val of_list : elt list -> t
    val add_list : t -> elt list -> t
    val to_list : t -> elt list
    val pp :
      ?start:string ->
      ?stop:string -> ?sep:string -> elt CCSet.printer -> t CCSet.printer
  end
module ElemIdMap :
  sig
    type key = CCString.t
    type 'a t = 'a Map.Make(CCString).t
    val empty : 'a t
    val is_empty : 'a t -> bool
    val mem : key -> 'a t -> bool
    val add : key -> 'a -> 'a t -> 'a t
    val singleton : key -> 'a -> 'a t
    val remove : key -> 'a t -> 'a t
    val merge :
      (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val for_all : (key -> 'a -> bool) -> 'a t -> bool
    val exists : (key -> 'a -> bool) -> 'a t -> bool
    val filter : (key -> 'a -> bool) -> 'a t -> 'a t
    val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
    val cardinal : 'a t -> int
    val bindings : 'a t -> (key * 'a) list
    val min_binding : 'a t -> key * 'a
    val max_binding : 'a t -> key * 'a
    val choose : 'a t -> key * 'a
    val split : key -> 'a t -> 'a t * 'a option * 'a t
    val find : key -> 'a t -> 'a
    val find_last : (key -> bool) -> 'a t -> key * 'a
    val find_last_opt : (key -> bool) -> 'a t -> (key * 'a) option
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
    val get : key -> 'a t -> 'a option
    val get_or : key -> 'a t -> default:'a -> 'a
    val update : key -> ('a option -> 'a option) -> 'a t -> 'a t
    val choose_opt : 'a t -> (key * 'a) option
    val min_binding_opt : 'a t -> (key * 'a) option
    val max_binding_opt : 'a t -> (key * 'a) option
    val find_opt : key -> 'a t -> 'a option
    val find_first : (key -> bool) -> 'a t -> key * 'a
    val find_first_opt : (key -> bool) -> 'a t -> (key * 'a) option
    val merge_safe :
      f:(key ->
         [ `Both of 'a * 'b | `Left of 'a | `Right of 'b ] -> 'c option) ->
      'a t -> 'b t -> 'c t
    val union : (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
    val of_seq : (key * 'a) CCMap.sequence -> 'a t
    val add_seq : 'a t -> (key * 'a) CCMap.sequence -> 'a t
    val to_seq : 'a t -> (key * 'a) CCMap.sequence
    val of_list : (key * 'a) list -> 'a t
    val add_list : 'a t -> (key * 'a) list -> 'a t
    val keys : 'a t -> key CCMap.sequence
    val values : 'a t -> 'a CCMap.sequence
    val to_list : 'a t -> (key * 'a) list
    val pp :
      ?start:string ->
      ?stop:string ->
      ?arrow:string ->
      ?sep:string ->
      key CCMap.printer -> 'a CCMap.printer -> 'a t CCMap.printer
  end
module KeyMap :
  sig
    type key = CCString.t
    type 'a t = 'a Map.Make(CCString).t
    val empty : 'a t
    val is_empty : 'a t -> bool
    val mem : key -> 'a t -> bool
    val add : key -> 'a -> 'a t -> 'a t
    val singleton : key -> 'a -> 'a t
    val remove : key -> 'a t -> 'a t
    val merge :
      (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val for_all : (key -> 'a -> bool) -> 'a t -> bool
    val exists : (key -> 'a -> bool) -> 'a t -> bool
    val filter : (key -> 'a -> bool) -> 'a t -> 'a t
    val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
    val cardinal : 'a t -> int
    val bindings : 'a t -> (key * 'a) list
    val min_binding : 'a t -> key * 'a
    val max_binding : 'a t -> key * 'a
    val choose : 'a t -> key * 'a
    val split : key -> 'a t -> 'a t * 'a option * 'a t
    val find : key -> 'a t -> 'a
    val find_last : (key -> bool) -> 'a t -> key * 'a
    val find_last_opt : (key -> bool) -> 'a t -> (key * 'a) option
    val map : ('a -> 'b) -> 'a t -> 'b t
    val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
    val get : key -> 'a t -> 'a option
    val get_or : key -> 'a t -> default:'a -> 'a
    val update : key -> ('a option -> 'a option) -> 'a t -> 'a t
    val choose_opt : 'a t -> (key * 'a) option
    val min_binding_opt : 'a t -> (key * 'a) option
    val max_binding_opt : 'a t -> (key * 'a) option
    val find_opt : key -> 'a t -> 'a option
    val find_first : (key -> bool) -> 'a t -> key * 'a
    val find_first_opt : (key -> bool) -> 'a t -> (key * 'a) option
    val merge_safe :
      f:(key ->
         [ `Both of 'a * 'b | `Left of 'a | `Right of 'b ] -> 'c option) ->
      'a t -> 'b t -> 'c t
    val union : (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
    val of_seq : (key * 'a) CCMap.sequence -> 'a t
    val add_seq : 'a t -> (key * 'a) CCMap.sequence -> 'a t
    val to_seq : 'a t -> (key * 'a) CCMap.sequence
    val of_list : (key * 'a) list -> 'a t
    val add_list : 'a t -> (key * 'a) list -> 'a t
    val keys : 'a t -> key CCMap.sequence
    val values : 'a t -> 'a CCMap.sequence
    val to_list : 'a t -> (key * 'a) list
    val pp :
      ?start:string ->
      ?stop:string ->
      ?arrow:string ->
      ?sep:string ->
      key CCMap.printer -> 'a CCMap.printer -> 'a t CCMap.printer
  end
module OpSetBackend :
  sig
    val _ROOT_ID : string
    type actor = string
    type seq = int
    type obj_id = string
    type key = string
    type action = MakeMap | MakeList | MakeText | Ins | Set | Del | Link
    type value = Value of string | Link of { obj : value; }
    type elem_id = key * value option
    module SkipList :
      sig
        type t = elem_id list
        val empty : 'a list
        val insert_index :
          int -> key -> value option -> t -> elem_id CCList.t
        val index_of : 'a -> ('a * 'b) CCList.t -> int option
        val set_value : key -> value option -> t -> elem_id CCList.t
        val remove_index : int -> t -> elem_id CCList.t
      end
    type op = {
      key : key;
      action : action;
      actor : actor;
      seq : seq;
      obj : obj_id;
      elem : int;
      value : string option;
    }
    type lamport_op = { actor : actor; elem : int; }
    module OpSet :
      sig
        type elt = op
        type t
        val empty : t
        val is_empty : t -> bool
        val mem : elt -> t -> bool
        val add : elt -> t -> t
        val singleton : elt -> t
        val remove : elt -> t -> t
        val union : t -> t -> t
        val inter : t -> t -> t
        val diff : t -> t -> t
        val compare : t -> t -> int
        val equal : t -> t -> bool
        val subset : t -> t -> bool
        val iter : (elt -> unit) -> t -> unit
        val map : (elt -> elt) -> t -> t
        val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
        val for_all : (elt -> bool) -> t -> bool
        val exists : (elt -> bool) -> t -> bool
        val filter : (elt -> bool) -> t -> t
        val partition : (elt -> bool) -> t -> t * t
        val cardinal : t -> int
        val elements : t -> elt list
        val min_elt : t -> elt
        val max_elt : t -> elt
        val choose : t -> elt
        val split : elt -> t -> t * bool * t
        val find : elt -> t -> elt
        val min_elt_opt : t -> elt option
        val max_elt_opt : t -> elt option
        val choose_opt : t -> elt option
        val find_opt : elt -> t -> elt option
        val find_first : (elt -> bool) -> t -> elt
        val find_first_opt : (elt -> bool) -> t -> elt option
        val find_last : (elt -> bool) -> t -> elt
        val find_last_opt : (elt -> bool) -> t -> elt option
        val of_seq : elt CCSet.sequence -> t
        val add_seq : t -> elt CCSet.sequence -> t
        val to_seq : t -> elt CCSet.sequence
        val of_list : elt list -> t
        val add_list : t -> elt list -> t
        val to_list : t -> elt list
        val pp :
          ?start:string ->
          ?stop:string -> ?sep:string -> elt CCSet.printer -> t CCSet.printer
      end
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
    type ref = {
      action : action;
      obj : obj_id;
      key : key;
      value : value option;
    }
    type obj_aux = {
      _max_elem : int;
      _following : op list KeyMap.t;
      _init : op;
      _inbound : OpSet.t;
      _elem_ids : SkipList.t option;
      _insertion : op ElemIdMap.t;
    }
    type obj = op list KeyMap.t * obj_aux
    type t = {
      states : state list ActorMap.t;
      history : change list;
      by_object : obj ObjectIdMap.t;
      clock : seq ActorMap.t;
      deps : seq ActorMap.t;
      undo_pos : int;
      undo_stack : ref list list;
      redo_stack : ref list list;
      queue : change CCFQueue.t;
      undo_local : ref list option;
    }
    val get_obj_aux : t -> ObjectIdMap.key -> obj_aux CCOpt.t
    val get_obj_aux_exn : t -> ObjectIdMap.key -> obj_aux
    val causaly_ready : t -> change -> bool
    val transitive_deps : t -> seq ActorMap.t -> seq ActorMap.t
    val apply_make : t -> op -> t * edit list
    val apply_insert : t -> op -> t * 'a list
    val get_conflicts : op list -> conflict list CCOpt.t
    val get_path :
      t ->
      ObjectIdMap.key ->
      ([> `IntPath of int | `StrPath of key ] as 'a) list CCOpt.t ->
      'a list CCOpt.t
    val patch_list :
      t ->
      ObjectIdMap.key ->
      int -> key -> edit_action -> op list option -> t * edit list
    val is_concurrent : t -> op -> op -> bool
    val get_field_ops : t -> 'a -> key -> op list
    val get_parent : t -> ObjectIdMap.key -> ElemIdMap.key -> key option
    val lamport_compare : lamport_op -> lamport_op -> int
    val insertions_after :
      t -> ObjectIdMap.key -> key option -> key option -> string CCList.t
    val get_previous : t -> ObjectIdMap.key -> ElemIdMap.key -> key option
    val update_list_element : t -> ObjectIdMap.key -> key -> t * edit list
    val update_map_key : t -> ObjectIdMap.key -> key -> t * edit list
    val apply_assign : t -> op -> bool -> t * edit list
    val apply_ops : t -> op list -> t * edit list
    val apply_change : t -> change -> t * edit list
    val apply_queued_ops : t -> edit list list -> t * edit list list
    val push_undo_history : t -> t
    val add_change : t -> change -> bool -> t * edit list list
    val init : unit -> t
  end
