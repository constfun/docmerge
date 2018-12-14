module type OrderedType = sig
  type t

  val compare : t -> t -> int

  val sexp_of_t : t -> Sexplib.Sexp.t
end

module CCString = struct
  include CCString

  let sexp_of_t t = Sexplib.Sexp.Atom t
end

module CCInt = struct
  include CCInt

  let sexp_of_t t = Sexplib.Sexp.Atom (string_of_int t)
end

module CCMapMake (Key : OrderedType) = struct
  include CCMap.Make (Key)

  let sexp_of_t (sexp_of_value : 'a -> Sexplib.Sexp.t) (t : 'a t) =
    let open Sexplib.Sexp in
    List
      (fold
         (fun key value atm_lis ->
           List [Key.sexp_of_t key; sexp_of_value value] :: atm_lis )
         t [])
end

module CCSetMake (Key : OrderedType) = struct
  include CCSet.Make (Key)

  let sexp_of_t (t : t) =
    let open Sexplib.Sexp in
    List (fold (fun value atm_lis -> Key.sexp_of_t value :: atm_lis) t [])
end

module CCFQueueWithSexp = struct
  include CCFQueue

  let sexp_of_t (sexp_of_value : 'a -> Sexplib.Sexp.t) (t : 'a t) =
    Sexplib.Sexp.List (CCList.map sexp_of_value (CCFQueue.to_list t))
end
