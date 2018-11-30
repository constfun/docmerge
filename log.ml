let log : string -> 'a Js.t -> unit = fun msg o ->
  Js.Unsafe.fun_call (Js.Unsafe.js_expr "console.log") [|
    Js.Unsafe.inject (Js.string ("OCaml: " ^ msg));
    Js.Unsafe.inject o;
  |]

let log_str : string -> unit = fun msg ->
  Js.Unsafe.fun_call (Js.Unsafe.js_expr "console.log") [|
    Js.Unsafe.inject (Js.string ("OCaml: " ^ msg));
  |]
