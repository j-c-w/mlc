(* t-compile: --dump-typecheck *)
(* Regression: Types were not properly unified in patterns. *)

fun f (x: int: 'a) (y: 'a) = ""

(* We expect both the y and the x to be unified to the same
* variable  type. *)
(* t-scan-times-2: int :typecheck *)
