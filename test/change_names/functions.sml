(* t-compile: --dump-change-names *)

fun g x = x
fun f x = g x

(* t-scan: @[a-z]+#[a-z]+ (@[a-z]+#[a-z]+) = (@[a-z]+#[a-z]+) : change_names *)
