(* t-compile: *)
(* This should fail.  The ASTChagneNames pass needs to be updated to handle
* this.  *)

exception a of string

fun f a = 1
