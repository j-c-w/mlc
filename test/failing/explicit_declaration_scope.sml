(* t-compile: *)
(* t-fail: *)

(* This was taken from the PolyML testsuite. It originally failed *)
val x = let val id = (fn z => z): 'a -> 'a in id 2; id true end

(* This should fail. *)
(* val x = (let val id = (fn z => z): 'a -> 'a in id id end; (fn z => z): 'a) *)
