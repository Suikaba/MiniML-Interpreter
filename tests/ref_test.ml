
let counter =
  let r = ref 0 in
  let body dummy = r := !r + 1; !r in
  body;;

counter ();;
counter ();;
counter ();;

let ri = ref 0
and rb = ref true
and rf = ref (fun x -> x);;

ri := 1; !ri;;
rb := false; !rb;;
!rf !ri;;
!rf !rb;; (* todo: must be type error *)
rf := (fun x -> x + 10);;
!rf 1;;
!rf true;; (* runtime error *)

