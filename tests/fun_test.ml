
(fun x -> x + 2) 1;;

(fun id -> fun x -> id x) (fun x -> x) 0;;

let f =
  let x = 2 in
  let addx = fun y -> x + y in
  addx
in f 4;;

let f = fun x y z -> x + y + z in
f 2 3 4;;
