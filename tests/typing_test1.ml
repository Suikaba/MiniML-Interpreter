
let x = 10;;
let y = x + 10;;
x + y;;

fun x -> x + 10;;

(fun f x -> f x) (fun b -> b) true;;

(fun f x -> 0 + f x) (fun b -> b);;

(fun x -> fun y -> y x) 0;;

(fun x -> fun y -> fun z -> z y x) 0 false;;

let f = (fun x y z -> x + y + z)
in f 1 2 3;;

let f = (fun x -> x < 10)
and g = (fun y -> y && true) in
f 10;;

let f x y z = if true then z y else y x in
f 10 (fun a -> false);;
