
let rec fact n =
  if n < 1 then 1 else n * fact (n - 1)
in fact 5;;

let rec even n =
  if n = 0 then true else odd (n - 1)
and odd n =
  if n = 0 then false else even (n - 1);;
even 10;;
even 11;;
odd 10;;
odd 11;;

let rec f x = x
and y = 10
and mult2 x = x * 2
in mult2 (f y);;
