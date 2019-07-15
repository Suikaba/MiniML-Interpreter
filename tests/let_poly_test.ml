
let f x = let g y = x = y in
          let z = x + 10 in
          g 0;;

let rec ntimes_apply f x n =
  if n = 0 then x
  else f (ntimes_apply f x (n - 1));;

ntimes_apply (fun x -> x + 1) 0 10;;
ntimes_apply not true 3;;
(ntimes_apply (fun x -> x + 1) 0 10) < 100 && ntimes_apply not true 10;;

let rec ntimes_apply f x n =
  if n = 0 then x
  else f (ntimes_apply f x (n - 1))
in (ntimes_apply (fun x -> x + 1) 0 10) < 10 || ntimes_apply not true 9;;
