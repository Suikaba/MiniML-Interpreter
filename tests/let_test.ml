
let add2 x = x + 2;;
let plus x y = x + y;;
let twotimesapply f x = f (f x);;

add2 0;;
plus 1 2;;
twotimesapply add2 0;;

