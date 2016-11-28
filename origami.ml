type point = float * float
;;

type kartka = point -> int
;;

let prostokat p1 p2 =
    let xl = fst p1 and yl = snd p1 in
    let xr = fst p2 and yr = snd p2 in
    fun (x, y) ->
        if (x >= xl && x <= xr && y >= yl && y <= yr) then 1
        else 0
;;
let kolko p r =

;;

let zloz p1 p2 k = fun (x, y) -> 1

;;

let skladaj pl k = fun (x, y) -> 1

;;