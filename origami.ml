type point = float * float
;;

type kartka = point -> int
;;

let (<=.) x y =
    x -. 1e-12 <= y
;;

let (>=.) x y = 
    y -. 1e-12 <= x
;;

let (=.) x y =
    x <=. y && y <=. x
;;

let prostokat p1 p2 =
    let xl = fst p1 and yl = snd p1 in
    let xr = fst p2 and yr = snd p2 in
    fun (x, y) ->
        if (x >=. xl && x <=. xr && y >=. yl && y <=. yr) then 1
        else 0
;;
let kolko p r =
    let xo = fst p in
    let yo = fst p in
    let square n = n *. n in
    fun (x, y) ->
        if square (x -. xo) +. square (y -. yo) <=. square r then 1
        else 0
;;

let zloz p1 p2 k =
    let find_ln (x1, y1) (x2, y2) =
        ((y2 -. y1) /. (x2 -. x1),
        ((-1.) *. x1 *. (y2 -. y1) /. (x2 -. x1)) +. y1) in
    let find_lnp a (x, y) =
        ((-1.) /. a, (x *. (1. /. a)) +. y) in
    let find_d (a1, b1) (a2, b2) = 
        ((b2 -. b1) /. (a1 -. a2),
        (a1 *. (b2 -. b1) /. (a1 -. a2)) +. b1) in
    let find_s (xc, yc) (xd, yd) =
        ((2. *. xd) -. xc, (2. *. yd) -. yc) in
    let symmetry (x, y) =
        if fst p1 =. fst p2 || snd p1 =. snd p2 then
            if fst p1 =. fst p2 then
                find_s (x, y) (fst p1, y)
            else find_s (x, y) (x, snd p1)
        else
            let ln = find_ln p1 p2 in
            let lnp = find_lnp (fst ln) (x, y) in
            let d = find_d ln lnp in
            find_s (x, y) d
        in
    let position p1 p2 p3 =
        let x1 = fst p1 and y1 = snd p1 in
        let x2 = fst p2 and y2 = snd p2 in
        let x3 = fst p3 and y3 = snd p3 in
        let det =
            (x1 *. y2) +. (x2 *. y3) +. (x3 *. y1)
            -. (y1 *. x2) -. (y2 *. x3) -. (y3 *. x1) in
        if det =. 0. then 0
        else if det >=. 0. then 1
        else -1
        in
    fun (x, y) ->
        match position p1 p2 (x, y) with
        | -1 -> 0
        | 0 -> k (x, y)
        | _ -> ((k (x, y) + k (symmetry (x, y))))
;;

let skladaj pl k =
    List.fold_left (fun krt (x, y) ->
        zloz x y krt) k pl
;;
