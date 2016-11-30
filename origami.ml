(*
    Autor: Jakub Wróblewski, 386401, Gr. 5
    Recenzent: Marek Smolarczyk, 386007, Gr. 5
*)

(* Typ reprezentujący punkt *)
type point = float * float
;;

(* Funkcja zwracająca ilość przebitych kartek w danym punkcie *)
type kartka = point -> int
;;

(* Funkcje porównywania liczb z uwzględnieniem niedokładności floatów *)
let (<=.) x y =
    x -. 1e-12 <= y
;;

let (>=.) x y = 
    y -. 1e-12 <= x
;;

let (=.) x y =
    x <=. y && y <=. x
;;

(* funkcja zwracająca kartkę w kształcie prostokątu o
lewym dolnym rogu w punkcie p1 oraz prawym górnym rogu w punkie p2 *)
let prostokat p1 p2 =
    let xl = fst p1 and yl = snd p1 in
    let xr = fst p2 and yr = snd p2 in
    fun (x, y) ->
        if x >=. xl && x <=. xr && y >=. yl && y <=. yr then 1
        else 0
;;

(* funkcja zwracająca kartkę w kształcie koła o
środku w punkcie p i promieniu r *)
let kolko p r =
    let xo = fst p
    and yo = snd p
    and square n = n *. n in
    fun (x, y) ->
        if square (x -. xo) +. square (y -. yo) <=. square r then 1
        else 0
;;

(* funkcja składająca kartkę wzdłuż prostej wyznaczonej przez punkty p1 i p2 *)
let zloz p1 p2 k =
    (* funkcja znajdująca prostą przechodzącą przez punkty p1 i p2 *)
    (* zwraca parę (a, b) z postaci y = ax + b *)
    let find_ln (x1, y1) (x2, y2) =
        ((y2 -. y1) /. (x2 -. x1),
        ((-1.) *. x1 *. (y2 -. y1) /. (x2 -. x1)) +. y1)
    (* funkcja znajdująca prostą prostopadłą do poprzedniej prostej *)
    (* również zwraca parę (a, b) z postaci y = ax +b *)
    and find_lnp a (x, y) =
        (-1.) /. a, (x /. a) +. y
    (* funkcja znajdująca punkt przecięcia dwóch poprzednich prostych *)
    (* zwraca parę (x, y) czyli współrzędne punktu *)
    let find_d (a1, b1) (a2, b2) =
        ((b2 -. b1) /. (a1 -. a2),
        (a1 *. (b2 -. b1) /. (a1 -. a2)) +. b1)
    (* funkcja znajdująca punkt symetryczny do danego punktu względem
    prostej przechodzącej przez punkty p1 i p2 *)
    (* zwraca parę (x, y) czyli współrzędne punktu *)
    and find_s (xc, yc) (xd, yd) =
        ((2. *. xd) -. xc, (2. *. yd) -. yc) in
    (* funkcja pomocnicza znajdująca punkt symetryczny*)
    let symmetry (x, y) =
        if fst p1 =. fst p2 || snd p1 =. snd p2 then
            if fst p1 =. fst p2 then
                find_s (x, y) (fst p1, y)
            else 
                find_s (x, y) (x, snd p1)
        else
            let ln = find_ln p1 p2 in
            let lnp = find_lnp (fst ln) (x, y) in
            let d = find_d ln lnp in
            find_s (x, y) d
    (* funkcja określająca pozycję punktu p3 względem
    prostej wyznaczonej przez punkty p1 i p2 *)
    (* det > 0 - p3 znajduje się na lewo od prostej p1 p2 *)
    (* det = 0 - p3 znajduje się na prostej p1 p2 *)
    (* det < 0 - p3 znajduje się na prawo od prostej p1 p2 *)
    and position p1 p2 p3 =
        let x1 = fst p1 and y1 = snd p1
        and x2 = fst p2 and y2 = snd p2
        and x3 = fst p3 and y3 = snd p3 in
        let det =
            (x1 *. y2) +. (x2 *. y3) +. (x3 *. y1)
            -. (y1 *. x2) -. (y2 *. x3) -. (y3 *. x1) in
        if det =. 0. then 0
        else if det >=. 0. then 1
        else -1
    in
    (* funkcja zwracająca ilość przebić *)
    fun p ->
        match position p1 p2 p with
        | -1 -> 0
        | 0 -> k p
        | _ -> k p + k (symmetry p)
;;

let skladaj pl k =
    List.fold_left (fun krt (x, y) -> zloz x y krt) k pl
;;

(*
let a = prostokat (1.0, 1.0) (3.0, 3.0);;

assert(a (1.0, 1.0) = 1);;
assert(a (3.0, 3.0) = 1);;
assert(a (1.0, 3.0) = 1);;
assert(a (3.0, 1.0) = 1);;
assert(a (1.5, 1.5) = 1);;
assert(a (2.0, 2.0) = 1);;
assert(a (0.0, 0.0) = 0);;
assert(a (4.0, 4.0) = 0);;
assert(a (2.0, -1.) = 0);;

let kolko1 = kolko (5.0, 5.0) 2.0;;

assert(kolko1 (3.0, 5.0) = 1);;
assert(kolko1 (7.0, 5.0) = 1);;
assert(kolko1 (5.0, 7.0) = 1);;
assert(kolko1 (5.0, 3.0) = 1);;
assert(kolko1 (5.0, 5.0) = 1);;
assert(kolko1 (6.0, 5.5) = 1);;
assert(kolko1 (1.0, 1.0) = 0);;
assert(kolko1 (6.0, 9.0) = 0);;

let b = prostokat ((-9.), (-7.)) (13., 6.);;

assert(b ((-9.), (-7.)) = 1);;
assert(b (13., 6.) = 1);;
assert(b (0., 0.) = 1);;
assert(b (-10., 0.) = 0);;
assert(b ((-2.), (-7.)) = 1);;
assert(b ((-9.), (-8.)) = 0);;

let na_pol = zloz (2., -4.) (2., 10.) b;;

assert(na_pol (13., 6.) = 0);;
assert(na_pol (-9., -7.) = 2);;
assert(na_pol (-9., 0.) = 2);;
assert(na_pol (2., 4.) = 1);;
assert(na_pol (2., 10.) = 0);;
assert(na_pol (2., -7.) = 1);;
assert(na_pol (0., 0.) = 2);;
assert(na_pol (-10., -2.) = 0);;
assert(na_pol (3., 0.) = 0);;

let p1 = prostokat (0., 0.) (10., 10.);;
let l1 = [((0., 0.), (10., 10.));
          ((5., 0.), (10., 5.))];;

let p2 = skladaj l1 p1;;

assert((p2 (5., 5.)) = 1);;

let l1 = [((0., 0.), (10., 10.));
          ((5., 0.), (10., 5.));
          ((10., 0.), (0., 10.));
          ((2.5, 0.), (2.5, 10.))];;

let p2 = skladaj l1 p1;;

assert((p2 (7., 3.)) = 0);;
assert((p2 (5., 8.)) = 0);;
assert((p2 (3., 5.)) = 0);;
assert((p2 (5., 5.)) = 0);;
assert((p2 (0., 0.)) = 2);;
assert((p2 (0., 10.)) = 2);;
assert((p2 (2.5, 2.5)) = 2);;
assert((p2 (2.5, 7.5)) = 2);;
assert((p2 (2.5, 5.)) = 4);;
assert((p2 (0., 5.)) = 5);;
print_int (p2 (0., 0.));;
print_int (p2 (0., 10.));;
print_int (p2 (2.5, 2.5));;
print_int (p2 (2.5, 7.5));;
print_int (p2 (2.5, 5.));;
print_int (p2 (0., 5.));;
print_int (p2 (1., 2.));;
assert((p2 (1., 2.)) = 4);;
assert((p2 (1., 5.)) = 8);;
assert((p2 (1., 8.)) = 4);;

let p1 = prostokat (0., 0.) (10., 10.);;

let p2 = zloz (5., 0.) (5., 10.) p1;;

assert((p2 (2.5, 2.5)) = 2);;

let p2 = zloz (5., 5.) (0., 5.) p2;;

assert((p2 (2.5, 2.5)) = 4);;
*)
