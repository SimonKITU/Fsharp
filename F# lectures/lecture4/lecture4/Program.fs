
// lecture 4 exercise 1 slide 7

let rec map f = function
    | []    -> []
    | x::xs -> f x :: map f xs;;

let g = map (fun x -> x * x + 1);;

let gs xs = map (fun x -> x * x + 1) xs;;

//exercise 2 slide 8
let rec exists p = function
    | []    -> false
    | x::xs -> p x || exists p xs;;

let isMember y = exists(fun x -> x=y);;
let isMembers y xs = exists(fun x -> x=y) xs;;

//exercise 3 slide 11
let rec forall p = function
    | []    -> true
    | x::xs -> p x && forall p xs;;

let disjoint xs ys = forall(fun x -> not(isMember x ys)) xs;;

let disjoints xs = forall(fun y -> not(isMember y xs));;

let subset xs ys = forall (fun x -> isMember x ys) xs;;


//exercise 4 slide 13
let rec filter p = function
    | []     -> []
    | x::xs -> if p x then x :: filter p xs
               else filter p xs;;

let inter xs ys = filter(fun x -> isMember x ys) xs;;

//exercise 5 slide 19
let rec foldBack f xlst e =
    match xlst with
    | x::xs -> f x (foldBack f xs e)
    | []    -> e  ;;


let insert x ys = if isMember x ys then ys else x::ys;;

let union xs ys = foldBack(fun x rs -> insert x rs) xs ys;;

let unionn = foldBack(insert);;
