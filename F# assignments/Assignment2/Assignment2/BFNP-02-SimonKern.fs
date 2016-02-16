//Exercise 2.1
let rec downTo n = if n = 0 then [] else n :: downTo (n-1);;

let rec downTot = function
    | 0 -> []
    | n -> n :: downTot(n-1);;
//Exercise 2.2
let rec removeOddIdx = function
    | x :: y :: xs -> x :: removeOddIdx xs
    | xs -> xs
    | _ -> [];;
//Exercise 2.3
let rec combinePair = function
    | x :: y :: xs -> (x,y) :: combinePair xs
    | _ -> [];;
//Exercise 2.4
//3.2 (*
let rec rates = function
    | (P, s, ps) when ps >= 12 -> rates (P, s + 1, ps - 12)
    | (P, s, ps) when ps <= -12 -> rates (P, s - 1, ps + 12)
    | (P, s, ps) when s >= 20 -> rates (P + 1, s - 20, ps)
    | (P, s, ps) when s <= -20 -> rates (P - 1, s + 20, ps)
    | (P, s, ps) -> (P, s, ps);;

let convertToPence (P, s, ps) = (P * 20 + s) * 12 + ps;;

let (.+.) (x, y, z) (w, v, q) = rates(0, 0, convertToPence(x, y, z) + convertToPence(w, v, q));;

let (.-.) (x, y, z) (w, v, q) = rates(0, 0, convertToPence(x, y, z) - convertToPence(w, v, q));;

//Exercise 2.5
//3.3
let (.++.) (a,b) (c,d) = (a+c, b+d);;
let (.*.) (a, b) (c, d) = (a * c - b * d, b * c + a * d);;
let (.--.) (a, b) = (-a, -b);; 
let (.//.) a b = (a/(a**2.0 + b**2.0), -b/(a**2.0 + b**2.0));;
let (./.) (a,b) (c,d) = (a,b) .*. (c .//. d);; 
//Exercise 2.6
//4.4
let rec altsum = function
    | [] -> 0
    | x :: xs -> x + (altsum xs * -1);;
//Exercise 2.7
let explode (s:string) = List.ofArray(s.ToCharArray());;

let rec explode2 = function
    | "" -> []
    | (s:string) -> s.[0] :: explode2 (s.Remove(0,1));; 
//Exercise 2.8
let implode l = List.foldBack(fun x xs -> (x : char).ToString() + xs) l "";;

let implodeRev l = List.fold (fun xs x -> (x : char).ToString() + xs) "" (l : list<char>);;
//Exercise 2.9
let toUpper (s:string) = implode (List.map (fun c -> System.Char.ToUpper(c)) (explode s));;
let toUpper1 = explode >> List.map (fun c -> System.Char.ToUpper(c)) >> implode;;
let toUpper2 (s:string) =  explode s |> (implode << List.map(fun c -> System.Char.ToUpper(c));;
//Exercise 2.10
let palindrome (s:string) = s.ToLower().Equals(implodeRev <| explode (s.ToLower()));;
//Exercise 2.11
let rec ack = function
    | (m,n) when m = 0 -> n+1
    | (m,n) when m > 0 && n = 0 -> ack (m-1,1)
    | (m,n) when m > 0 && n > 0-> ack (m-1,ack(m,n-1));;

// ack (3,11) = 16381

//Exercise 2.12
let time f =
    let start = System.DateTime.Now in
    let res = f () in
    let finish = System.DateTime.Now in
    (res, (finish - start));;

// time (fun () -> ack(3,11)) = (16381, 00:00:00.4856688)

let timeArg1 f a = time (fun () -> f a);;

//Exercise 2.13
//5.4
