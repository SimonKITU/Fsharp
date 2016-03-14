// Exercise 4.1
//9.1
// Exercise 4.2
//9.3
let rec sum acc = function
    | (m, 0) -> m + acc
    | (m, n) -> sum (acc+m+n) (m, (n-1));;

// Exercise 4.3
//9.4
let rec length = function
    | ([], _) -> 0
    | (x::[], l) -> l+1
    | (x::xs, l) -> length (xs, l+1);;

// Exercise 4.4
//9.6

let rec fact = function
| 0 -> 1
| n -> n * fact(n-1);;

let rec factA = function
| (0,m) -> m
| (n,m) -> factA(n-1, n*m);;

let id = (fun a -> a);;
let rec factC n c = if n = 0 then c 1 else factC (n-1) (fun a -> c (a * n));;

let xs16 = List.init 1000000 (fun i -> 16);;


for i in xs16 do let _ = fact i in ();;         // Real: 00:00:00.037, CPU: 00:00:00.046, GC gen0: 0, gen1: 0, gen2: 0

for i in xs16 do let _ = factA (i,1) in ();;    // Real: 00:00:00.022, CPU: 00:00:00.031, GC gen0: 0, gen1: 0, gen2: 0


for i in xs16 do let _ = factC i id in ();;     // Real: 00:00:00.388, CPU: 00:00:00.390, GC gen0: 85, gen1: 4, gen2: 0





// Exercise 4.5
//8.6
let fib n =
    let mutable n' = 0
    let mutable fibOld = 1
    let mutable r = 0
    while n' < n do
        let fibt = r
        r <- r + fibOld
        fibOld <- fibt
        n' <- n' + 1
    r;;

// Exercise 4.6
//9.7


let rec fibA n x y = if n > 1 then fibA (n-1) (x+y) (x) else x;;
let rec fibC n c = if n > 1 then fibC (n-1) (fun x -> fibC (n-2) ( fun a -> c a + x) ) else c 1;;
let rec fibD n c = if n > 1 then fibD (n-1) (fun x y -> c (x + y) x) else c 1 1;;

for i in xs16 do let _ = fib i in ();;                      //Real: 00:00:00.021

for i in xs16 do let _ = fibA i 1 1 in ();;                 //Real: 00:00:00.024

for i in xs16 do let _ = fibC i id in ();;                  //Real: 00:01:27.737

for i in xs16 do let _ = fibD i (fun x y -> x) in ();;      //Real: 00:00:00.613



// Exercise 4.7
//9.8
type 'a BinTree =
    | Leaf
    | Node of 'a * 'a BinTree * 'a BinTree

let intBinTree = Node(43, Node(25, Node(56,Leaf, Leaf), Leaf), Node(562, Leaf, Node(78, Node(10, Leaf, Leaf), Leaf)))

let rec countA acc tree =
    match tree with
    | Leaf -> acc
    | Node(v, treeL, treeR) -> countA (countA (acc+1) treeR) treeL

// Exercise 4.8   
//9.9
let rec countACH t a c =
    match t with
    | Node(_, Leaf, Leaf) -> c a
    | Node(_, Leaf, treeR) -> countACH treeR a (fun x -> c (a + x + 1))
    | Node(_, treeL, Leaf) -> countACH treeL a (fun x -> c (a + x + 1))
    | Node(_, treeL, treeR) -> countACH treeL a (fun x -> c (a + x + 1)) +
                               countACH treeR a (fun x -> c (a + x + 1))

let rec countAC t a c = 1 + countACH t a c

// Exercise 4.9
//9.10
//The problem is that (fun res -> 1::k(res)) is not tail-recursive.
//when making the fun tail-resursive it will not stack overflow

let rec bigListK n k =
    if n = 0 then k []
    else bigListK (n-1) (fun res -> k(1::res));;

let res = bigListK 130000 id;;


// Exercise 4.10
//9.11


// Exercise 4.11
// 11.1
let oddSeq = Seq.initInfinite (fun i -> 1 + i * 2)

// Exercise 4.12
//11.2
let factSeq = Seq.initInfinite (fun i -> factC i id)

