// Assignment 07
//Exercise 1
let rec slowfib n = if n<2 then 1.0 else slowfib(n-1) + slowfib(n-2);;

// Don Syme examples, adapted
// Windows 10 x64, Intel i5-3570k @ 3.40 GHz 

let fib42 = slowfib(42);;
// Real: 00:00:01.899, CPU: 00:00:01.906, GC gen0: 0, gen1: 0, gen2: 0

let fibs = [ slowfib(41); slowfib(42) ];;
// Real: 00:00:03.073, CPU: 00:00:03.062, GC gen0: 0, gen1: 0, gen2: 0

let fibs =
    let tasks = [ async { return slowfib(41) };
                  async { return slowfib(42) } ]
    Async.RunSynchronously (Async.Parallel tasks);;
// Real: 00:00:01.999, CPU: 00:00:03.125, GC gen0: 0, gen1: 0, gen2: 0

let fibs = [ for i in 0..42 do yield slowfib(i) ];;
// Real: 00:00:04.988, CPU: 00:00:05.031, GC gen0: 0, gen1: 0, gen2: 0

let fibs =
    let tasks = [ for i in 0..42 do yield async { return slowfib(i) } ]
    Async.RunSynchronously (Async.Parallel tasks);;
// Real: 00:00:02.368, CPU: 00:00:04.984, GC gen0: 1, gen1: 0, gen2: 0

//Exercise 2
let isPrime n =
    let rec testDiv a = a*a > n || n % a <> 0 && testDiv (a+1)
    n>=2 && testDiv 2;;

let factors n =
    let rec factorsIn d m =
        if m <= 1 then []
        else if m % d = 0 then d :: factorsIn d (m/d) else factorsIn (d+1) m
    factorsIn 2 n;;

let random n =
    let generator = new System.Random ()
    fun () -> generator.Next n;;

let r10000 = random 10000;; // 150x faster than creating a new System.Random

let rec ntimes (f : unit -> 'a) n =
    if n=0 then () else (ignore (f ()); ntimes f (n-1));;
    
let bigArray = Array.init 500000 (fun _ -> r10000 ());;


Array.map factors bigArray;;
// Real: 00:00:01.710, CPU: 00:00:01.687, GC gen0: 4, gen1: 3, gen2: 0

Array.Parallel.map factors bigArray;;
// Real: 00:00:00.681, CPU: 00:00:01.890, GC gen0: 5, gen1: 4, gen2: 1

Array.init 200000 factors;;
// Real: 00:00:09.091, CPU: 00:00:09.125, GC gen0: 4, gen1: 3, gen2: 1

let factors200000 = Array.Parallel.init 200000 factors;;
// Real: 00:00:02.843, CPU: 00:00:08.921, GC gen0: 3, gen1: 1, gen2: 0

//Exercise 3

let histogram = Array.init 200000 (fun i -> 0)
let incr i = histogram.[i] <- histogram.[i] + 1
Array.iter (fun fs -> List.iter incr fs) factors200000;;

let y = factors200000 |> Seq.map(fun e -> e |> Seq.countBy id) |> Seq.concat |> Seq.fold(fun (m:Map<int,int>) (e, v)-> if(m.ContainsKey(e)) then m.Add(e, m.[e] + v) else m.Add(e, v)) Map.empty;;

//Exercise 4

let isPrime n =
    let rec testDiv a = a*a > n || n % a <> 0 && testDiv (a+1)
    n>=2 && testDiv 2;;

let num = Array.Parallel.init 10000000 (fun i -> i);;
let count = (Array.Parallel.choose (fun i -> if isPrime i then Some i else None) num).Length;;

