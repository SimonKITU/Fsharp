// F# parallel computing examples inspired by Hansen and Rischel
// chapter 13.6 * sestoft@itu.dk * 2015-03-11

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

#time;;

// Array.map isPrime bigArray;;
// Not much of a win on two-core Mono MacOS.  Presumably because most
// integers have small prime factors ... eg. 2/3 of them have either 2
// or 3 as prime factor.
// Array.Parallel.map isPrime bigArray;;
// Strangely, today (2013-04-24) the parallelized version is nearly twice
// as fast...

// Better example: Prime factors of random numbers (more work)

Array.map factors bigArray;;
Array.Parallel.map factors bigArray;;

// Even better example: Prime factors of [1..200000]

Array.init 200000 factors;;
let factors200000 = Array.Parallel.init 200000 factors;;

// > Array.init 200000 factors;;
// Real: 00:00:03.043, CPU: 00:00:03.042, GC gen0: 1, gen1: 0

// > Array.Parallel.init 200000 factors;;
// Real: 00:00:01.590, CPU: 00:00:03.056, GC gen0: 1, gen1: 0

let histogram = Array.init 200000 (fun i -> 0)
let incr i = histogram.[i] <- histogram.[i] + 1
Array.iter (fun fs -> List.iter incr fs) factors200000;;
