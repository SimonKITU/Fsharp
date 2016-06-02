// Computing slowfib(42) is CPU-intensive, ca 2 sec. Mono Mac 

let rec slowfib n = if n<2 then 1.0 else slowfib(n-1) + slowfib(n-2);;

// Don Syme examples, adapted
// Times are Mono 2.10.8 on MacOS 10.6.8 Core 2 Duo 2.66 GHz 

let fib42 = slowfib(42);;
// Real: 00:00:02.668, CPU: 00:00:02.668, GC gen0: 0

let fibs = [ slowfib(41); slowfib(42) ];;
// Real: 00:00:04.313, CPU: 00:00:04.312, GC gen0: 0

let fibs =
    let tasks = [ async { return slowfib(41) };
                  async { return slowfib(42) } ]
    Async.RunSynchronously (Async.Parallel tasks);;
// Real: 00:00:02.690, CPU: 00:00:04.274, GC gen0: 0

let fibs = [ for i in 0..42 do yield slowfib(i) ];;
// Real: 00:00:06.983, CPU: 00:00:06.979, GC gen0: 0

let fibs =
    let tasks = [ for i in 0..42 do yield async { return slowfib(i) } ]
    Async.RunSynchronously (Async.Parallel tasks);;
// Real: 00:00:04.072, CPU: 00:00:06.938, GC gen0: 0

// Dissection of this:

// async { return slowfib(i) }
// has type: Async<float>
// an asynchronous tasks that, when run, will produce a float

// let tasks = [ for i in 0..42 do yield async { return slowfib(i) } ]
// has type: Async<float> list
// a list of asynchronous tasks, each of which, when run, will produce a float

// Async.Parallel tasks
// has type: Async<float []>
// an asynchronous tasks that, when run, will produce a list of floats

// Async.RunSynchronously (Async.Parallel tasks)
// has type: float []
// a list of floating-point numbers
