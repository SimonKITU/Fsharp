// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

  //1.1
  let g n = n + 4;;

  //1.2
  let h (x,y) = System.Math.Sqrt(x ** 2.0 + y ** 2.0);;

  //1.3
  fun fn -> fn + 4;;

  fun (x,y) -> System.Math.Sqrt(x ** 2.0 + y ** 2.0);;

  //1.4
  let rec f = function
  | 0 -> 0
  | n -> n + f(n - 1);;

  (*
  Rec formula:
  0 = 0
  n = n + (n - 1) for n > 0
  
  Eval:
  f(4)
  4 + f(4 - 1)
  4 + f(3)
  4 + 3 + f(3 - 1)
  4 + 3 + f(2)
  4 + 3 + 2 + f(2 - 1)
  4 + 3 + 2 + f(1)
  4 + 3 + 2 + 1 + f(1 - 1)
  4 + 3 + 2 + 1 + f(0)
  4 + 3 + 2 + 1 + 0
  10
  
  *)

  //1.5
  let rec fib = function
  | 0 -> 0
  | 1 -> 1
  | n -> fib(n - 1) + fib(n - 2);;

  (*
  Eval:
  fib(4)
  fib(4 - 1) + fib(4 - 2)
  fib(3 - 1) + fib(3 - 2) + fib(2 - 1) + fib(2 - 2) 
  fib(2 - 1) + fib(2 - 2) + 1 + 1 + 0
  1 + 0 + 1 + 1 + 0
  3
  
  *)

  //1.6
  let rec sum = function
  | (m, 0) -> m
  | (m, n) -> (m + n) + sum(m, n-1);;

  (*
  Rec Form:
  (m,0) = m
  (m, n) = (m + n) + sun(m, n - 1)  for n > 0
  *)

  //1.7
  (*  Determine types:
        (System.Math.PI, fact -1)       = float * int
        fact(fact 4)                    = int
        power(System.Math.PI, fact 2)   = float
        (power, fact)                   = float * int
 *)

 //1.8
 (*
 Env:
 a -> 5
 f a -> a + 1
 g b -> (f b) + a

 Eval
 f 3
 3 + 1
 4

 g 3
 (f 3) + 5
 4 + 5
 9
 *)