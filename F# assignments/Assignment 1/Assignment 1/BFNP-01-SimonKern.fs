// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

//Exercise 1.1

let sqr x = x*x;;

//Exercise 1.2

let pow x n = System.Math.Pow(x,n);;

//Exercise 1.3
//1.1
let g n = n+4;;
//Exercise 1.4
//1.2
let h (x,y) = System.Math.Sqrt((x*x)+(y*y));;
//Exercise 1.5
//1.4
let rec f = function
| 0 -> 0
| n -> n + f(n-1);; 

(* rec from
    0! = 0  (i)
    n! = n + (n-1) + n (ii)

eval
f(4)
   4 + f(4-1)               
   4 + f(3)                 
   4 + 3 + f(3-1)           
   4 + 3 + f(2)             
   4 + 3 + 2 + f(2-1)       
   4 + 3 + 2 + f(1)         
   4 + 3 + 2 + 1 + f(1-1)   
   4 + 3 + 2 + 1 + f(0)     
   4 + 3 + 2 + 1 + 0        
   10

*)

//Exercise 1.6
//1.5
let rec fb = function
| 0 -> 0
| 1 -> 1
| n -> fb(n-1) + fb(n-2);; 


(* eval
fb(4)
fb(4-1) + fb(4-2)
fb(3-1) + fb(3-2) + fb(2-1) + fb(2-2)
fb(2-1) + fb(2-2) + 1 + 1 + 0
1 + 0 + 1 + 1 + 0
3
*)

//
//Exercise 1.7
//1.6
let rec sum = function
| (m,0) -> m
| (m,n) -> m + sum(m,n-1);;

(*
rec form
m,0 - > m   (i)
m,n -> (m+n) + sum(m,n-1) (ii)

*)

//Exercise 1.8
//1.7

(* 
(System.Math.PI, fact -1)       //float * int
fact(fact 4)                    // int
power(System.Math.PI, fact 2)   // float
(power, fact)                   // float * int
*)

//Exercise 1.9
//1.8

(* Environment:
a -> 5
f a -> a + 1
g b -> (f b) + a

Evaluation

f 3
3 + 1
4

g 3
(f 3) + 5
4 + 5
9



*)
//Exercise 1.10
let dup s:string = s + s;;

//Exercise 1.11
let rec dupn (s:string) (n:int) = 
    match n with
    | 1 -> s
    | _ -> s + (dupn s (n-1));;
 
//Exercise 1.12
let timediff (x,n) (y,m) = ((y*60) - (x*60)) + (m-n);;

//Exercise 1.13
let minutes (x,n) = timediff (0,0) (x,n);;
//Exercise 1.14
//2.2
let rec pow2(s:string,n:int) = 
    match n with
    | 1 -> s
    | _ -> s + pow2(s,n-1);;
//Exercise 1.15
//2.8
let rec bin = function
    | (n,0) -> 1
    | (n,k) when k = n -> 1
    | (n,k) -> bin(n-1, k-1) + bin(n-1, k);;

//Exercise 1.16
//2.9
(*
1. int * int -> int
2. x >= 0
3. 
    f(2,3)
    f(2-1,2*3)
    f(1,6)
    f(1-1,1*6)
    f(0,6)
    6
4. f is a fuction that takes the pair argument (x,y)
*)

//Exercise 1.17
//2.10
let test(c,e) = if c then e else 0;;

(*
1. bool * int -> int
2. jeg får en stackoverflowexception, men forstår ikke helt hvorfor..
går udfra at opgaven skal vise at den ikke evaluere fact -1 hvis c er false
3. resultate giver 0 som forventet 
*)
//Exercise 1.18
//2.13
let curry f = fun a -> fun b -> f(a,b);;

let uncurry g = fun (a,b) -> g a b;; 