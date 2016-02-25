type 'a BinTree =
    | Leaf
    | Node of 'a * 'a BinTree * 'a BinTree;;


// Exercise 3.1
let rec inOrder = function
    | Leaf -> []
    | Node(x, t1, t2) -> inOrder t1 @ x :: inOrder t2;;

// Exercise 3.2
let rec mapInOrder f = function
    | Leaf -> Leaf
    | Node(x, t1, t2) -> Node(f x, mapInOrder f t1, mapInOrder f t2);;

// Exercise 3.3
let foldInOrder f n t = List.fold f n (inOrder t);;

let floatBinTree = Node(43.0,Node(25.0, Node(56.0,Leaf, Leaf), Leaf), Node(562.0, Leaf, Node(78.0, Leaf,Leaf)));;

// foldInOrder (fun n a -> a + n) 0.0 floatBinTree;;
// = 764.0

// Exercise 3.4, 3.5 and 3.6
let update x v s = Map.add x v s;;

// Arithmetic expressions
type aExp = 
    | N of int
    | V of string
    | Add of aExp * aExp
    | Mul of aExp * aExp
    | Sub of aExp * aExp;;

let rec A a s =
    match a with
    | N n -> n
    | V x -> Map.find x s
    | Add(a1, a2) -> A a1 s + A a2 s
    | Mul(a1, a2) -> A a1 s * A a2 s
    | Sub(a1, a2) -> A a1 s - A a2 s;;

// Bool expressions
type bExp =
    | TT
    | FF
    | Eq of aExp * aExp
    | Lt of aExp * aExp
    | Neg of bExp
    | Con of bExp * bExp;;

let rec B b s =
    match b with
    | TT -> true
    | FF -> false
    | Eq(b1, b2) -> (A b1 s) = (A b2 s)
    | Lt(b1, b2) -> (A b1 s) < (A b2 s)
    | Neg(b) -> not (B b s)
    | Con(b1, b2) -> (B b1 s) && (B b2 s);;

// Stmts
type stm =
    | Ass of string * aExp
    | Skip
    | Seq of stm * stm
    | ITE of bExp * stm * stm
    | While of bExp * stm
    | IT of bExp * stm
    | RepeatU of stm * bExp
    | Inc of string;;

let rec I stm s =
    match stm with
    | Ass(x, a) -> update x (A a s) s
    | Skip -> s
    | Seq(stm1, stm2) -> I stm2 (I stm1 s)
    | ITE(b, stm1, stm2) -> if B b s then I stm1 s else I stm2 s
    | While(b, stm1) -> if B b s then I (Seq(stm1, stm)) s else s
    | IT(b, stm1) -> if B b s then I stm1 s else s
    | RepeatU(stm1, b) -> I (While(Neg(b), stm1)) s
    | Inc(x) -> I (Ass(x, Add(V x, N 1))) s;;

let m = Map.ofList[("x", 4); ("y", 0)];;

let e1 = Seq(Ass("x", N 6), ITE(Neg(Eq(V "x", N 6)), Ass("x", N 6), Ass("x", N 3)));;
let e2 = Seq(Ass("y", N 1), While(Lt(V "y", V "x"), Ass("y", Add(V "y", N 1))));;
let e3 = ITE(Neg(Eq(V "x", N 0)), Ass("x", N 6), Ass("x", Mul(V "x", V "y")));;
let e4 = Seq(RepeatU(Inc("x"), Eq(V "x", N 14)), Ass("y", Add(V "x", N 6)));;
let e5 = IT(Lt(V "y", V "x"), Ass("y", N 6));;

let r1 = I e1 m;;
let r2 = I e2 m;;
let r3 = I e3 m;;
let r4 = I e4 m;;
let r5 = I e5 m;;

// Results
(*
val r1 : Map<string,int> = map [("x", 3); ("y", 0)]


val r2 : Map<string,int> = map [("x", 4); ("y", 4)]


val r3 : Map<string,int> = map [("x", 6); ("y", 0)]


val r4 : Map<string,int> = map [("x", 14); ("y", 20)]


val r5 : Map<string,int> = map [("x", 4); ("y", 6)]
*)


// Exercise 3.7
// 6.2
type Fexpr =
    | Const of float
    | X
    | Add of Fexpr * Fexpr
    | Sub of Fexpr * Fexpr
    | Mul of Fexpr * Fexpr
    | Div of Fexpr * Fexpr
    | Sin of Fexpr
    | Cos of Fexpr
    | Log of Fexpr
    | Exp of Fexpr;;
    
let rec pff = function
    | Const(x) -> x.ToString()
    | X -> "X"
    | Add(x, y) -> pff x + " " + pff y + " +"
    | Sub(x, y) -> pff x + " " + pff y + " -"
    | Mul(x, y) -> pff x + " " + pff y + " *"
    | Div(x, y) -> pff x + " " + pff y + " /"
    | Sin(x) -> pff x + " sin"
    | Cos(x) -> pff x + " cos"
    | Log(x) -> pff x + " log"
    | Exp(x) -> pff x + " exp";;

// Exercise 3.8
// 6.8
type Instruction = 
    | ADD
    | SUB
    | MULT
    | DIV
    | SIN
    | COS
    | LOG
    | EXP
    | PUSH of float;;

type Stack = List<float>;;

let intpInstr (s:Stack) i:Stack = 
    match s, i with 
    | x::y::xs, ADD -> (y + x)::xs
    | x::y::xs, SUB -> (y - x)::xs
    | x::y::xs, MULT -> (y * x)::xs
    | x::y::xs, DIV -> (y / x)::xs
    | x::xs, SIN -> System.Math.Sin(x)::xs
    | x::xs, COS -> System.Math.Cos(x)::xs
    | x::xs, LOG -> System.Math.Log(x)::xs
    | x::xs, EXP -> System.Math.Exp(x)::xs
    | xs, PUSH(x) -> x::xs
    | xs, _ -> xs;;

// ..........