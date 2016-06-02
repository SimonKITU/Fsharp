#r @"/Users/nielshallenberg/Dropbox/Documents/Work/ITU/Course/BFNP-F2013/Lectures/Source/Chapter7/Queue/Queue.dll" 

let q0 = Queue.empty

let q0 = Queue.empty : Queue.Queue<int>

let q1 = Queue.put 1 q0

let q2 = Queue.put 2 q1

let (x,q3) = Queue.get q2

let q4 = Queue.put 4 q3

let (x2,q5) = Queue.get q4

let (x3, q6) = Queue.get q5

(* Equality does not work *)
let qnew = Queue.put 2 q0
let _ = qnew = q3
