// Compile library: fsc -a Vector.fsi Vector.fs
// Compile library: fsc -a Vector.fsi VectorTypeExtension.fs

#r @"/Users/nielshallenberg/Dropbox/Documents/Work/ITU/Course/BFNP-F2014/Lectures/Lec06/Vector/Vector.dll"

#r @"/Users/nielshallenberg/Dropbox/Documents/Work/ITU/Course/BFNP-F2014/Lectures/Lec06/Vector/VectorTypeExtension.dll"


open Vector

let a = make(1.0,-2.0)

let b = make(3.0,4.0)

let c = 2.0 * a - b

coord c 

let d = c * a

let e = norm b

a.ToString()

let a = make(1.0,2.0)

string(a+a)
