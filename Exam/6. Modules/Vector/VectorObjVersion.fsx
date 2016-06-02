// Code from Hansen and Rischel: Functional Programming using F#     16/12 2012
// Chapter 7: Modules

// From Section 7.5 Classes and objects 

// An OO-based vector type

type ObjVector(X: float, Y: float) =
    member v.x = X
    member v.y = Y
    member v.coord() = (v.x, v.y)
    member v.norm() = sqrt(v.x * v.x + v.y * v.y)
    static member (~-) (v: ObjVector) = ObjVector(- v.x,- v.y)
    static member (+)  (v1: ObjVector, v2:ObjVector)
        = ObjVector(v1.x + v2.x, v1.y + v2.y)
    static member (-)  (v1: ObjVector, v2:ObjVector)
        = ObjVector(v1.x - v2.x, v1.y - v2.y)
    static member (*) (a,v:ObjVector) = ObjVector(a*v.x,a*v.y)
    static member (*)  (v1: ObjVector, v2:ObjVector)
        = v1.x * v2.x + v1.y * v2.y

let a = ObjVector(1.0, -2.0)

let b = ObjVector(Y=4.0, X=3.0)

b.coord()

let c = 2.0 * a - b

c.coord()

b.x

let d = c * a

let e = b.norm()

let g = (+) a b

g.coord()
