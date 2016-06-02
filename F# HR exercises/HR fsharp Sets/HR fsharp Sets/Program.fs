// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

let males = set["Bob"; "Bill"; "Ben"];;

let rec setremv s:Set<string> = 
    if Set.count s < 2 then s else setremv (Set.remove (Set.maxElement s) s);;