open System
open System.Collections
open System.Diagnostics
open System.Linq
open System.IO
open System.Numerics
open System.Collections.Generic

let sw = Stopwatch()
sw.Start()

let distincts = ref (new Set<BigInteger>(Seq.empty))

let get_distinct (a_min : int) (a_max : int) (b_min : int) (b_max : int) =
    for i=a_min to a_max do
        for j=b_min to b_max do
            let power = ((BigInteger(i))**j)
            distincts := (!distincts).Add(power)

(get_distinct 2 100 2 100)
let answer = (!distincts).Count

sw.Stop()

printfn "Answer : %d" answer
printfn "Elapsed Time : %f" sw.Elapsed.TotalMilliseconds