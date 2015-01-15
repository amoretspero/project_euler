open System
open System.Diagnostics
open System.Collections.Generic
open System.Numerics

let sw = Stopwatch()
sw.Start()

let mutable init = BigInteger(1)
for i=1 to 1000 do
    init <- init * BigInteger(2)

let answer_str = init.ToString()
let mutable answer = 0
for i=0 to answer_str.Length-1 do
    answer <- answer + (Convert.ToInt32(Convert.ToString(answer_str.[i])))

sw.Stop()
printfn "Answer : %d" answer
printfn "Elapsed Time : %f" sw.Elapsed.TotalMilliseconds