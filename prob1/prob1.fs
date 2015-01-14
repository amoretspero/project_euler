open System
open System.Diagnostics

let sw = Stopwatch()
sw.Start()
let mutable sum = 0
for i=1 to 999 do
    if (i%3=0)||(i%5=0) then sum <- sum + i
sw.Stop()
printfn "Answer : %d" sum
printfn "Elapset time : %f" sw.Elapsed.TotalMilliseconds