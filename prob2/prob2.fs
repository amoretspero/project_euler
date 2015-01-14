open System
open System.Diagnostics

let sw = Stopwatch()
sw.Start()
let mutable init1 = 0
let mutable init2 = 1
let mutable fib_res = 0
let mutable res = 0
while init2 <= 4000000 do
    let temp = init1 + init2
    if temp%2=0 && temp <= 4000000 then res <- res + temp
    init1 <- init2
    init2 <- temp
sw.Stop()
printfn "Answer : %d" res
printfn "Elapsed Time : %f" sw.Elapsed.TotalMilliseconds