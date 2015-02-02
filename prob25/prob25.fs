open System
open System.Collections
open System.Diagnostics
open System.Numerics
open System.IO
open System.Collections.Generic
open System.Linq

let sw = Stopwatch()
sw.Start()

let rec bi_fib (n : int) =
    let mutable res1 = BigInteger(0)
    let mutable res2 = BigInteger(1)
    if (n>=2) then
        for i=2 to n do
            let temp = res1 + res2
            res1 <- res2
            res2 <- temp
    else
        res2 <- res2
    res2

let rec fib_digit (n : int) =
    let mutable res = 1
    while ((bi_fib res).ToString().Length)<n do
        res <- res + 1
    res

let answer = (fib_digit 1000)

sw.Stop()
printfn "Answer : %d" answer
printfn "Elapsed Time : %f" sw.Elapsed.TotalMilliseconds