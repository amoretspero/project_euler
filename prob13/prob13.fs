open System
open System.Diagnostics
open System.Collections.Generic
open System.IO
open System.Numerics

let sw = Stopwatch()
sw.Start()

let num_lst = [ for num in (File.ReadAllLines("num.txt")) -> BigInteger.Parse(num) ]
let mutable sum = BigInteger(0)
for bi in num_lst do
    sum <- sum + bi
let answer = (Convert.ToString(sum)).Substring(0, 10)

sw.Stop()
printfn "Answer : %s" answer
printfn "Elapsed Time : %f" sw.Elapsed.TotalMilliseconds