open System
open System.Collections
open System.Diagnostics
open System.Numerics
open System.Collections.Generic

let sw = Stopwatch()
sw.Start()

let rec factorial (n : int) =
    let mutable res = BigInteger(1)
    let mutable counter = n
    while (counter <> 0) do
        res <- res * BigInteger(counter)
        counter <- counter - 1
    res
    
let rec biginteger_digit_sum (n : BigInteger) =
    let b_str = n.ToString()
    let mutable res = 0
    for i=0 to b_str.Length-1 do
        res <- res + (Convert.ToInt32(Convert.ToString(b_str.[i])))
    res
    
let answer = biginteger_digit_sum (factorial 100)

sw.Stop()
printfn "Answer : %d" answer
printfn "Elapsed Time : %f" sw.Elapsed.TotalMilliseconds