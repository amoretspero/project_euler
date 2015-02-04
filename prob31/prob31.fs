open System
open System.Collections
open System.Diagnostics
open System.IO
open System.Linq
open System.Numerics
open System.Collections.Generic

let sw = Stopwatch()
sw.Start()

let coins = [200; 100; 50; 20; 10; 5; 2; 1]

let rec count_ways (lst : int list) (price : int) =
    match lst with
    | [] -> 0
    | _ when price = 0 -> 1
    | [ elem ] ->
        if (price >= elem)&&(price%elem=0) then 1 else 0
    | h :: t ->
        if price < h then 
            0+(count_ways t price)
        else
            let quotient = price/h
            let h_time = [0 .. quotient]
            List.sum [for elem in h_time -> (count_ways t (price - h*elem))]

let answer = count_ways coins 200

sw.Stop()

printfn "Answer : %d" answer
printfn "Elapsed Time : %f" sw.Elapsed.TotalMilliseconds