module File1

open System
open System.Collections
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Linq
open System.Numerics

let mutable p_start = -1
let mutable p_end = -1
printfn "Insert p_start : (MIN : 12)"
p_start <- Convert.ToInt32(Console.ReadLine())
printfn "Insert p_end : (MAX : 1000)"
p_end <- Convert.ToInt32(Console.ReadLine())

let mutable ans = 0
let mutable ans_p = 0
let sw = new Stopwatch()
sw.Start()
for cnt=p_start to p_end do
    let square_lst = List.map (fun i -> i * i) [1 .. cnt-2]
    let mutable res = 0
    for i=1 to cnt-3 do
        for j=i to cnt-3 do
            let k = cnt - i - j
            if k < 0 || k < i || k < j then
                res <- res
            else
                if (i*i + j*j = k*k) then
                    printfn "Solution found for p = %d, i=%d, j=%d, k=%d" cnt i j k
                    res <- res + 1
    //printfn "Number of solutions for p = %d is : %d" cnt res
    if res > ans then
        ans <- res
        ans_p <- cnt
sw.Stop()
printfn "Maximum number of solutions (at p = %d) : %d - Elapsed Time : %d" ans_p ans sw.ElapsedMilliseconds