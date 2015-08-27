module File1

open System
open System.Collections
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Linq
open System.Numerics

let get_self_power (n : int) =
    let mutable res = 1L
    for i=0 to n-1 do
        res <- (res * (int64 n))%10000000000L
    //printfn "Self power of %d is : %d" n res
    res

let get_sum_ten_digit (lst : int64 list) =
    let mutable res = 0L
    for i=0 to lst.Length-1 do
        res <- (res + lst.[i])%10000000000L
    res

let get_ten_digit (limit : int) =
    let digit_lst = [for i in 1 .. limit -> (get_self_power i)]
    get_sum_ten_digit digit_lst


let ans_sw = new Stopwatch()
ans_sw.Start()
let mutable num_insert = -1
printfn "Insert number : "
num_insert <- System.Convert.ToInt32(System.Console.ReadLine())
let ans = get_ten_digit(num_insert)
ans_sw.Stop()
printfn "Answer : %d (Elapsed Time : %f ms)" ans ans_sw.Elapsed.TotalMilliseconds

(*
===================
Answer : 9110846700
Input :
    num_insert : 1000
Elapsed Time : 
    Answer search : 957.394100ms
System : 
    CPU : i7-4790K (4.0GHz - Turbo 4.4GHz)
    RAM : DDR3-12800, 16GB @1600MHz
    GPU : AMD RADEON 290X
===================
*)