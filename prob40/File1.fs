module File1

open System
open System.Collections
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Linq
open System.Numerics

let sw = new Stopwatch()
sw.Start()
let mutable digit_count = 0
let mutable prev_digit_count = 0
let mutable cnt = 1
let mutable temp = 1
let mutable ans = 1
printfn "Data Initialization done - Elapsed Time : %d(ms)" sw.ElapsedMilliseconds
while temp <= 1000000 do
    prev_digit_count <- digit_count
    digit_count <- digit_count + cnt.ToString().Length
    if prev_digit_count < temp && temp <= digit_count then
        let found_digit = Convert.ToInt32(cnt.ToString().[cnt.ToString().Length - 1 - (digit_count - temp)].ToString())
        printfn "d_%d : %d - Elapsed Time : %d(ms)" temp found_digit sw.ElapsedMilliseconds
        ans <- ans * found_digit
        temp <- temp * 10
    cnt <- cnt + 1
printfn "Answer : %d - Elapsed Time : %d(ms)" ans sw.ElapsedMilliseconds