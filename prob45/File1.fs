module File1

open System
open System.Collections
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Linq
open System.Numerics

let triangle_num_set = new HashSet<int64>()
let pentagonal_num_set = new HashSet<int64>()
let hexagonal_num_set = new HashSet<int64>()
let mutable triangle_num_set_end = 0
let mutable pentagonal_num_set_end = 0
let mutable hexagonal_num_set_end = 0

/// Generate 1st to nth triangle number
let triangle_num_gen (n : int) =
    if n > triangle_num_set_end then
        for i=triangle_num_set_end+1 to n do
            let elem = ((int64)i * (int64)i + (int64)i)/2L
            triangle_num_set.Add(elem) |> ignore

/// Generate 1st to nth pentagonal number
let pentagonal_num_gen (n : int) =
    if n > pentagonal_num_set_end then
        for i=pentagonal_num_set_end+1 to n do
            let elem = (3L * (int64)i * (int64)i - (int64)i)/2L
            pentagonal_num_set.Add(elem) |> ignore

/// Generate 1st to nth hexagonal number
let hexagonal_num_gen (n : int) =
    if n > hexagonal_num_set_end then
        for i=hexagonal_num_set_end+1 to n do
            let elem = 2L * (int64)i * (int64)i - (int64)i
            hexagonal_num_set.Add(elem) |> ignore

let mutable triangle_num_in = -1
let mutable pentagonal_num_in = -1
let mutable hexagonal_num_in = -1
printfn "Enter how many trignale numbers to generate :"
triangle_num_in <- Convert.ToInt32(Console.ReadLine())
printfn "Enter how many pentagonal numbers to generate :"
pentagonal_num_in <- Convert.ToInt32(Console.ReadLine())
printfn "Enter how many hexagonal numbers to generate :"
hexagonal_num_in <- Convert.ToInt32(Console.ReadLine())

let ans = new HashSet<int64>()

let sw = new Stopwatch()

sw.Start()
printfn "\n==============================\n\nNow generating triangle numbers."
triangle_num_gen(triangle_num_in)
printfn "Generating triangle numbers done! - Elapsed Time : %d(ms)\n" sw.ElapsedMilliseconds
printfn "Now generating pentagonal numbers."
pentagonal_num_gen(pentagonal_num_in)
printfn "Generating pentagonal numbers done! - Elapsed Time : %d(ms)\n" sw.ElapsedMilliseconds
printfn "Now generating hexagonal numbers."
hexagonal_num_gen(hexagonal_num_in)
printfn "Generating hexagonal numbers done! - Elapsed Time : %d(ms)\n" sw.ElapsedMilliseconds
printfn "Now calculating for triangle AND pentagonal AND hexagonal number.\n"
for elem_hex in hexagonal_num_set do
    if pentagonal_num_set.Contains(elem_hex) then
        if triangle_num_set.Contains(elem_hex) then
            printfn "Found triangle AND pentagonal AND hexagonal number! - %d" elem_hex
            ans.Add(elem_hex) |> ignore
sw.Stop()
printfn "\nCalculating for triangle AND pentagonal AND hexagonal number done! - Elapsed Time : %d(ms)\n" sw.ElapsedMilliseconds

printfn "\n==============================\n\nFound numbers are : "
for elem in ans do
    printfn "%d" elem

(*
    Answer would appear with input of all 100000
*)