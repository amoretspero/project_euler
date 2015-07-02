module File1

open System
open System.Collections
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Linq
open System.Numerics

let pentagonal_set = new HashSet<int64>()

let pentagonal_gen (n : int) =
    for i=1 to n do
        let elem = ((int64)i * (3L * (int64)i - 1L)) / 2L
        pentagonal_set.Add(elem) |> ignore

let mutable pent_num = -1

printfn "Enter the number of pentagonal numbers to create :"
pent_num <- Convert.ToInt32(Console.ReadLine())

let sw = new Stopwatch()
sw.Start()
printfn "\nStarting pentagonal number generation\n"
pentagonal_gen(pent_num)
printfn "End of pentagonal number generation - Elapsed Time : %d(ms)\n" sw.ElapsedMilliseconds

let mutable res_i = -1
let mutable res_j = -1
let mutable res_diff = -1L

//for i=0 to pent_num-1 do
for i=1 to pent_num do
    let temp_i = (3L * (int64)i * (int64)i - (int64)i)/2L
    //for j=i+1 to pent_num-1 do
    for j=i+1 to pent_num do
        (*let temp_i = pentagonal_set.ElementAt(i)
        let temp_j = pentagonal_set.ElementAt(j)*)
        let temp_j = (3L * (int64)j * (int64)j - (int64)j)/2L
        let temp_sum = temp_i + temp_j
        let temp_diff = temp_j - temp_i
        if pentagonal_set.Contains(temp_sum) then
            if pentagonal_set.Contains(temp_diff) then
                if res_diff = -1L || temp_diff < res_diff then
                    res_i <- i
                    res_j <- j
                    res_diff <- temp_diff

sw.Stop()
printfn "End of Searching for answer in %d pentagonal numbers - Elapsed Time : %d(ms)\n" pent_num sw.ElapsedMilliseconds
printfn "Answer : %d" res_diff

(*
    Answer would appear with pent_num=10000
*)