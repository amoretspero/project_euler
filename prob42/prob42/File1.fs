module File1

open System
open System.Collections
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Linq
open System.Numerics

let eng_words = File.ReadAllText("p042_words.txt").Split([| ',' |]) |> Array.map(fun s -> s.Substring(1, s.Length-2))

let word_encoder (s : string) =
    let mutable res = 0
    for i=0 to s.Length-1 do
        res <- res + Convert.ToInt32(s.[i]) - 64
    res

let encoded_words = Array.map word_encoder eng_words

let triangle_set = new HashSet<int>()

let mutable triangle_set_max = 0

let triangle_set_gen (n : int) =
    let mutable loop_break = false
    let mutable cnt = 1
    while (not loop_break) do
        let temp = (cnt * (cnt + 1))/2
        if (not (triangle_set.Contains(temp))) then
            triangle_set.Add(temp) |> ignore
        if temp + cnt + 1 > n then
            loop_break <- true
        cnt <- cnt + 1
    triangle_set_max <- triangle_set.Max()
triangle_set_gen(2000)

let mutable ans = 0

for elem in encoded_words do
    if elem > triangle_set_max then
        triangle_set_gen(elem)
    if triangle_set.Contains(elem) then
        ans <- ans + 1

printfn "For given file, there are %d triangle words!" ans
