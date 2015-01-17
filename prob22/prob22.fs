open System
open System.Collections
open System.Diagnostics
open System.Numerics
open System.Collections.Generic
open System.IO
open System.Linq

let sw = Stopwatch()
sw.Start()

let names = List.ofArray(((File.ReadAllText("names.txt")).Split([| ' '; '\"'; ',' |]))) |> List.filter (fun s -> (s<>"\"")&&(s<>",")&&(s<>"")) |> List.sort

let get_score (name : string) (pos : int) =
    let mutable res = 0L
    for i=0 to name.Length-1 do
        res <- res + ((Convert.ToInt64(name.[i]))-64L)
    res*(Convert.ToInt64(pos))

let rec get_name_score_sum (lst : string list) (len : int) =
    match lst with
    | [] -> 0L
    | h::t -> (get_score h (len-(lst.Length - 1)))+(get_name_score_sum t len)

let answer = get_name_score_sum names names.Length

sw.Stop()
printfn "Answer : %d" answer
printfn "Elapsed Time : %f" sw.Elapsed.TotalMilliseconds