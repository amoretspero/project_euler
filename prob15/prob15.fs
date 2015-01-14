open System
open System.Diagnostics
open System.Collections.Generic

let sw = Stopwatch()
sw.Start()

let dict = new Dictionary<(int64 * int64), int64>()

let rec find_path (x : int64) (y : int64) =
    match x with
    | _ when (x=0L || y=0L) -> 1L
    | _ ->
        let mutable prev_down = 0L
        let mutable prev_left = 0L
        if dict.ContainsKey((x-1L,y)) then 
            prev_down <- dict.[(x-1L,y)]
        else
            prev_down <- (find_path (x-1L) y)
        if dict.ContainsKey((x,y-1L)) then 
            prev_left <- dict.[(x,y-1L)]
        else
            prev_left <- (find_path x (y-1L))
        dict.Add((x, y), (prev_down+prev_left))
        (prev_down+prev_left)

let answer = find_path 20L 20L

sw.Stop()
printfn "Answer : %d" answer
printfn "Elapsed Time : %f" sw.Elapsed.TotalMilliseconds