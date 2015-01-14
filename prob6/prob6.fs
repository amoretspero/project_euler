open System
open System.Diagnostics

let sw = Stopwatch()
sw.Start()

let rec sum_of_square (lst : int list) =
    match lst with 
    | [] -> 0
    | h::t -> (h*h)+(sum_of_square t)
    
let rec square_of_sum (lst : int list) =
    (int ((float(List.sum(lst)))**2.0))
    
let answer = Math.Abs((sum_of_square [1 .. 100]) - (square_of_sum [1 .. 100]))

sw.Stop()

printfn "Answer : %d" answer
printfn "Elapsed Time : %f(ms)" sw.Elapsed.TotalMilliseconds