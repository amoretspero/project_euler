open System
open System.Collections
open System.Diagnostics
open System.IO
open System.Linq
open System.Numerics
open System.Collections.Generic

let sw = Stopwatch()
sw.Start()

exception INVALID_INPUT

let get_square_diagonal_sum (n : int) =
    match n with
    | 1 -> 1
    | _ when n>=2 -> (2*n - 1)*(2*n - 1)*4 - (6 * (n-1) * 2)
    | _ -> raise INVALID_INPUT

let rec get_total_diagonal (n : int) =
    match n with
    | _ when n > 1 -> (int64 (get_square_diagonal_sum n)) + (int64 (get_total_diagonal (n-1)))
    | 1 -> 1L
    | _ -> raise INVALID_INPUT

let answer = get_total_diagonal 501

sw.Stop()
printfn "Answer : %d" answer
printfn "Elapsed Time : %f" sw.Elapsed.TotalMilliseconds