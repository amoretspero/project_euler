open System
open System.Collections
open System.Diagnostics
open System.Numerics
open System.Linq
open System.IO
open System.Collections.Generic

exception INVALID_INPUT

let sw = Stopwatch()
sw.Start()

let rec factorial (n : int) =
    match n with
    0 -> 1
    | 1 -> 1
    | _ when (n >= 2) -> n*(factorial (n-1))
    | _ -> raise INVALID_INPUT

let rec lst_remover (lst : 'a list) (elem : 'a) =
    match lst with
    | [] -> raise INVALID_INPUT
    | [ a ] -> if elem=a then [] else [ a ]
    | h::t -> if h=elem then (lst_remover t elem) else h::(lst_remover t elem)

let rec permute (lst : int list) (n : int) =
    match n with
    | _ when (n > factorial(lst.Length)) -> raise INVALID_INPUT
    | _ when (n < 0) -> raise INVALID_INPUT
    | _ when (n = 0)||(lst = []) -> ""
    | _ ->
        let mutable temp = 0
        while (temp<lst.Length)&&(((temp+1)*(factorial (lst.Length - 1))) < n) do
            temp <- temp + 1
        Convert.ToString(lst.[temp])+(permute (lst_remover lst lst.[temp]) (n - (temp*(factorial (lst.Length - 1)))))

let answer = (permute [0; 1; 2; 3; 4; 5; 6; 7; 8; 9] 1000000)

sw.Stop()
printfn "Answer : %s" answer
printfn "Elapsed Time : %f" sw.Elapsed.TotalMilliseconds