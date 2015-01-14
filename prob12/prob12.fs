open System
open System.Diagnostics
open System.Collections.Generic

let sw = Stopwatch()
sw.Start()

let check_divisor (n : int64) =
    let res = ref ([] : int64 list)
    let mutable i = 1L
    while i <= int64(floor(sqrt(float(n)))) do
        if n%i=0L then
            if n%i<>i then res := i :: n/i :: (!res)
            else res := i :: (!res)
        i <- i + 1L
    !res

let triangle_number_gen (n : int64) = List.sum [ 1L .. n ]

let get_ans (n : int) =
    let res_lst = ref []
    let mutable res = 0L
    let mutable i = 1L
    while (!res_lst).Length <= n do
        let tri_num = triangle_number_gen i
        let divisor_lst = check_divisor tri_num
        res <- tri_num
        res_lst := divisor_lst
        printfn "i : %d" i
        i <- i + 1L
    res

let answer = get_ans(500)

sw.Stop()
printfn "Answer : %d" answer
printfn "Elapsed Time : %f" sw.Elapsed.TotalMilliseconds