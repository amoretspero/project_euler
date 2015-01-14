open System
open System.Diagnostics
open System.Collections.Generic

let sw = Stopwatch()
sw.Start()

let get_collatz_seq (n : int64) =
    let res = ref [ n ]
    let mutable start_num = n
    while (start_num <> 1L) do
        if start_num%2L=0L then
            start_num <- start_num/2L
            res := List.append (!res) [ start_num ]
        else
            start_num <- (3L*start_num + 1L)
            res := List.append (!res) [ start_num ]
    !res

let rec get_collatz_seq_rec (n : int64) =
    match n with
    1L -> [ n ]
    | _ ->
        if n%2L=0L then List.append  [ n ] (get_collatz_seq_rec (n/2L))
        else List.append [ n ] (get_collatz_seq_rec (3L*n + 1L))

let rec chain_detector_rec (n : int64) =
    match n with
    1L -> 1
    | _ ->
        System.Math.Max((get_collatz_seq n).Length, (chain_detector_rec (n-1L)))

let chain_hash = new System.Collections.Hashtable()

let rec chain_detector (n : int) =
    let mutable res = 0L
    let mutable res_start = 0
    for i=1 to n do
        let mutable len = 0L
        if i%2=0 then 
            let mutable div_num = 0
            let mutable i_temp = i
            while (i_temp%2 = 0) do
                div_num <- div_num + 1
                i_temp <- i_temp/2
            len <- (Convert.ToInt64((chain_hash.Item(i_temp)).ToString()))+(int64 div_num)
        else
            len <- int64((get_collatz_seq (int64(i))).Length)
            chain_hash.Add(i, len)
        if len > res then 
            res <- len
            res_start <- i
    res_start

let answer = chain_detector 1000000

sw.Stop()
printfn "Answer : %d" answer
printfn "Elapsed Time : %f" sw.Elapsed.TotalMilliseconds