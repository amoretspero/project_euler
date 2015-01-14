open System
open System.Diagnostics

let sw = Stopwatch()
sw.Start()

let prime_hash = new System.Collections.Generic.HashSet<int64>()

let rec hash_detect (n : int64) (hashset : System.Collections.Generic.HashSet<int64>) =
    let mutable res = false
    for elem in hashset do
        if n%elem=0L then res <- true
    res
    
let rec prime_detect (n : int64) =
    let mutable res = true
    if (hash_detect n prime_hash) then
        res <- false
    if res then
        let limit = int(floor(sqrt(float(n))))
        let mutable i = 2L
        while i <= int64(limit) do
            if n%i=0L then res <- false
            i <- i+1L
    res
    
let rec get_next_prime (start_num : int64) =
    if (prime_detect (start_num+1L)) then
        start_num+1L
    else
        (get_next_prime (start_num+1L))
        
let rec get_nth_prime (n : int) (current : int64) =
    match n with 
    | 0 -> 1L
    | 1 -> (get_next_prime current)
    | _ -> (get_nth_prime (n-1) (get_next_prime current))
    
let answer = get_nth_prime 10001 1L

sw.Stop()

printfn "Answer : %d" answer
printfn "Elapsed Time : %f" sw.Elapsed.TotalMilliseconds