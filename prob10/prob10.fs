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
    if n=1L then res <- false
    res;;
    
let mutable answer = 0L
for i=1 to 1999999 do
    if (prime_detect (int64 i)) then answer <- answer + (int64 i)

sw.Stop()
printfn "Answer : %d" answer
printfn "Elapsed Time : %f" sw.Elapsed.TotalMilliseconds