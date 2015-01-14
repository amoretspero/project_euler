open System
open System.Diagnostics

let sw = Stopwatch()
sw.Start()
let prime_hash = new System.Collections.Generic.HashSet<int64>()
let prime_list = ref [ ]

let rec hash_detect (n : int64) (hashset : System.Collections.Generic.HashSet<int64>) =
    let mutable res = false
    for elem in hashset do
        if n%elem=0L then res <- true
    res

let rec prime_detect (n : int64) =
    //let prime_list = ref [ Convert.ToInt64(2) ]
    let mutable res = true
    (*if (hash_detect n prime_hash) then
        res <- false
        res*)
    let mutable i = 0
    while res&&(i < (!prime_list).Length) do
        if n%(!prime_list).[i]=0L then res <- false
    if res then
        let limit = int(floor(sqrt(float(n))))
        let mutable i = Convert.ToInt64(2)
        while i <= int64(limit) do
            if n%i = Convert.ToInt64(0) then res <- false
            i <- i + Convert.ToInt64(1)
        //for i=0 to (!prime_list).Length - 1 do
        //    if n % (!prime_list).[i] = Convert.ToInt64(0) then res <- false
        //if res = true then prime_hash.Add(n) |> ignore
    res

let dividable (n : int64) =
    let mutable res = false
    let mutable i = 0
    while (not res)&&(i < (!prime_list).Length) do
        if n%(!prime_list).[i]=Convert.ToInt64(0) then res <- true
        i <- i + 1
    res

let rec get_next_prime (start_num : int64) =
    //if dividable(start_num) then
    //    (get_next_prime (start_num+Convert.ToInt64(1)))
    if (prime_detect (start_num+(Convert.ToInt64(1)))) then 
        //prime_list := (start_num + Convert.ToInt64(1)) :: !prime_list
        start_num+Convert.ToInt64(1)
    else (get_next_prime (start_num+Convert.ToInt64(1)))

let mutable factorize_start = Convert.ToInt64(2)

let rec factorize (n : int64) =
    if (n%factorize_start=Convert.ToInt64(0)) then 
        let next_n = n/factorize_start
        //let factorize_start_temp = factorize_start
        //factorize_start <- get_next_prime factorize_start
        printfn "%d" factorize_start
        factorize_start :: (factorize next_n)
    else if (factorize_start <= n) then
        printfn "%d" factorize_start
        factorize_start <- (get_next_prime factorize_start)
        (factorize n)
    else
        []
        
let test = factorize 851475143L
sw.Stop()
printfn "Answer : %d" test.[test.Length - 1]
printfn "Elapsed Time : %f(ms)" sw.Elapsed.TotalMilliseconds
