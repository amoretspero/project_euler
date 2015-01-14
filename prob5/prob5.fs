open System
open System.Diagnostics

let sw = Stopwatch()
sw.Start()

let first_elem (a, _, _) = a
let second_elem (_, b, _) = b
let third_elem (_, _, c) = c

let rec hash_detect (n : int) (hashset : System.Collections.Generic.HashSet<int>) =
    let mutable res = false
    for elem in hashset do
        if n%elem=0 then res <- true
    res

let prime_list = ref []
let rec prime_detect (n : int) =
    let mutable res = true
    let mutable i = 0
    while res&&(i < (!prime_list).Length) do
        if n%(!prime_list).[i]=0 then res <- false
    if res then
        let limit = int(floor(sqrt(float(n))))
        let mutable i = 2
        while i <= limit do
            if n%i = 0 then res <- false
            i <- i + 1
    res
    
let rec prime_finder (n : int) =
    if n=2 then [n]
    else if (prime_detect n) then n::(prime_finder(n-1))
    else (prime_finder (n-1))
    
let rec factor_find (num : int) (prime : int) =
    if num%prime=0 then 1+(factor_find (num/prime) prime)
    else 0
    
let rec factor_adder (num_lst : int list) (prime_lst : int list) =
    match prime_lst with
    h::t -> 
        let mutable res = 0
        let mutable max_factor = 0
        for elem in num_lst do
            let factor = (factor_find elem h)
            res <- res + factor
            if factor > max_factor then max_factor <- factor
        if res = 0 then (factor_adder num_lst t)
        else (h, res, max_factor) :: (factor_adder num_lst t)
    | [] -> []
        
    
let rec factorize_list (int_lst : int list) =
    let max_val = List.max(int_lst)
    let prime_lst = prime_finder max_val
    let factorized = (factor_adder int_lst prime_lst)
    let mutable res = 1
    for elem in factorized do
        res <- res * (int((float(first_elem elem))**(float(third_elem elem))))
    res

let answer = factorize_list [1 .. 20]

sw.Stop()

printfn "Answer : %d" answer
printfn "Elapsed Time : %f" sw.Elapsed.TotalMilliseconds
        
    
    
    