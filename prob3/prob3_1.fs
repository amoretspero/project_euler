open System

let mutable bound = 1000000

let prime_detector (n : int64) =
    let mutable res = true
    let limit = Convert.ToInt64(floor(sqrt(float(n))))
    let mutable i = 2L
    while i <= limit do
        if n%i=0L then res <- false
        i <- i + 1L
    res
    
let rec prime_finder (n : int) =
    if n = 2 then [ n ]
    else if (prime_detector (Convert.ToInt64(n))) then
        n :: (prime_finder (n-1))
    else
        (prime_finder (n-1))
        
let rec multiplier (lst : int64 list) =
    match lst with
    [] -> 1L
    | [ elem ] -> elem
    | _ -> lst.Head * (multiplier (lst.Tail))
    
let value_m = multiplier ([ for elem in (prime_finder bound) -> Convert.ToInt64((float elem)**(floor(Math.Log((float bound), (float elem)))))])
