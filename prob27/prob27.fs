open System
open System.Collections
open System.Diagnostics
open System.IO
open System.Linq
open System.Numerics
open System.Collections.Generic

let sw = Stopwatch()
sw.Start()

let rec det_prime (n : int) =
    if n<=1 then false
    else if n=2 then true
    else if n=3 then true
    else
        let mutable res = true
        for i=2 to (int32(floor(sqrt(float(n))))) do
            if n%i=0 then res <- false else res <- res
        res
        
let arb_formula (a : int) (b : int) (n : int) = n*n + a*n + b

let formula_gen (a : int) (b : int) = (arb_formula a b)

let test_formula (formula : int -> int) =
    let mutable res = 0
    let mutable stop = false
    let mutable cnt = 0
    while (not stop) do
        if (det_prime (formula(cnt))) then
            res <- cnt
            cnt <- cnt + 1
        else
            stop <- true
    res

let find_max (a_min : int) (a_max : int) (b_min : int) (b_max : int) =
    let mutable res_max = 0
    let mutable res_a = 0
    let mutable res_b = 0
    for i=a_min to a_max do
        for j=b_min to b_max do
            if j <= 0 then
                res_max <- res_max
            else
                let formula = formula_gen i j
                let res = test_formula formula
                if res > res_max then 
                    res_max <- res
                    res_a <- i
                    res_b <- j
    printfn "Maximum length : %d, a : %d, b : %d, mult : %d" res_max res_a res_b (res_a*res_b)

