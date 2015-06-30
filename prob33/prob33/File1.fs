module File1

open System
open System.Collections
open System.Collections.Generic
open System.Numerics
open System.IO
open System.Diagnostics
open System.Linq

let prime_set = new HashSet<int>()

let prime_set_add (n : int) =
    if n=2 then
        prime_set.Add(n) |> ignore
    if (n % 2) <> 0 then
        let mutable res = true
        for elem in prime_set do
            if n%elem = 0 then
                res <- false
            else
                res <- res
        if res then
            prime_set.Add(n) |> ignore

let prime_gen (n : int) =
    for i=2 to n do
        let prime_set_max = 
            match prime_set.Count with
            | 0 -> 1
            | _ -> prime_set.Max()
        if prime_set_max < i then
            prime_set_add(i)


let find_gcd (n1 : int) (n2 : int) =
    let mutable n1_temp = n1
    let mutable n2_temp = n2
    prime_gen (System.Math.Min(n1_temp, n2_temp))
    let mutable res = 1
    for elem in prime_set do    
        while (n1_temp%elem=0 && n2_temp%elem=0) do
            res <- res * elem
            n1_temp <- n1_temp / elem
            n2_temp <- n2_temp / elem
    res
    

type frac (numerator : int, denominator : int) =
    
    let mutable curr_numerator = numerator
    let mutable curr_denominator = denominator
    
    member f.numerator with get() = curr_numerator and set v = curr_numerator <- v
    member f.denominator with get() = curr_denominator and set v = curr_denominator <- v

    member f.lowest_terms = 
        let gcd = (find_gcd curr_numerator curr_denominator)
        frac(curr_numerator/gcd, curr_denominator/gcd)

    member f.reduce(n : int) =
        if curr_numerator%n=0 && curr_denominator%n=0 then
            curr_numerator <- curr_numerator/n
            curr_denominator <- curr_denominator/n

    member f.string_notation() =
        curr_numerator.ToString() + "/" + curr_denominator.ToString()
    
    member f.ToFloat() =
        (float)curr_numerator / (float)curr_denominator

let fracs_found = new HashSet<frac>()

for i=11 to 99 do
    for j=10 to i-1 do
        if (i%10 <> 0) && (j%10 <> 0) then
            let curr_frac = frac(j, i)
            let incorr_frac1 = frac(j/10, i/10)
            let incorr_frac2 = frac(j/10, i%10)
            let incorr_frac3 = frac(j%10, i/10)
            let incorr_frac4 = frac(j%10, i%10)
            
            if curr_frac.ToFloat() = incorr_frac1.ToFloat() && j%10 = i%10 then
                printfn "curr_frac : %s, incorr_frac1 : %s" (curr_frac.string_notation()) (incorr_frac1.string_notation())
                fracs_found.Add(curr_frac) |> ignore
            else if curr_frac.ToFloat() = incorr_frac2.ToFloat() && j%10 = i/10 then
                printfn "curr_frac : %s, incorr_frac2 : %s" (curr_frac.string_notation()) (incorr_frac2.string_notation())
                fracs_found.Add(curr_frac) |> ignore
            else if curr_frac.ToFloat() = incorr_frac3.ToFloat() && j/10 = i%10 then
                printfn "curr_frac : %s, incorr_frac3 : %s" (curr_frac.string_notation()) (incorr_frac3.string_notation())
                fracs_found.Add(curr_frac) |> ignore
            else if curr_frac.ToFloat() = incorr_frac4.ToFloat() && j/10 = i/10 then
                printfn "curr_frac : %s, incorr_frac4 : %s" (curr_frac.string_notation()) (incorr_frac4.string_notation())
                fracs_found.Add(curr_frac) |> ignore

printfn "fracs_found.Count : %d" fracs_found.Count