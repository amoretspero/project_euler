module File1

open System
open System.Collections
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Linq
open System.Numerics

open prime_funcs

let prime_set = new HashSet<int>()
let prime_set_end = ref 0

let check_square (n : int) =
    let sqrt_n = System.Math.Sqrt(float n)
    let sqrt_n_int = (float)(Convert.ToInt32(sqrt_n))
    if sqrt_n-sqrt_n_int=0.0 then
        true
    else
        false

/// Check goldbach's conjecture for i=2 to n.
let check_goldbach (n : int) =
    if prime_set_end.Value < n then
        prime_gen n prime_set_end prime_set
    for i=2 to n do
        if i%2<>0 && (not (prime_set.Contains(i))) then
            let mutable loop_break = false
            let mutable cnt = 0
            let mutable res = false
            while (not loop_break) && (cnt < prime_set.Count) do
                let elem = prime_set.ElementAt(cnt)
                if elem < i then
                    let diff = i - elem
                    //printfn "i = %d, elem = %d, diff = %d" i elem diff
                    if diff%2=0 then
                        if (check_square (diff/2)) then
                            res <- true
                        else
                            res <- res
                else
                    loop_break <- true
                cnt <- cnt + 1
            if (not res) then
                printfn "Goldbach's conjecture failed! - %d" i
    printfn "\nCheck for goldbach's conjecture is finished!\n"

let mutable g_end = -1
printfn "Enter g_end(check_goldbach will check goldbach's conjecture from 2 to g_end) :"
g_end <- Convert.ToInt32(Console.ReadLine())

check_goldbach(g_end)