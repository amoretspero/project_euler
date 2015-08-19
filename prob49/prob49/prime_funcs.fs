﻿module prime_funcs

open System
open System.Collections
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Linq
open System.Numerics

type prime_factor(basis : int, exp : int) =
    let mutable curr_basis = basis
    let mutable curr_exp = exp

    member pf.basis with get() = curr_basis and set v = curr_basis <- v
    member pf.exp with get() = curr_exp and set v = curr_exp <- v

    override pf.ToString() = curr_basis.ToString() + "^" + curr_exp.ToString()
    

/// prime_gen : This function generates primes less than or equal to n, 
/// sets prime_set_end to n, and saves the generated primes to prime_set
let prime_gen (n : int) (prime_set_end : int ref) (prime_set : HashSet<int>) =
    if n > prime_set_end.Value then
        for i=prime_set_end.Value+1 to n do
            if i=2 then
                prime_set.Add(i) |> ignore
                //printfn "Generated prime : %d" i
            else if i%2<>0 && i > 2 then
                let sqrt_i = Convert.ToInt32(System.Math.Sqrt(float i))
                let mutable loop_break = false
                let mutable cnt = 0
                let mutable res = true
                while (not loop_break) && (cnt < prime_set.Count) do
                    let elem = prime_set.ElementAt(cnt)
                    if i%elem=0 then
                        res <- false
                        loop_break <- true
                    else if elem > sqrt_i then
                        loop_break <- true
                    else
                        res <- res
                        cnt <- cnt + 1
                if res then
                    prime_set.Add(i) |> ignore
                    //printfn "Generated prime : %d" i
    prime_set_end.Value <- n