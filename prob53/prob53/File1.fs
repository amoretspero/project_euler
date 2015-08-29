module File1

open System
open System.Collections
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Linq
open System.Numerics

exception InvalidInput

/// Computes nCr
let combination (n : BigInteger) (r : BigInteger) =
    if n < r || n <= 0I || r < 0I then
        raise InvalidInput
    else
        let mutable res = 1I
        let n_temp = (int n)
        let r_temp = if r > n/2I then (int (n-r)) else (int r)
        for i=n_temp downto n_temp - r_temp + 1 do
            res <- res * System.Numerics.BigInteger.Parse(i.ToString())
        for j=1 to r_temp do
            res <- res / System.Numerics.BigInteger.Parse(j.ToString())
        res

/// Returns number of combinations larger than limit with n between n_start and n_end inclusive. 
let combination_determine (limit : BigInteger) (n_start : int) (n_end : int) =
    let mutable cnt = 0
    for n=n_start to n_end do
        for r=0 to n do
            let temp = combination (BigInteger.Parse(n.ToString())) (BigInteger.Parse(r.ToString()))
            if temp > limit then
                cnt <- cnt + 1
    cnt

let mutable limit_input = -1I
let mutable n_start_input = -1
let mutable n_end_input = -1

printfn "Insert limit_input :"
limit_input <- BigInteger.Parse(System.Console.ReadLine())
printfn ""
printfn "Insert n_start_input :"
n_start_input <- System.Convert.ToInt32(System.Console.ReadLine())
printfn ""
printfn "Insert n_end_input :"
n_end_input <- System.Convert.ToInt32(System.Console.ReadLine())
printfn ""

let find_ans_sw = new Stopwatch()
printfn "Now starting calculation...\n"
find_ans_sw.Start()
let ans = combination_determine limit_input n_start_input n_end_input
find_ans_sw.Stop()
printfn "Calculation done. Found %d combinations exceeds %s (Elapsed Time : %fms)" ans (limit_input.ToString()) find_ans_sw.Elapsed.TotalMilliseconds

(*
===================
Answer :4075
===================
*)