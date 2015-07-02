module File1

open System
open System.Collections
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Linq
open System.Numerics

let prime_set = new HashSet<int64>()

let mutable prime_set_end = 0L

let prime_gen (n : int64) =
    printfn "Generating prime to %d" n
    prime_set_end <- n
    let mutable i = 2L
    while i <= n do
    //for i=2L to n do
        if i=2L then
            prime_set.Add((int64)i) |> ignore
        else if i%2L<>0L then
            let mutable res = true
            let mutable cnt = 0L
            let mutable loop_break = false
            let n_square_root = Convert.ToInt64(System.Math.Sqrt(float n))
            while cnt < (int64)prime_set.Count && (not loop_break) do
                let elem = prime_set.ElementAt((int32)cnt)
                if i%elem = 0L then
                    res <- false
                else
                    res <- res
                if n_square_root < elem then
                    loop_break <- true
                cnt <- cnt + 1L
            if res then
                prime_set.Add((int64)i) |> ignore
        i <- i + 1L

let pandigital_check_set = new HashSet<int>()

let check_n_digit_pandigital (n : int64) =
    let str_notation = n.ToString()
    let length = str_notation.Length
    if n/(Convert.ToInt64(System.Math.Pow(float 10, float ((int64)length-1L)))) > (int64)length then
        false
    else if str_notation.[0]=str_notation.[1] then
        false
    else if str_notation.Contains("0") then
        false
    else
        let mutable res = true
        for i=0 to length-1 do
            if Convert.ToInt32(str_notation.[i].ToString()) > length then
                res <- false
            else
                res <- res
            pandigital_check_set.Add(Convert.ToInt32(str_notation.[i].ToString())) |> ignore
        if pandigital_check_set.Count = length then
            pandigital_check_set.Clear()
            res
        else
            pandigital_check_set.Clear()
            false

let prime_check (n : int64) =
    let sqrt_n = Convert.ToInt64(System.Math.Sqrt(float n))
    if prime_set_end < sqrt_n then
        prime_gen(sqrt_n + 1L)
    let mutable cnt = 0
    let mutable res = true
    while cnt < prime_set.Count && prime_set.ElementAt(cnt) <= sqrt_n && res do
        if n%(prime_set.ElementAt(cnt))=0L then
            res <- false
        else
            res <- res
        cnt <- cnt + 1
    res

let check_digits (n : int) =
    let sw_cd = new Stopwatch()
    sw_cd.Start()
    let mutable res = 0L
    let mutable i = Convert.ToInt64(System.Math.Pow(float 10, float (n-1)))
    prime_gen(Convert.ToInt64(System.Math.Sqrt(System.Math.Pow(float 10, float n))) + 1L)
    while i <= Convert.ToInt64(System.Math.Pow(float 10, float n))-1L do
        //printfn "Checking pandigitality and primality for %d" i
    //for i=Convert.ToInt64(System.Math.Pow(float 10, float (n-1))) to Convert.ToInt64(System.Math.Pow(float 10, float n))-1L do
        if (check_n_digit_pandigital i) then
            if (prime_check i) then
                if i > res then
                    res <- i
        i <- i + 1L
    sw_cd.Stop()
    printfn "Checking pandigitality and primality for %d-digit numbers done. Elapsed Time : %d(ms)" n sw_cd.ElapsedMilliseconds
    res

(*
    Answer would appear with check_digits with n=7
*)
