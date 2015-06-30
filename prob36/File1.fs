module File1

open System
open System.Collections
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Numerics
open System.Linq

let rec get_binary (n : int) =
    match n with
    0 -> ""
    | 1 -> "1"
    | _ ->
        match (n%2) with
        | 0 -> (get_binary (n/2)) + "0"
        | 1 -> (get_binary (n/2)) + "1"
        | _ -> ""

let check_binary_palindrome (n : int) =
    let byte_rep_str = get_binary(n)
    let mutable res = true
    for i=0 to byte_rep_str.Length/2 - 1 do
        if byte_rep_str.[i] <> byte_rep_str.[byte_rep_str.Length - 1 - i] then
            res <- false
        else
            res <- res
    res

let check_decimal_palindrome (n : int) =
    let str_notation = n.ToString()
    let mutable res = true
    for i=0 to str_notation.Length/2 - 1 do
        if str_notation.[i] <> str_notation.[str_notation.Length - 1 - i] then
            res <- false
        else
            res <- res
    res

let check_double_base_palindrome (n : int) =
    (check_binary_palindrome n) && (check_decimal_palindrome n)

let mutable res = 0I
let mutable res_count = 0
let mutable num_end = -1
let mutable num_start = -1

printfn "Enter num_start : "
num_start <- Convert.ToInt32(Console.ReadLine())
printfn "Enter num_end : "
num_end <- Convert.ToInt32(Console.ReadLine())

printfn "\nNow Searching for Double-base palindromes from %d to %d\n" num_start num_end
let sw = new Stopwatch()
sw.Start()
for i=num_start to num_end do
    if (check_double_base_palindrome i) then
        printfn "Double-base palindrome found! - DECIMAL : %d, BINARY : %s" i (get_binary i)
        res_count <- res_count + 1
        res <- res + BigInteger.Parse(i.ToString())
sw.Stop()
printfn "\nNumber of all double-base palindromes : %d" res_count
printfn "Sum of all double-base palindromes : %s" (res.ToString())
printfn "Elapsed Time : %d(ms)\n" sw.ElapsedMilliseconds