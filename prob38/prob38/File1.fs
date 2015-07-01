module File1

open System
open System.Collections
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Linq
open System.Numerics

let check_pandigital (n : int64) =
    if n.ToString().Length = 9 && (not (n.ToString().Contains("0"))) then
        n.ToString().ToCharArray().Distinct().Count() = 9
    else
        false

let get_concat_product (num : int) (n : int) =
    let mutable res = ""
    for i=1 to n do
        res <- res + (num*i).ToString()
    Convert.ToInt64 res

let find_max_concat_product (n : int) =
    let mutable digit_break = true
    let mutable res = 0L
    let mutable cnt = 1
    while digit_break do
        let concat_prod = get_concat_product cnt n
        if (check_pandigital concat_prod) then
            if concat_prod > res then
                res <- concat_prod
        else
            if concat_prod.ToString().Length > 9 then
                digit_break <- false
        cnt <- cnt + 1
    res

let mutable res = 0L
let sw = new Stopwatch()
sw.Start()
for i=2 to 9 do
    let max_concat_prod_temp = find_max_concat_product i
    printfn "For n=%d, maximum concatation product is : %d" i max_concat_prod_temp
    if res < max_concat_prod_temp then
        res <- max_concat_prod_temp
sw.Stop()
printfn "Maximum pandigital multiple : %d - Elapsed Time : %d(ms)" res sw.ElapsedMilliseconds
