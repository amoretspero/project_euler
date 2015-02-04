open System
open System.Collections
open System.Diagnostics
open System.IO
open System.Linq
open System.Numerics
open System.Collections.Generic

let sw = Stopwatch()
sw.Start()

let first_elem (a, _, _) = a
let second_elem (_, b, _) = b
let third_elem (_, _, c) = c

let determin_pandigital (identity : int * int * int) =
    let identity_str = (first_elem identity).ToString() + (second_elem identity).ToString() + (third_elem identity).ToString()
    if identity_str.Distinct().Except("0").Count() = 9 && identity_str.Length = 9 then true else false

let get_pandigital_product () =
    let mutable cnt = 0
    let mutable sum = 0L
    let nums = ref []
    for i=1 to 9999 do
        for j=1 to 9999 do
            let prod = i*j
            if determin_pandigital(i, j, prod) && not((!nums).Contains(int64 prod)) then 
                cnt <- cnt + 1
                sum <- sum + (int64 (prod))
                nums := (int64 prod) :: (!nums)
                printfn "%d * %d = %d" i j prod
        printfn "i : %d done" i
    for elem in (!nums) do printfn "pandital num : %d" elem
    (cnt, sum)

let answer = get_pandigital_product()

sw.Stop()
printfn "Answer : %d" (snd answer)
printfn "Elapsed Time : %f" sw.Elapsed.TotalMilliseconds
