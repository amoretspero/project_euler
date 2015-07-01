module File1

open System
open System.Collections
open System.Collections.Generic
open System.IO
open System.Numerics
open System.Diagnostics
open System.Linq

let digit_facs = [1I; 1I; 2I; 6I; 24I; 120I; 720I; 5040I; 40320I; 362880I]

let rec fac (n : BigInteger) =
    match n with
    | _ when (n = 0I) -> 1I
    | _ -> n * (fac (n - 1I))

let get_digit_fac_sum (n : BigInteger) =
    let string_notation = n.ToString()
    let fac_sum = ref 0I
    String.iter (fun c -> fac_sum.Value <- fac_sum.Value + digit_facs.[(Convert.ToInt32(c.ToString()))]) string_notation
    fac_sum.Value

let mutable num_start = -1
let mutable num_end = -1

let found_digits = new HashSet<BigInteger>()

printfn "Enter num_start : "
num_start <- Convert.ToInt32(Console.ReadLine())
printfn "Enter num_end : "
num_end <- Convert.ToInt32(Console.ReadLine())

for i=num_start to num_end do
    let num = BigInteger.Parse(i.ToString())
    let digit_fac_sum = get_digit_fac_sum(num)
    if num = digit_fac_sum && num <> 1I && num <> 2I then
        printfn "num : %s" (num.ToString())
        found_digits.Add(num) |> ignore

let res = ref 0I

for elem in found_digits do
    res.Value <- res.Value + elem

printfn "\nAnswer : %s\n" (res.Value.ToString())