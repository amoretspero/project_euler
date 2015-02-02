open System
open System.Collections
open System.Diagnostics
open System.IO
open System.Linq
open System.Numerics
open System.Collections.Generic

let sw = Stopwatch()
sw.Start()

let rec human_divide (numerator : int) (denominator : int) (digits : int) (digit_limit : int) =
    if digits = digit_limit then ""
    else
        let mutable integer_part = (numerator / denominator).ToString()
        if (digits = 0)&&(denominator*(int32 integer_part) <> numerator) then integer_part <- integer_part + "."
        if (denominator*(int32 (integer_part.Replace(".", "")))) = numerator then integer_part
        else
            let left_over = numerator - ((int32 (integer_part.Replace(".", ""))) * denominator)
            (integer_part.ToString())+(human_divide (left_over*10) denominator (digits+1) digit_limit)

let rec get_decimal (float_num : string) =
    if (not (float_num.Contains("."))) then ""
    else
        let mutable res = ""
        let mutable dec = false
        for i=0 to float_num.Length-1 do
            if float_num.[i].ToString() = "." then dec <- true
            else if dec then res <- res + float_num.[i].ToString()
            else res <- res
        res

let rec string_splitter (str : string) (num : int) =
    if str.Length >= num then
        str.Substring(0, num) :: (string_splitter (str.Substring(num)) num)
    else
        [ str ]

let rec det_circulator (float_num : string) =
    let decimal = get_decimal float_num
    let len = decimal.Length
    let mutable reciprocal = ""
    let mutable det = false
    let mutable cnt = 1
    while (cnt < (len/2))&&(not det) do
        let splitted = (string_splitter decimal cnt)
        let mutable det_while = true
        let mutable i=0
        let mutable stop = false
        while (i <= splitted.Length - 3)&&(not stop) do
            let mutable det_sub = true
            for j=i to System.Math.Min(splitted.Length - 2, i+3) do
                if splitted.[i] = splitted.[j] then det_sub <- det_sub else det_sub <- false
            if det_sub then 
                det_while <- true
                stop <- true 
                reciprocal <- splitted.[i]
            else det_while <- false
            i <- i + 1
        det <- det_while
        cnt <- cnt + 1
    printfn "length : %d" reciprocal.Length
    reciprocal

let find_longest_reciprocal (num : int) (limit : int) =
    let mutable len = 0
    let mutable res = 0
    for i=1 to num do
        let reciprocal = (det_circulator (human_divide 1 i 0 limit))
        printfn "i : %d, reciprocal : %s" i reciprocal
        if reciprocal.Length > len then
            len <- reciprocal.Length
            res <- i
    res
