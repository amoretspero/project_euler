open System
open System.Diagnostics

let sw = Stopwatch()
sw.Start()

let check_palindrome (n : int) =
    match n with
    | _ when n < 0 -> false
    | 0 -> false
    | _ ->
        let str = Convert.ToString(n)
        let len = str.Length
        match len with
        | _ when len%2<>0 -> false
        | _ -> 
            let mutable res = true
            for i=0 to (len/2)-1 do
                if str.[i]<>str.[len-1-i] then res <- false
            res

let first_elem (a, b, c) = a
let second_elem (a, b, c) = b
let third_elem (a, b, c) = c

let get_max_tuple_list (lst : (int * int * int) list) =
    let mutable res = 0
    for i=0 to (lst.Length - 1) do
        if (first_elem lst.[i])>(first_elem lst.[res]) then res <- i
    res
                                    
let res = ref []
for num1=100 to 999 do
    for num2=100 to 999 do
        if (check_palindrome (num1*num2)) then
            res := ((num1*num2), num1, num2) :: (!res)
let answer = (!res).[(get_max_tuple_list (!res))]

sw.Stop()
printfn "Answer : %d = %d * %d" (first_elem answer) (second_elem answer) (third_elem answer)
printfn "Elapsed Time : %f(ms)" sw.Elapsed.TotalMilliseconds