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
let mutable prime_set_limit = 0
let mutable number_of_primes = -1
printfn "Insert prime_set_limit :"
prime_set_limit <- System.Convert.ToInt32(System.Console.ReadLine())
let prime_set_sw = new Stopwatch()
printfn "\nNow generating prime to %d\n" prime_set_limit
prime_set_sw.Start()
prime_gen prime_set_limit prime_set_end prime_set
prime_set_sw.Stop()
printfn "Generating prime to %d done. (Elapsed Time : %fms)" prime_set_limit prime_set_sw.Elapsed.TotalMilliseconds

let prime_lst = List.ofSeq(prime_set.ToList())
let prime_lst_len = prime_lst.Length

let change_digit_sub (num : int list) (location : int list) (ch_digit : int) =
    [for i in 0 .. num.Length-1 -> if location.Contains(i) then ch_digit else num.[i]]

let lst_to_int32 (num : int list) =
    let mutable res = 0
    let len = num.Length
    for i=0 to len - 1 do
        res <- res + (int (System.Math.Pow(float 10, float (len - 1 - i)))) * num.[i]
    res

/// Change digits specified by location and returns numbers with changed digits and which is also prime.
let change_digit (num : int) (location : int list) =
    let digit = List.map (fun (x : char) -> (int)(x.ToString())) (List.ofArray(num.ToString().ToCharArray()))
    let digit_min = (int)(System.Math.Pow(float 10, float (digit.Length - 1)))
    let mutable digit_check = true
    let mutable digit_check_init = digit.[location.[0]]
    for elem in location do
        if digit.[elem] <> digit_check_init then digit_check <- false else digit_check <- digit_check
    if digit_check then
        let temp = List.filter (fun x -> prime_set.Contains(x) && x >= digit_min) (List.map (fun x -> lst_to_int32 x) [for i in 0 .. 9 -> change_digit_sub digit location i])
        temp
    else
        ([] : int list)

/// Given how many elements to be selected(num), elements to select(loc), initial list(res=[[]]), returns (loc.Length)C(res).
let rec combination_gen (num : int) (loc : int list) (res : int list list) =
    match num with
    | 0 -> res
    | _ -> 
        let temp = ref ([] : int list list)
        for elem in res do
            let available_locs = List.except elem loc
            for i=0 to available_locs.Length-1 do
                temp.Value <- List.append temp.Value (combination_gen (num - 1) (List.ofSeq(available_locs.Skip(i))) [(available_locs.[i] :: elem)])
        temp.Value
        
printfn "Insert number_of_primes :"
number_of_primes <- System.Convert.ToInt32(System.Console.ReadLine())

let check_ans (digit : int) =
    let len = digit.ToString().Length
    let mutable ans_found = false
    for i=1 to len-1 do
        let combination = combination_gen i [0 .. len-1] [[]]
        for elem in combination do
            let change_res = change_digit digit elem
            if change_res.Length = number_of_primes then
                printfn "Found answer! : Base prime - %d" digit
                printfn "%d primes are :" number_of_primes
                for elem in change_res do
                    printf "%d " elem
                    printf "\n"
                ans_found <- true
            (*else
                prime_lst_mutable.Value <- List.except change_res prime_lst_mutable.Value*)
    ans_found

let mutable check_limit = -1
printfn "Insert check_limit :"
check_limit <- System.Convert.ToInt32(System.Console.ReadLine())
let find_ans_sw = new Stopwatch()
find_ans_sw.Start()
let mutable cnt = 0
let mutable while_break = false
while cnt < prime_lst_len && (not while_break) do
    let pr = prime_lst.[cnt]
    let res = check_ans pr
    while_break <- res
    if pr > check_limit then while_break <- true
    cnt <- cnt + 1
find_ans_sw.Stop()
printfn "Finding answer done. (Elapsed Time : %fms)" find_ans_sw.Elapsed.TotalMilliseconds