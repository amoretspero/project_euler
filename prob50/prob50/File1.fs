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
printfn "Insert prime_set_limit :"
prime_set_limit <- System.Convert.ToInt32(System.Console.ReadLine())
printfn "\nNow starting prime generation to %d\n" prime_set_limit

let prime_gen_sw = new Stopwatch()
prime_gen_sw.Start()
prime_gen prime_set_limit prime_set_end prime_set
prime_gen_sw.Stop()
printfn "Prime Generation to %d done. (Elapsed Time : %fms)\n\n" prime_set_limit prime_gen_sw.Elapsed.TotalMilliseconds

let mutable answer_limit = -1
printfn "Insert answer_limit :"
answer_limit <- System.Convert.ToInt32(System.Console.ReadLine())

let prime_lst = List.ofSeq(prime_set.ToList())
let prime_lst_len = prime_lst.Length

let find_ans (limit : int) (cnt_start : int) =
    let mutable ans = 0
    let mutable len = 0
    let mutable prime_sum = 0
    let mutable cnt = cnt_start
    while ans < limit && cnt < prime_lst_len && prime_sum < limit do
        prime_sum <- prime_sum + prime_lst.[cnt]
        cnt <- cnt + 1
        if prime_set.Contains(prime_sum) then
            ans <- prime_sum
            len <- cnt - cnt_start
    [ans; len]

let find_ans_total (limit : int) =
    let mutable cnt = 0
    let mutable len = 0
    let mutable ans = 0
    while cnt < prime_lst_len && prime_lst.[cnt] < limit do
        let temp = find_ans limit cnt
        if temp.[1] > len then
            ans <- temp.[0]
            len <- temp.[1]
        cnt <- cnt + 1
    ans


printfn "\nNow finding answer.\n"
let find_ans_sw = new Stopwatch()
find_ans_sw.Start()
let res = find_ans_total answer_limit
find_ans_sw.Stop()
printfn "Finding answer to limit of %d done.\nAnswer : %d (Elapsed Time : %fms)" answer_limit res find_ans_sw.Elapsed.TotalMilliseconds

(*
===================
Answer : 997651
Input :
    prime_set_limit : 1000000
    answer_limit : 1000000
Elapsed Time : 
    Prime generation : 3155.481400ms
    Answer search : 13564.770300ms
System :
    CPU : i7-4790K (4.0GHz - Turbo 4.4GHz)
    RAM : DDR3-12800, 16GB @1600MHz
    GPU : AMD RADEON 290X
===================
*)