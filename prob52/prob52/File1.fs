module File1

open System
open System.Collections
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Linq
open System.Numerics

let num_to_digit (n : int) =
    List.map (fun x -> int(x.ToString())) (List.ofArray(n.ToString().ToCharArray()))

let rec digit_compare (lst1 : int list) (lst2 : int list) =
    match lst1 with
    | [] ->
        match lst2 with
        | [] -> true
        | _ -> false
    | h1 :: t1 ->
        match lst2 with
        | [] -> false
        | h2 :: t2 ->
            if h1 = h2 then
                digit_compare t1 t2
            else
                false


let check_ans (mult : int) (limit : int) =
    let mutable cnt = 1
    let mutable loop_break = false
    while (not loop_break) && (cnt < limit) do
        let mutable multiple = cnt
        let mutable mult_cnt = 0
        let mutable mult_loop_break = false
        let digit_lst = ref (List.sort(num_to_digit multiple))
        while mult_cnt < mult && (not mult_loop_break) do
            let temp = List.sort(num_to_digit multiple)
            if (not (digit_compare digit_lst.Value temp)) then
                mult_loop_break <- true
            else
                mult_cnt <- mult_cnt + 1
                multiple <- multiple + cnt
        if mult_loop_break then
            cnt <- cnt + 1
        else
            printfn "Found answer! : x = %d" cnt
            loop_break <- true
    cnt

let mutable number_of_multiples = -1
let mutable check_limit = -1
printfn "Insert number_of_multiples :"
number_of_multiples <- System.Convert.ToInt32(System.Console.ReadLine())
printfn "Insert check_limit :"
check_limit <- System.Convert.ToInt32(System.Console.ReadLine())

let find_ans_sw = new Stopwatch()
find_ans_sw.Start()
let ans = check_ans number_of_multiples check_limit
find_ans_sw.Stop()

printfn "\nSearch for answer for multiple to %d and to limit of %d done. (Elapsed Time : %fms)" number_of_multiples check_limit find_ans_sw.Elapsed.TotalMilliseconds

(*
===================
Answer : 142857
===================
*)