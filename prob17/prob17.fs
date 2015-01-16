open System
open System.Diagnostics
open System.Collections
open System.Collections.Generic
open System.Numerics

let sw = Stopwatch()
sw.Start()

exception INVALID_INPUT

let rec get_eng_word (n : int) =
    match (Convert.ToString(n)).Length with
    | 0 -> raise INVALID_INPUT
    | 1 ->
        match n with
        0 -> ""
        | 1 -> "one"
        | 2 -> "two"
        | 3 -> "three"
        | 4 -> "four"
        | 5 -> "five"
        | 6 -> "six"
        | 7 -> "seven"
        | 8 -> "eight"
        | 9 -> "nine"
        | _ -> raise INVALID_INPUT
    | 2 ->
        match n with
        10 -> "ten"
        | 11 -> "eleven"
        | 12 -> "twelve"
        | 13 -> "thirteen"
        | 14 -> "fourteen"
        | 15 -> "fifteen"
        | 16 -> "sixteen"
        | 17 -> "seventeen"
        | 18 -> "eighteen"
        | 19 -> "nineteen"
        | 20 -> "twenty"
        | _ when (n>=21)&&(n<=29) -> "twenty"+(get_eng_word (n-20))
        | 30 -> "thirty"
        | _ when (n>=31)&&(n<=39) -> "thirty"+(get_eng_word (n-30))
        | 40 -> "forty"
        | _ when (n>=41)&&(n<=49) -> "forty"+(get_eng_word (n-40))
        | 50 -> "fifty"
        | _ when (n>=51)&&(n<=59) -> "fifty"+(get_eng_word (n-50))
        | 60 -> "sixty"
        | _ when (n>=61)&&(n<=69) -> "sixty"+(get_eng_word (n-60))
        | 70 -> "seventy"
        | _ when (n>=71)&&(n<=79) -> "seventy"+(get_eng_word (n-70))
        | 80 -> "eighty"
        | _ when (n>=81)&&(n<=89) -> "eighty"+(get_eng_word (n-80))
        | 90 -> "ninety"
        | _ when (n>=91)&&(n<=99) -> "ninety"+(get_eng_word (n-90))
        | _ -> raise INVALID_INPUT
    | 3 ->
        if (n%100=0) then
            (get_eng_word (Convert.ToInt32(Convert.ToString((Convert.ToString(n)).[0]))))+"hundred"
        else
            (get_eng_word (Convert.ToInt32(Convert.ToString((Convert.ToString(n)).[0]))))+"hundredand"+(get_eng_word (n%100))
    | 4 ->
        if (n%1000=0) then
            (get_eng_word (Convert.ToInt32(Convert.ToString((Convert.ToString(n)).[0]))))+"thousand"
        else
            (get_eng_word (Convert.ToInt32(Convert.ToString((Convert.ToString(n)).[0]))))+"thousandand"+(get_eng_word (n%1000))
    | _ -> raise INVALID_INPUT
    
let rec word_counter (start_num : int) (end_num : int) =
    match start_num with
    | _ when start_num < end_num -> ((get_eng_word start_num).Length) + (word_counter (start_num+1) end_num)
    | _ when start_num = end_num -> ((get_eng_word start_num).Length)
    | _ when start_num > end_num -> raise INVALID_INPUT
    | _ -> raise INVALID_INPUT
    
let answer = (word_counter 1 1000)

sw.Stop()
printfn "Answer : %d" answer
printfn "Elapsed Time : %f" sw.Elapsed.TotalMilliseconds