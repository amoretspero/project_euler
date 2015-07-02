module File1

open System
open System.Collections
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Linq
open System.Numerics

let list_random_pick (lst : int list) =
    let length = lst.Length
    let rand = Random()
    lst.[rand.Next()%length]

let list_random_pick_nonzero (lst : int list) =
    let length = lst.Length
    let rand = Random()
    let rand_num = rand.Next()%length
    let mutable elem = lst.[rand_num]
    if elem = 0 then
        elem <- lst.[(rand_num+1)%length]
    elem

let mutable res = 0L

let rec find_ans (curr_num : int64) (lst : int list) =
    match lst.Length with
    10 ->
        //printfn "lst.Length = 10 - curr_num=%d" curr_num
        let elem = list_random_pick_nonzero lst 
        for digit in lst do
            if digit<>0 then
                let next_num = curr_num * 10L + (int64)digit
                find_ans next_num (List.ofSeq(lst.Except([digit])))
    | 9 ->
        //printfn "lst.Length = 9 - curr_num=%d" curr_num
        let elem = list_random_pick lst
        for digit in lst do
            let next_num = curr_num * 10L + (int64)digit
            find_ans next_num (List.ofSeq(lst.Except([digit])))
    | 8 ->
        //printfn "lst.Length = 8 - curr_num=%d" curr_num
        let elem = list_random_pick lst
        for digit in lst do
            let next_num = curr_num * 10L + (int64)digit
            find_ans next_num (List.ofSeq(lst.Except([digit])))
    | 7 ->
        //printfn "lst.Length = 7 - curr_num=%d" curr_num
        let elem = list_random_pick lst
        for digit in lst do
            if digit%2 = 0 then
                let next_num = curr_num * 10L + (int64)digit
                find_ans next_num (List.ofSeq(lst.Except([digit])))
    | 6 ->
        //printfn "lst.Length = 6 - curr_num=%d" curr_num
        for digit in lst do
            let next_num = curr_num * 10L + (int64)digit
            let det = Convert.ToInt32(next_num.ToString().Substring(2, 3))
            if det%3 = 0 then
                find_ans next_num (List.ofSeq(lst.Except([digit])))
    | 5 ->
        //printfn "lst.Length = 5 - curr_num=%d" curr_num
        for digit in lst do
            if digit=5 || digit=0 then
                let next_num = curr_num * 10L + (int64)digit
                find_ans next_num (List.ofSeq(lst.Except([digit])))
    | 4 ->
        //printfn "lst.Length = 4 - curr_num=%d" curr_num
        for digit in lst do
            let next_num = curr_num * 10L + (int64)digit
            let det = Convert.ToInt32(next_num.ToString().Substring(4, 3))
            if det%7=0 then
                find_ans next_num (List.ofSeq(lst.Except([digit])))
    | 3 ->
        //printfn "lst.Length = 3 - curr_num=%d" curr_num
        for digit in lst do
            let next_num = curr_num * 10L + (int64)digit
            let det = Convert.ToInt32(next_num.ToString().Substring(5, 3))
            if det%11=0 then
                find_ans next_num (List.ofSeq(lst.Except([digit])))
    | 2 ->
        //printfn "lst.Length = 2 - curr_num=%d" curr_num
        for digit in lst do
            let next_num = curr_num * 10L + (int64)digit
            let det = Convert.ToInt32(next_num.ToString().Substring(6, 3))
            if det%13=0 then
                find_ans next_num (List.ofSeq(lst.Except([digit])))
    | 1 ->
        //printfn "lst.Length = 1 - curr_num=%d" curr_num
        for digit in lst do
            let next_num = curr_num * 10L + (int64)digit
            let det = Convert.ToInt32(next_num.ToString().Substring(7, 3))
            if det%17=0 then
                find_ans next_num (List.ofSeq(lst.Except([digit])))
    | 0 ->
        printfn "Sub-string divisibility satisfacting number found! - number : %d" curr_num 
        res <- res + curr_num
    | _ -> res <- res

find_ans 0L [0; 1; 2; 3; 4; 5; 6; 7; 8; 9]

printfn "Answer : %d" res