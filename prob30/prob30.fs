open System
open System.Collections
open System.Diagnostics
open System.IO
open System.Linq
open System.Numerics
open System.Collections.Generic

let sw = Stopwatch()
sw.Start()

exception INVALID_INPUT

let get_nth_powers (n : int) =
    [ for elem in [0 .. 9] -> ((int32 ((float elem)**(float n))), elem)]

let rec list_flat (lst : 'a list list) =
    match lst with
    | [] -> []
    | [ elem ] -> elem
    | h :: t -> List.append h (list_flat t)

let rec list_distribute (lst : 'a list list) (elem : 'a) =
    [ for sub_lst in lst -> elem :: sub_lst]

let rec get_n_permutation (lst : 'a list) (n : int) =
    match n with
    1 -> [ for elem in lst -> [ elem ]]
    | _ when n>=2 -> 
        let res = ref []
        for elem in lst do
            let prev_res = (get_n_permutation (List.ofSeq(lst.Except([elem]))) (n-1))
            res := List.append (!res) (list_distribute prev_res elem)
        !res
    | _ -> raise INVALID_INPUT

let rec get_n_seqs (lst : 'a list) (n : int) =
    match n with
    | _ when n>=2 ->
        let res = ref []
        for elem in lst do
            let prev_res = (get_n_seqs lst (n-1))
            res := List.append (!res) (list_distribute prev_res elem)
        !res
    | 1 -> [ for elem in lst -> [ elem ]]
    | _ -> raise INVALID_INPUT

let rec int_lst_to_int (lst : int list) =
    let mutable res = ""
    for i=0 to lst.Length-1 do
        if i=0 && lst.[i]=0 then
            res <- res
        else
            res <- res+lst.[i].ToString()
    (int32 res)

let find_answer (n : int) =
    let power_list = (get_nth_powers n)
    let power_sum = (fst (List.unzip(power_list))).Sum()
    let power_sum_digit = power_sum.ToString().Length
    let numbers = (get_n_seqs power_list power_sum_digit)
    let res = ref []
    for elem in numbers do
        //printfn "elem : %d" (int_lst_to_int (snd (List.unzip(elem))))
        if (fst (List.unzip(elem))).Sum() = int32(int_lst_to_int (snd (List.unzip(elem)))) then res := (int_lst_to_int (snd (List.unzip(elem)))) :: (!res)
    (!res) |> List.filter (fun s -> s>=10) |> List.sum

let answer = find_answer 5

sw.Stop()

printfn "Answer : %d" answer
printfn "Elapsed Time : %f" sw.Elapsed.TotalMilliseconds