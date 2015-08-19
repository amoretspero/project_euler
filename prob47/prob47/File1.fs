module File1

open System
open System.Collections
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Linq
open System.Numerics

open prime_funcs

exception EMPTY_LIST

let prime_set = new HashSet<int>()
let prime_set_end = ref 0

let mutable prime_num_insert = -1
printfn "Enter prime_num_insert : "
prime_num_insert <- System.Convert.ToInt32(System.Console.ReadLine())

let mutable prime_factor_insert = -1
printfn "Enter prime_factor_insert : "
prime_factor_insert <- System.Convert.ToInt32(System.Console.ReadLine())

let rec print_list (lst : prime_factor list) =
    match lst with
    [] -> raise EMPTY_LIST
    | [ elem ] -> printfn "%s " (elem.ToString())
    | h :: t ->
        printf "%s " (h.ToString())
        print_list t

let rec prime_factor_mult (n : int) (lst : prime_factor list) =
    match lst with
    [] -> raise EMPTY_LIST
    | [elem] -> n * (int (System.Math.Pow(float elem.basis, float elem.exp)))
    | h :: t ->
        prime_factor_mult (n * (int (System.Math.Pow(float h.basis, float h.exp)))) t

let sort_func = (fun (x : prime_factor) (y : prime_factor) -> 
                    if x.basis > y.basis then 1 
                    else if x.basis < y.basis then 0 
                    else 
                        if x.exp >= y.exp then 1
                        else 0)

let check_prime_factor (n : int) (lst : prime_factor list) =
    //let sorted_lst = List.sortWith sort_func lst
    let basis_lst = (List.map (fun (x : prime_factor) -> x.basis) lst).Distinct()
    let pf_res = prime_factor_mult 1 lst
    if n <> pf_res then 
        false 
    else 
        if basis_lst.Count() <> lst.Count() then
            false
        else
            true

let rec list_copy (lst : 'a list) =
    let res = ref ([] : 'a list)
    for i=0 to lst.Length-1 do
        res.Value <- List.append res.Value [lst.[i]]
    res.Value

let factored_set = new System.Collections.Hashtable()

let prime_factor_adder (factor : int) (exp : int) (lst : prime_factor list) =
    //let lst_copied = List.foldBack (fun elem acc -> elem::acc) lst []
    //let lst_copied = list_copy lst
    let mutable cnt = 0
    let mutable loop_break = false 
    let res = ref ([] : prime_factor list)
    while (cnt < lst.Length) do
        if (lst.[cnt].basis = factor) then
            //lst.[cnt].exp <- lst.[cnt].exp + exp
            //loop_break <- true
            res.Value <- List.append res.Value [prime_factor(lst.[cnt].basis, lst.[cnt].exp+exp)]
            loop_break <- true
        else
            res.Value <- List.append res.Value [prime_factor(lst.[cnt].basis, lst.[cnt].exp)]
        cnt <- cnt + 1
    if (not loop_break) then
        res.Value <- List.append res.Value [prime_factor(factor, exp)]
    (*let res = 
        if (not loop_break) then
            List.append lst_copied [prime_factor(factor, exp)]
        else
            lst_copied*)
    res.Value

printfn "Data Initialization Done!\n"

let prime_gen_sw = new Stopwatch()
prime_gen_sw.Start()
prime_gen prime_num_insert prime_set_end prime_set
let prime_lst = prime_set.ToList()
prime_gen_sw.Stop()

printfn "Prime generation to %d Done! (Elapsed Time : %fms)\n" prime_num_insert prime_gen_sw.Elapsed.TotalMilliseconds

/// find_prime_factors : given an integer, find its prime_factors and returns them as prime_factor list
let find_prime_factors(n : int) =
    let sqrt_n = n/2 + 1
    let mutable num = n
    let mutable cnt = 0
    let mutable loop_break = false
    let ans = ref ([] : prime_factor list)
    while (num > 1)&&(not loop_break)&&(cnt < prime_set.Count) do
        //let elem = prime_set.ElementAt(cnt)
        let elem = prime_lst.[cnt]
        let mutable exp = 0
        while (num > 1)&&(num%elem = 0)&&(not loop_break) do
            num <- num/elem
            exp <- exp+1
            if (num > 1) then
                let prev_pf = unbox (factored_set.Item(num))
                ans.Value <- prime_factor_adder elem exp prev_pf
                loop_break <- true
        if (elem > sqrt_n) || (num = 1) then
            loop_break <- true
        cnt <- cnt + 1
    if ans.Value.Length = 0 then
        ans.Value <- [prime_factor(n, 1)]
    factored_set.Add(n, ans.Value)
    ans.Value

let prime_factor_sw = new Stopwatch()
prime_factor_sw.Start()
let mutable error_count = 0
for i=2 to prime_factor_insert do
    //find_prime_factors i |> ignore
    let res = find_prime_factors i
    (*printf "Prime factors of %d is : " i
    print_list res*)
    let pf_check = (check_prime_factor i res)
    if (not pf_check) then
        error_count <- error_count + 1
    //printfn "Check correctness : %s" (pf_check.ToString())
prime_factor_sw.Stop()
printfn "Prime factor generation Done! (Elapsed Time : %fms) (Error : %d)\n" prime_factor_sw.Elapsed.TotalMilliseconds error_count

let check_ans_sw = new Stopwatch()
let mutable ans = -1
let check_ans () =
    let mutable cnt = 2
    let mutable loop_break = false
    let pf1 = ref ([] : prime_factor list)
    let pf2 = ref ([] : prime_factor list)
    let pf3 = ref ([] : prime_factor list)
    let pf4 = ref ([] : prime_factor list)
    while (cnt < prime_factor_insert - 3) && (not loop_break) do
        pf1.Value <- pf2.Value
        pf2.Value <- pf3.Value
        pf3.Value <- pf4.Value
        pf4.Value <- unbox (factored_set.Item(cnt))
        if pf1.Value.Length = 4 then
            if pf2.Value.Length = 4 then
                if pf3.Value.Length = 4 then
                    if pf4.Value.Length = 4 then
                        let temp = (List.append (List.append (List.append pf1.Value pf2.Value) pf3.Value) pf4.Value).Distinct()
                        if temp.Count() = 16 then
                            ans <- cnt-3
                            loop_break <- true
        cnt <- cnt + 1

check_ans_sw.Start()
check_ans()
check_ans_sw.Stop()
if ans <> -1 then
    printfn "Answer found : %d %d %d %d (Elapsed Time : %fms)\n" ans (ans + 1) (ans + 2) (ans + 3) check_ans_sw.Elapsed.TotalMilliseconds
else
    printfn "Answer not found (Elapsed Time : %fms)\n" check_ans_sw.Elapsed.TotalMilliseconds

(*
===================
Answer : 134043
Elapsed Time : 
    Prime generation : 240.191600ms (Input : 200000)
    Prime factor generation : 1498.968900ms (Input : 200000) (Error : 0)
    Answer search : 8.129500ms
System : 
    CPU : i7-4790K (4.0GHz - Turbo 4.4GHz)
    RAM : DDR3-12800, 16GB @1600MHz
    GPU : AMD RADEON 290X
===================
*)