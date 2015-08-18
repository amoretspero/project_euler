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

let check_prime_factor (n : int) (lst : prime_factor list) =
    let pf_res = prime_factor_mult 1 lst
    if n <> pf_res then false else true

let factored_set = new System.Collections.Hashtable()

let prime_factor_adder (factor : int) (exp : int) (lst : prime_factor list) =
    let lst_copied = List.foldBack (fun elem acc -> elem::acc) lst []
    let mutable cnt = 0
    let mutable loop_break = false
    while (cnt < lst_copied.Length) && (not loop_break) do
        if (lst_copied.[cnt].basis = factor) then
            lst_copied.[cnt].exp <- lst_copied.[cnt].exp + exp
            loop_break <- true
        cnt <- cnt + 1
    let res = 
        if (not loop_break) then
            List.append lst_copied [prime_factor(factor, exp)]
        else
            lst_copied
    res

printfn "Data Initialization Done!\n"

let prime_gen_sw = new Stopwatch()
prime_gen_sw.Start()
prime_gen prime_num_insert prime_set_end prime_set
prime_gen_sw.Stop()

printfn "Prime generation to %d Done! (Elapsed Time : %fms)\n" prime_num_insert prime_gen_sw.Elapsed.TotalMilliseconds

/// find_prime_factors : given an integer, find its prime_factors and returns them as prime_factor list
let find_prime_factors (n : int) =
    //let sqrt_n = Convert.ToInt32(System.Math.Sqrt((float n)))
    let sqrt_n = n/2 + 1
    let mutable num = n
    let mutable cnt = 0
    let mutable loop_break = false
    let ans = ref ([] : prime_factor list)
    while (num > 1)&&(not loop_break)&&(cnt < prime_set.Count) do
        let elem = prime_set.ElementAt(cnt)
        let mutable exp = 0
        while (num > 1)&&(num%elem = 0) do
            num <- num/elem
            exp <- exp + 1
        if factored_set.ContainsKey(num) then
            let prev_pf = unbox (factored_set.Item(num))
            ans.Value <- prime_factor_adder elem exp prev_pf
            loop_break <- true
        else
            if exp <> 0 then
                ans.Value <- List.append ans.Value [prime_factor(elem, exp)]
            //if (prime_set.ElementAt(cnt + 1) > sqrt_n)||(num=1) then
            if (elem > sqrt_n) || (num=1) then
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
