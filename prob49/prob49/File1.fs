module File1

open System
open System.Collections
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Linq
open System.Numerics

open prime_funcs

let prime_set_end = ref 0
let mutable prime_gen_limit = -1

printfn "Insert prime_gen_limit : "
prime_gen_limit <- System.Convert.ToInt32(System.Console.ReadLine())

let prime_set = new HashSet<int>()

let prime_gen_sw = new Stopwatch()
prime_gen_sw.Start()
prime_gen prime_gen_limit prime_set_end prime_set
prime_gen_sw.Stop()
printfn "Prime generation to %d done! (Elapsed Time : %fms)" prime_gen_limit prime_gen_sw.Elapsed.TotalMilliseconds

let get_n_base (n : int) (b : int) =
    let mutable num = n
    let res = ref ([] : int list)
    while num >= b do
        let remainder = num%b
        num <- num/b
        res.Value <- List.append res.Value [remainder]
    if num <> 0 then
        res.Value <- List.append res.Value [num]
    if n=0 then
        res.Value <- List.append res.Value [0]
    List.ofSeq(res.Value.Reverse())

let rec zero_append (n : int) (lst : int list) =
    let temp = [ for i in 1 .. (n - lst.Length) -> 0]
    List.append temp lst

/// Generates permutations of numbers from 0 to n-1.
let rec permutation_gen (n : int) =
    let res = [ for i in 1 .. (int (System.Math.Pow(float n, float n))) -> (get_n_base i n)]
    List.filter (fun (x : int list) -> x.Distinct().Count() = n) (List.map (fun (x : int list) -> if x.Length < n then (zero_append n x) else x) res)
        
/// Generates permutations of numbers from given number(n)'s digits.
let find_permutations (n : int) =
    let digit = n.ToString().Length
    let digit_lst = List.ofArray(n.ToString().ToCharArray()) |> List.map (fun c -> int(c.ToString()))
    let permutation_lst = permutation_gen digit
    let res = List.map (fun (x : int list) -> List.map (fun x -> digit_lst.[x]) x) permutation_lst
    res

let get_number_from_lst (lst : int list) =
    let len = lst.Length
    let mutable res = 0
    for i=len-1 downto 0 do
        res <- res + (lst.[i] * (int)(System.Math.Pow(float 10, float (len - i - 1))))
    res

/// Find answers for numbers that have given digit. Ex : if digit=3, 100 - 999
let find_answer (digit : int) =
    let start_num = int32(System.Math.Pow(10.0, (float (digit-1))))
    let end_num = (int32(System.Math.Pow(10.0, (float digit)))) - 1
    for i=start_num to end_num do
        if prime_set.Contains(i) then
            let permutation_lst = List.filter (fun x -> x >= start_num && x <= end_num) (List.map (fun (x : int list) -> get_number_from_lst x) (find_permutations i))
            let permutation_lst_prime = List.filter (fun x -> prime_set.Contains(x)) permutation_lst
            if permutation_lst_prime.Contains(i + 3330) then
                if permutation_lst_prime.Contains(i + 6660) then
                    printfn "Found Answer! - %d %d %d" i (i+3330) (i+6660)

let mutable insert_digit = -1
printfn "Insert digit to find answer :"
insert_digit <- System.Convert.ToInt32(System.Console.ReadLine())

let find_answer_sw = new Stopwatch()
find_answer_sw.Start()
find_answer insert_digit
find_answer_sw.Stop()

printfn "Finding answer for %d digit numbers done! (Elapsed Time : %fms)" insert_digit find_answer_sw.Elapsed.TotalMilliseconds

(*
===================
Answer : 2969
Input : 
    prime_gen_limit : 10000
    insert_digit : 4
Elapsed Time :
    Prime generation : 4.065700ms
    Answer search : 229.375800ms
System :
    CPU : i7-4790K (4.0GHz - Turbo 4.4GHz)
    RAM : DDR3-12800, 16GB @1600MHz
    GPU : AMD RADEON 290X
===================
*)