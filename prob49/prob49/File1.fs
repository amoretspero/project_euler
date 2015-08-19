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
printfn "Prime generation to %d Done! (Elapsed Time : %fms)" prime_gen_limit prime_gen_sw.Elapsed.TotalMilliseconds

let rec permutation_gen (n : int) =
    let res = ref ([] : int list list)


let find_permutations (n : int) =
    let digit = n.ToString().Length
    let digit_lst = List.ofArray(n.ToString().ToCharArray()) |> List.map (fun c -> int(c.ToString()))
    for i=0 to digit-1 do
        


let find_answer (digit : int) =
    let start_num = int32(System.Math.Pow(10.0f, (float (digit-1))))
    let end_num = (int32(System.Math.Pow(10.0f, (float digit)))) - 1
    for i=start_num to end_num do
        