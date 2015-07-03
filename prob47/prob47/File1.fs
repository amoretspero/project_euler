module File1

open System
open System.Collections
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Linq
open System.Numerics

let prime_set = new HashSet<int>
let prime_set_end = ref 0

let find_prime_factors (n : int) =
    let sqrt_n = Convert.ToInt32(System.Math.Sqrt(n))
    let mutable num = n
    let mutable cnt = 0
    let mutable loop_break = false
    let ans = new HashSet<int>()
    while (n <> 1)&&(not loop_break)&&(cnt < prime_set.Count) do
        let elem = prime_set.ElementAt(cnt)
