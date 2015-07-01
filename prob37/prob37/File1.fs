module File1

open System
open System.Collections
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Linq
open System.Numerics

let prime_set = new HashSet<int>()

let mutable prime_gen_gap = -1

let mutable prime_set_end = 0

let prime_gen (n : int) =
    printfn "Generating prime to %d" n
    prime_set_end <- n
    for i=2 to n do
        if i=2 then
            prime_set.Add(i) |> ignore
        else if i%2<>0 then
            let mutable res = true
            let mutable cnt = 0
            let mutable loop_break = false
            let n_square_root = Convert.ToInt32(System.Math.Sqrt(float n))
            while cnt < prime_set.Count && (not loop_break) do
                let elem = prime_set.ElementAt(cnt)
                if i%elem = 0 then
                    res <- false
                else
                    res <- res
                if n_square_root < elem then
                    loop_break <- true
                cnt <- cnt + 1
            if res then
                prime_set.Add(i) |> ignore

let rec check_truncate_prime (n : int) =
    if prime_set_end < n then
        prime_gen(prime_set_end + prime_gen_gap)
    if prime_set.Contains(n) && n > 7 then
        let mutable res = true
        let digit = n.ToString().Length
        for i=0 to digit-2 do
            let left_trunc = n/(Convert.ToInt32(System.Math.Pow(float 10, float (digit - 1 - i))))
            let right_trunc = n%(Convert.ToInt32(System.Math.Pow(float 10, float (digit - 1 - i))))
            //printfn "left_trunc : %d, right_trunc : %d" left_trunc right_trunc
            if res then
                res <- (prime_set.Contains(left_trunc)) && (prime_set.Contains(right_trunc))
            else
                res <- res
        res
    else
        false

let sw = new Stopwatch()
let mutable prime_cnt = 0
let mutable cnt = 1
let mutable num_of_truncate_primes = -1

printfn "Insert number of truncate primes to find(MAX : 11)"
num_of_truncate_primes <- Convert.ToInt32(Console.ReadLine())
printfn "Insert prime number generation gap : "
prime_gen_gap <- Convert.ToInt32(Console.ReadLine())
if num_of_truncate_primes > 11 then
    printfn "Adjusting your input to 11"
    num_of_truncate_primes <- 11

sw.Start()
while prime_cnt < num_of_truncate_primes do
    if (check_truncate_prime cnt) then
        printfn "Found truncate prime : %d - Elapsed Time : %d(ms)" cnt sw.ElapsedMilliseconds
        prime_cnt <- prime_cnt + 1
    cnt <- cnt + 1
sw.Stop()
printfn "Found %d truncate primes - Elapsed Time : %d(ms)" num_of_truncate_primes sw.ElapsedMilliseconds
