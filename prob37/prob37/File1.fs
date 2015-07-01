module File1

open System
open System.Collections
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Linq
open System.Numerics

let prime_set = new HashSet<int>()

let mutable prime_set_end = 0

let prime_gen (n : int) =
    prime_set_end <- n
    for i=2 to n do
        if i=2 then
            prime_set.Add(i) |> ignore
        else if i%2<>0 then
            let mutable res = true
            for elem in prime_set do
                if i%elem = 0 then
                    res <- false
                else
                    res <- res
            if res then
                prime_set.Add(i) |> ignore

let rec check_truncate_prime (n : int) =
    if prime_set_end < n then
        prime_gen(n)
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
