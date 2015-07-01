module File1

open System
open System.Collections
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Linq
open System.Numerics

let prime_set = new HashSet<int>()

let circular_prime_set = new HashSet<int>()

let mutable prime_set_end = -1

let prime_gen (n : int) =
    for i=2 to n do
        if i=2 then
            prime_set.Add(i) |> ignore
        else if i%2 <> 0 then
            let mutable res = true
            for elem in prime_set do
                if i%elem=0 then
                    res <- false
                else
                    res <- res
            if res then
                prime_set.Add(i) |> ignore

let mutable lim = -1

let rotation_gen (n : int) (rot_num : int) =
    let mutable res = 0
    for i=0 to n.ToString().Length-1 do
        res <- res + Convert.ToInt32(n.ToString().[(i+rot_num)%n.ToString().Length].ToString()) * Convert.ToInt32(System.Math.Pow(float 10, float (n.ToString().Length-1 - i)))
    res

printfn "Enter lim : "
lim <- Convert.ToInt32(Console.ReadLine())

let sw_prime_gen = new Stopwatch()
sw_prime_gen.Start()
prime_gen(lim)
sw_prime_gen.Stop()
printfn "prime_gen finished to %d. Elapsed time : %d" lim sw_prime_gen.ElapsedMilliseconds
prime_set_end <- lim

let check_circular_prime (n : int) =
    if prime_set_end < n then
        prime_gen(n)
    if prime_set.Contains(n) then
        let mutable res = true
        for i=0 to n.ToString().Length-1 do
            let curr_num = rotation_gen n i
            if prime_set.Contains(curr_num) then
                res <- res
            else
                res <- false
        res
    else
        false

printfn "Now check circular primes"
let sw_circular_prime_check = new Stopwatch()
sw_circular_prime_check.Start()
for i=1 to lim do
    if (check_circular_prime i) then
        circular_prime_set.Add(i) |> ignore
sw_circular_prime_check.Stop()

printfn "Number of circular primes under %d : %d - Elapsed time : %d" lim circular_prime_set.Count sw_circular_prime_check.ElapsedMilliseconds