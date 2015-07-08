module File1

open System
open System.Collections
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Linq
open System.Numerics

open prime_funcs

type prime_factor (pow_base : int, pow_exp : int) =
    /// pow_base : Base of power.
    member pf.pow_base = pow_base
    /// pow_exp : Exponent of power.
    member pf.pow_exp = pow_exp
    /// ToString() : Gets the usual power notation (n ^ m).
    override pf.ToString() = pf.pow_base.ToString() + "^" + pf.pow_exp.ToString()

let prime_set = new HashSet<int>()
let prime_set_end = ref 0

let factored_set = new System.Collections.Hashtable()

let find_prime_factors (n : int) =
    let sqrt_n = Convert.ToInt32(System.Math.Sqrt(float n))
    let mutable num = n
    let mutable cnt = 0
    let mutable loop_break = false
    let ans = new HashSet<prime_factor>()
    while (num <> 1)&&(not loop_break)&&(cnt < prime_set.Count) do
        let elem = prime_set.ElementAt(cnt)
        let mutable exp_cnt = 0
        if num%elem = 0 then
            while num%elem=0 do
                num <- num/elem
                exp_cnt <- exp_cnt + 1
            ans.Add(prime_factor(elem, exp_cnt)) |> ignore
        cnt <- cnt + 1
    //factored_set.Add(n, ans)
    List.ofSeq(ans)

let find_ans (n : int) =
    (*if prime_set_end.Value < n then
        prime_gen (n+1) prime_set_end prime_set*)
    let set1 = ref ([] : prime_factor list)
    let set2 = ref ([] : prime_factor list)
    let set3 = ref ([] : prime_factor list)
    let set4 = ref ([] : prime_factor list)
    for i=2 to n-2 do
        printfn "finding answer : i = %d" i
        set1.Value <- set2.Value
        set2.Value <- set3.Value
        set3.Value <- set4.Value
        set4.Value <- find_prime_factors(i)
        let set1_cnt = set1.Value.Length
        let set2_cnt = set2.Value.Length
        let set3_cnt = set3.Value.Length
        let set4_cnt = set4.Value.Length
        if set1_cnt=4 then
            if set2_cnt=4 then 
                if set3_cnt=4 then
                    if set4_cnt=4 then
                        let set_app = List.append (List.append (List.append set1.Value set2.Value) set3.Value) set4.Value
                        let num_dist = set_app.Distinct().Count()
                        if set1_cnt+set2_cnt+set3_cnt+set4_cnt = num_dist then
                            printfn "Found three consecutive numbers to have three distinct prime factors : "
                            printf "%d : " i
                            for elem in set1.Value do
                                printf "%s " (elem.ToString())
                            printf "\n%d : " (i+1)
                            for elem in set2.Value do  
                                printf "%s " (elem.ToString())
                            printf "\n%d : " (i+2)
                            for elem in set3.Value do  
                                printf "%s " (elem.ToString())
                            printf "\n%d : " (i+3)
                            for elem in set4.Value do  
                                printf "%s " (elem.ToString())
                            printfn "\n"
                            File.WriteAllText("res.txt", i.ToString())
        
let mutable max_int_ans = -1
printfn "Enter the maximum natural number to search for answer :"
max_int_ans <- Convert.ToInt32(Console.ReadLine())
prime_gen max_int_ans prime_set_end prime_set
printfn "Prime gen done!"
find_ans(max_int_ans)

(*prime_gen 100000 prime_set_end prime_set
printfn "Prime factors of 120 is : "
let pf_24 = find_prime_factors(120)
for elem in pf_24 do
    printfn "%s" (elem.ToString())*)