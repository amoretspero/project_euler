open System
open System.Diagnostics

let sw = Stopwatch()
sw.Start()

let first_elem (a, _, _) = a
let second_elem (_, b, _) = b
let third_elem (_, _, c) = c

let rec get_pythagorean_triplet (n : int) =
    match n with
    | _ when n>=1 && n <= 5 -> [ (0, 0, 0) ]
    | _ ->
        let mutable a = 1
        let mutable b = 2
        let mutable c = 3
        let res = ref []
        while (a+b+c <= n) do
            while (a+b+c <= n) do
                while (a+b+c <= n) do
                    if (a*a + b*b = c*c) then res := (a, b, c) :: (!res)
                    //printfn "a : %d b : %d c : %d" a b c
                    c <- c + 1
                //printfn "a : %d b : %d c : %d" a b c
                b <- b + 1
                c <- b + 1
            //printfn "a : %d b : %d c : %d" a b c
            a <- a + 1
            b <- a + 1
            c <- b + 1
        !res
        
let mutable answer = (0, 0, 0)
for elem in (get_pythagorean_triplet 1000) do
    let sum = (first_elem elem)+(second_elem elem)+(third_elem elem)
    if sum=1000 then answer <- elem

sw.Stop()
printfn "Answer : %d (product of %d, %d %d)" ((first_elem answer)*(second_elem answer)*(third_elem answer)) (first_elem answer) (second_elem answer) (third_elem answer)
printfn "Elapsed Time : %f" sw.Elapsed.TotalMilliseconds