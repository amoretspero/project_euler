open System
open System.Collections
open System.Diagnostics
open System.Numerics
open System.Collections.Generic
open System.Linq

let sw = Stopwatch()
sw.Start()

let lst_distinct (lst : 'a list) =
    let res = ref []
    for elem in lst do
        if (!res).Contains(elem) then res := (!res) else res := elem::(!res)
    (!res)

let factorize (n : int) =
    let res = ref []
    for i=1 to int(sqrt(float(n))) do
        if n%i=0 then res := i::(n/i)::(!res)
    List.filter (fun s -> s<>n) (List.sort(lst_distinct (!res)))

let dict_get_keys (dict : Dictionary<'a, 'b>) (v : 'b) =
    let res = ref []
    for elem in dict do
        if elem.Value=v then res := elem.Key::(!res)
    List.sort(!res)

let get_amicable_num (l : int) =
    let factorize_mem = new Dictionary<int, int>()
    let amicable_lst = ref []
    for i=1 to l do
        let div_sum = List.sum((factorize i))
        let value_ref = ref 0
        if factorize_mem.ContainsValue(i) then
            let keys = (dict_get_keys factorize_mem i)
            for elem in keys do
                factorize_mem.TryGetValue(elem, value_ref) |> ignore
                if ((!value_ref)=i)&&(elem=div_sum) then amicable_lst := elem::i::(!amicable_lst)
            factorize_mem.Add(i, div_sum)
        else
            factorize_mem.Add(i, div_sum)
    (!amicable_lst)

let answer = List.sum(get_amicable_num 9999)

sw.Stop()
printfn "Answer : %d" answer
printfn "Elapsed Time : %f" sw.Elapsed.TotalMilliseconds
