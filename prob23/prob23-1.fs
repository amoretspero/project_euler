open System
open System.Collections
open System.Diagnostics
open System.Numerics
open System.Collections.Generic
open System.Linq
open System.Threading

let sw = Stopwatch()
sw.Start()

let lst_distinct (lst : 'a list) =
    let res = ref []
    for elem in lst do
        if (!res).Contains(elem) then res := (!res) else res := elem::(!res)
    !res

let get_divisor_sum (n : int) =
    let lst = ref []
    for i=1 to (int32(sqrt(float(n)))) do
        if (n%i=0) then lst := i::(n/i)::(!lst)
    List.sum(List.filter (fun s -> s<>n) (!lst))

let det_num (dict1 : Dictionary<int, int>) (dict2 : Dictionary<int, int>) (dict3 : Dictionary<int, int>) (n : int) =
    let sum = get_divisor_sum n
    if sum>n then dict3.Add((n, sum))
    elif sum=n then dict2.Add((n, sum))
    else dict1.Add((n, sum))