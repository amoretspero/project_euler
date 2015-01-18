open System
open System.Collections
open System.Diagnostics
open System.Numerics
open System.Collections.Generic
open System.Linq
open System.Threading
open System.IO

let sw = Stopwatch()
sw.Start()

let lst_distinct (lst : int list) =
    let res = ref []
    for elem in lst do
        if (!res).Contains(elem) then res := (!res) else res := elem::(!res)
    !res

let dict_deficient_1 = new Dictionary<int, int>()
let dict_perfect_1 = new Dictionary<int, int>()
let dict_abundant_1 = new Dictionary<int, int>()
let dict_deficient_2 = new Dictionary<int, int>()
let dict_perfect_2 = new Dictionary<int, int>()
let dict_abundant_2 = new Dictionary<int, int>()
let dict_deficient_3 = new Dictionary<int, int>()
let dict_perfect_3 = new Dictionary<int, int>()
let dict_abundant_3 = new Dictionary<int, int>()
let dict_deficient_4 = new Dictionary<int, int>()
let dict_perfect_4 = new Dictionary<int, int>()
let dict_abundant_4 = new Dictionary<int, int>()
let dict_deficient_5 = new Dictionary<int, int>()
let dict_perfect_5 = new Dictionary<int, int>()
let dict_abundant_5 = new Dictionary<int, int>()
let dict_deficient_6 = new Dictionary<int, int>()
let dict_perfect_6 = new Dictionary<int, int>()
let dict_abundant_6 = new Dictionary<int, int>()
let dict_deficient_7 = new Dictionary<int, int>()
let dict_perfect_7 = new Dictionary<int, int>()
let dict_abundant_7 = new Dictionary<int, int>()
let dict_deficient_8 = new Dictionary<int, int>()
let dict_perfect_8 = new Dictionary<int, int>()
let dict_abundant_8 = new Dictionary<int, int>()
let dict_deficient_9 = new Dictionary<int, int>()
let dict_perfect_9 = new Dictionary<int, int>()
let dict_abundant_9 = new Dictionary<int, int>()

let get_divisor_sum (n : int) =
    let lst = ref []
    for i=1 to (int32(sqrt(float(n)))) do
        if (n%i=0) then lst := i::(n/i)::(!lst)
    List.sum(lst_distinct(List.filter (fun s -> s<>n) (!lst)))

let det_num (dict1 : Dictionary<int, int>) (dict2 : Dictionary<int, int>) (dict3 : Dictionary<int, int>) (n : int) =
    let sum = get_divisor_sum n
    if sum>n then dict3.Add((n, sum))
    elif sum=n then dict2.Add((n, sum))
    else dict1.Add((n, sum))

let make_dict dict1 dict2 dict3 start_num end_num = for i=start_num to end_num do (det_num dict1 dict2 dict3 i)

(make_dict dict_deficient_1 dict_perfect_1 dict_abundant_1 1 28123)
(make_dict dict_deficient_2 dict_perfect_2 dict_abundant_2 1 28123)
(make_dict dict_deficient_3 dict_perfect_3 dict_abundant_3 1 28123)
(make_dict dict_deficient_4 dict_perfect_4 dict_abundant_4 1 28123)
(make_dict dict_deficient_5 dict_perfect_5 dict_abundant_5 1 28123)
(make_dict dict_deficient_6 dict_perfect_6 dict_abundant_6 1 28123)
(make_dict dict_deficient_7 dict_perfect_7 dict_abundant_7 1 28123)
(make_dict dict_deficient_8 dict_perfect_8 dict_abundant_8 1 28123)
(make_dict dict_deficient_9 dict_perfect_9 dict_abundant_9 1 28123)
(*(make_dict dict_deficient_1 dict_perfect_1 dict_abundant_1 1 1500)
(make_dict dict_deficient_2 dict_perfect_2 dict_abundant_2 1501 3000)
(make_dict dict_deficient_3 dict_perfect_3 dict_abundant_3 3001 4500)
(make_dict dict_deficient_4 dict_perfect_4 dict_abundant_4 4501 6000)
(make_dict dict_deficient_5 dict_perfect_5 dict_abundant_5 6001 7500)
(make_dict dict_deficient_6 dict_perfect_6 dict_abundant_6 7501 9000)
(make_dict dict_deficient_7 dict_perfect_7 dict_abundant_7 9001 10500)
(make_dict dict_deficient_8 dict_perfect_8 dict_abundant_8 10501 12123)*)

let dict_abundant_total1 = dict_abundant_1
let dict_abundant_total2 = dict_abundant_2
let dict_abundant_total3 = dict_abundant_3
let dict_abundant_total4 = dict_abundant_4
let dict_abundant_total5 = dict_abundant_5
let dict_abundant_total6 = dict_abundant_6
let dict_abundant_total7 = dict_abundant_7
let dict_abundant_total8 = dict_abundant_8
let dict_abundant_total9 = dict_abundant_9
let dict_size = dict_abundant_total1.Count
printfn "dict size : %d" dict_size

let mutable res_1 = 0L
let mutable res_2 = 0L
let mutable res_3 = 0L
let mutable res_4 = 0L
let mutable res_5 = 0L
let mutable res_6 = 0L
let mutable res_7 = 0L
let mutable res_8 = 0L
let mutable res_9 = 0L

let lst1 = ref []
let lst2 = ref []
let lst3 = ref []
let lst4 = ref []
let lst5 = ref []
let lst6 = ref []
let lst7 = ref []
let lst8 = ref []
let lst9 = ref []

let thread1 (dict : Dictionary<int, int>) = new Thread (fun () ->
    let sum_1 = ref []
    for i=0 to 800 do
        for j=i to 6964 do
            let sum = (((dict.ElementAt(i)).Key)+((dict.ElementAt(j)).Key))
            if sum <= 28123 then sum_1 := sum::(!sum_1)
    sum_1 := lst_distinct(!sum_1)
    lst1 := !sum_1
    for elem in (!sum_1) do
        res_1 <- res_1 + int64(elem)
        File.AppendAllText("t1.txt", (elem.ToString())+"\n")
    printfn "Thread1 Finished!, sum : %d" res_1)
let thread2 (dict : Dictionary<int, int>) = new Thread (fun () ->
    let sum_2 = ref []
    for i=801 to 1600 do
        for j=i to 6964 do
            let sum = (((dict.ElementAt(i)).Key)+((dict.ElementAt(j)).Key))
            if sum <= 28123 then sum_2 := sum::(!sum_2)
    sum_2 := lst_distinct(!sum_2)
    lst2 := !sum_2
    for elem in (!sum_2) do
        res_2 <- res_2 + int64(elem)
    printfn "Thread2 Finished!, sum : %d" res_2)
let thread3 (dict : Dictionary<int, int>) = new Thread (fun () ->
    let sum_3 = ref []
    for i=1601 to 2400 do
        for j=i to 6964 do
            let sum = (((dict.ElementAt(i)).Key)+((dict.ElementAt(j)).Key))
            if sum <= 28123 then sum_3 := sum::(!sum_3)
    sum_3 := lst_distinct(!sum_3)
    lst3 := !sum_3
    for elem in (!sum_3) do
        res_3 <- res_3 + int64(elem)
    printfn "Thread3 Finished!, sum : %d" res_3)
let thread4 (dict : Dictionary<int, int>) = new Thread (fun () ->
    let sum_4 = ref []
    for i=2401 to 3200 do
        for j=i to 6964 do
            let sum = (((dict.ElementAt(i)).Key)+((dict.ElementAt(j)).Key))
            if sum <= 28123 then sum_4 := sum::(!sum_4)
    sum_4 := lst_distinct(!sum_4)
    lst4 := !sum_4
    for elem in (!sum_4) do
        res_4 <- res_4 + int64(elem)
    printfn "Thread4 Finished!, sum : %d" res_4)
let thread5 (dict : Dictionary<int, int>) = new Thread (fun () ->
    let sum_5 = ref []
    for i=3201 to 4000 do
        for j=i to 6964 do
            let sum = (((dict.ElementAt(i)).Key)+((dict.ElementAt(j)).Key))
            if sum <= 28123 then sum_5 := sum::(!sum_5)
    sum_5 := lst_distinct(!sum_5)
    lst5 := !sum_5
    for elem in (!sum_5) do
        res_5 <- res_5 + int64(elem)
    printfn "Thread5 Finished!, sum : %d" res_5)
let thread6 (dict : Dictionary<int, int>) = new Thread (fun () ->
    let sum_6 = ref []
    for i=4001 to 4800 do
        for j=i to 6964 do
            let sum = (((dict.ElementAt(i)).Key)+((dict.ElementAt(j)).Key))
            if sum <= 28123 then sum_6 := sum::(!sum_6)
    sum_6 := lst_distinct(!sum_6)
    lst6 := !sum_6
    for elem in (!sum_6) do
        res_6 <- res_6 + int64(elem)
    printfn "Thread6 Finished!, sum : %d" res_6)
let thread7 (dict : Dictionary<int, int>) = new Thread (fun () ->
    let sum_7 = ref []
    for i=4801 to 5600 do
        for j=i to 6964 do
            let sum = (((dict.ElementAt(i)).Key)+((dict.ElementAt(j)).Key))
            if sum <= 28123 then sum_7 := sum::(!sum_7)
    sum_7 := lst_distinct(!sum_7)
    lst7 := !sum_7
    for elem in (!sum_7) do
        res_7 <- res_7 + int64(elem)
    printfn "Thread7 Finished!, sum : %d" res_7)
let thread8 (dict : Dictionary<int, int>) = new Thread (fun () ->
    let sum_8 = ref []
    for i=5601 to 6400 do
        for j=i to 6964 do
            let sum = (((dict.ElementAt(i)).Key)+((dict.ElementAt(j)).Key))
            if sum <= 28123 then sum_8 := sum::(!sum_8)
    sum_8 := lst_distinct(!sum_8)
    lst8 := !sum_8
    for elem in (!sum_8) do
        res_8 <- res_8 + int64(elem)
    printfn "Thread8 Finished!, sum : %d" res_8)

let thread9 (dict : Dictionary<int, int>) = new Thread (fun () ->
    let sum_9 = ref []
    for i=6401 to 6964 do
        for j=i to 6964 do
            let sum = (((dict.ElementAt(i)).Key)+((dict.ElementAt(j)).Key))
            if sum <= 28123 then sum_9 := sum::(!sum_9)
    sum_9 := lst_distinct(!sum_9)
    lst9 := !sum_9
    for elem in (!sum_9) do
        res_9 <- res_9 + int64(elem)
    printfn "Thread9 Finished!, sum : %d" res_9)

let thread1_full = (thread1 dict_abundant_1)
let thread2_full = (thread2 dict_abundant_2)
let thread3_full = (thread3 dict_abundant_3)
let thread4_full = (thread4 dict_abundant_4)
let thread5_full = (thread5 dict_abundant_5)
let thread6_full = (thread6 dict_abundant_6)
let thread7_full = (thread7 dict_abundant_7)
let thread8_full = (thread8 dict_abundant_8)
let thread9_full = (thread8 dict_abundant_9)

thread1_full.Start()
thread2_full.Start()
thread3_full.Start()
thread4_full.Start()
thread5_full.Start()
thread6_full.Start()
thread7_full.Start()
thread8_full.Start()
thread9_full.Start()

thread1_full.Join()
thread2_full.Join()
thread3_full.Join()
thread4_full.Join()
thread5_full.Join()
thread6_full.Join()
thread7_full.Join()
thread8_full.Join()
thread9_full.Join()

thread1_full.Abort()
thread2_full.Abort()
thread3_full.Abort()
thread4_full.Abort()
thread5_full.Abort()
thread6_full.Abort()
thread7_full.Abort()
thread8_full.Abort()
thread9_full.Abort()
//let sum_of_two_abundants = ref []
//for i=0 to dict_abundant.Count-1 do
//    for j=i to dict_abundant.Count-1 do
//        sum_of_two_abundants := (((dict_abundant.ElementAt(i)).Value)+((dict_abundant.ElementAt(j)).Value))::(!sum_of_two_abundants)
//sum_of_two_abundants := (lst_distinct(List.filter (fun s -> s <= 28123) (!sum_of_two_abundants)))

//let answer = List.sum (!sum_of_two_abundants)

let total_lst = List.sum(lst_distinct(List.append !lst9 (List.append (List.append (List.append !lst1 !lst2) (List.append !lst3 !lst4)) (List.append (List.append !lst5 !lst6) (List.append !lst7 !lst8)))))

printfn "total_lst_sum : %d" total_lst

let answer = ((28123 * 28124)/2) - total_lst

sw.Stop()
printfn "Answer : %d" answer
printfn "Elapsed Time : %f" sw.Elapsed.TotalMilliseconds