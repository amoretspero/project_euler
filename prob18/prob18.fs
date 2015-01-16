open System
open System.Collections
open System.Diagnostics
open System.Numerics
open System.Collections.Generic

exception INVALID_INPUT

let sw = Stopwatch()
sw.Start()

let path_mem = new Dictionary<(int * int), int>()

let triangle = [ [75];
                [95; 64];
                [17; 47; 82];
                [18; 35; 87; 10];
                [20; 04; 82; 47; 65];
                [19; 01; 23; 75; 03; 34];
                [88; 02; 77; 73; 07; 63; 67];
                [99; 65; 04; 28; 06; 16; 70; 92];
                [41; 41; 26; 56; 83; 40; 80; 70; 33];
                [41; 48; 72; 33; 47; 32; 37; 16; 94; 29];
                [53; 71; 44; 65; 25; 43; 91; 52; 97; 51; 14];
                [70; 11; 33; 28; 77; 73; 17; 78; 39; 68; 17; 57];
                [91; 71; 52; 38; 17; 14; 91; 43; 58; 50; 27; 29; 48];
                [63; 66; 04; 68; 89; 53; 67; 30; 73; 16; 69; 87; 40; 31];
                [04; 62; 98; 27; 23; 09; 70; 98; 73; 93; 38; 53; 60; 04; 23] ]

let rec find_max (lst : int list list) =
    match lst.Length with
    0 -> raise INVALID_INPUT
    | 1 -> 
        let mutable max_val = lst.[0].[0]
        if path_mem.ContainsKey((1, 0))&&path_mem.ContainsKey((1, 1)) then max_val <- Math.Max(((lst.[0].[0])+(path_mem.Item((1, 0)))), ((lst.[0].[0])+(path_mem.Item((1, 1)))))
        path_mem.Add((0, 0), max_val)
    | _ ->
        let row = (lst.Length)-1
        for column=0 to row do
            let mutable max_val = lst.[row].[column]
            if path_mem.ContainsKey((row+1, column))&&path_mem.ContainsKey((row+1, column+1)) then max_val <- Math.Max(((lst.[row].[column])+(path_mem.Item((row+1, column)))), ((lst.[row].[column])+(path_mem.Item((row+1, column+1)))))
            path_mem.Add((row, column), max_val)
        (find_max (List.rev(List.tail(List.rev(lst)))))
(find_max triangle)
let answer = path_mem.Item((0, 0))

sw.Stop()
printfn "Answer : %d" answer
printfn "Elapsed Time : %f" sw.Elapsed.TotalMilliseconds