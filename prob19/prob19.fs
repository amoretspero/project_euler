open System
open System.Collections
open System.Diagnostics
open System.Numerics
open System.Collections.Generic

let sw = Stopwatch()
sw.Start()

exception INVALID_INPUT

let get_days (y : int) (m : int) =
    match m with
    | 1 
    | 3
    | 5 
    | 7
    | 8
    | 10
    | 12 -> 31
    | 4
    | 6
    | 9
    | 11 -> 30
    | 2 when (y%4=0 && y%100<>0)||(y%400=0) -> 29
    | 2 when (y%100=0)&&(y%400<>0) -> 28
    | 2 when (y%4<>0) -> 28
    | _ -> raise INVALID_INPUT

let rec get_total_days (start_year : int) (end_year : int) =
    match start_year with
    | _ when start_year < 1900 -> raise INVALID_INPUT
    | _ when (start_year > end_year) -> 0
    | _ when (start_year%100=0 && start_year%400<>0) || (start_year%4<>0) -> 365 + (get_total_days (start_year+1) end_year)
    | _ when (start_year%4=0 && start_year%100<>0) || (start_year%400=0) -> 366 + (get_total_days (start_year+1) end_year)
    | _ -> raise INVALID_INPUT

let get_day_after (day : string) (num : int) =
    let mutable day_num = 0
    if day="MON" then day_num <- 0
    elif day="TUE" then day_num <- 1
    elif day="WED" then day_num <- 2
    elif day="THU" then day_num <- 3
    elif day="FRI" then day_num <- 4
    elif day="SAT" then day_num <- 5
    elif day="SUN" then day_num <- 6
    else raise INVALID_INPUT
    match (num+day_num)%7 with
    0 -> "MON"
    | 1 -> "TUE"
    | 2 -> "WED"
    | 3 -> "THU"
    | 4 -> "FRI"
    | 5 -> "SAT"
    | 6 -> "SUN"
    | _ -> raise INVALID_INPUT

let calendar_gen (y : int) (m : int) =
    let res = ref []
    let prev_days = (get_total_days 1900 (y-1))
    let mutable start_day = ""
    if prev_days%7=0 then start_day <- "MON"
    else if prev_days%7=1 then start_day <- "TUE"
    else if prev_days%7=2 then start_day <- "WED"
    else if prev_days%7=3 then start_day <- "THU"
    else if prev_days%7=4 then start_day <- "FRI"
    else if prev_days%7=5 then start_day <- "SAT"
    else if prev_days%7=6 then start_day <- "SUN"
    match m with
    | 1 -> 
        for d=1 to 31 do
            res := List.append [ (y, m, d, (get_day_after start_day (d-1))) ] (!res)
    | 2 ->
        let mutable prev_months = 0
        for i=1 to m-1 do
            prev_months <- prev_months + (get_days y i)
        for d=1 to (get_days y m) do
            res := List.append [ (y, m, d, (get_day_after start_day (d-1+prev_months))) ] (!res)
    | 3 ->
        let mutable prev_months = 0
        for i=1 to m-1 do
            prev_months <- prev_months + (get_days y i)
        for d=1 to (get_days y m) do
            res := List.append [ (y, m, d, (get_day_after start_day (d-1+prev_months))) ] (!res)
    | 4 ->
        let mutable prev_months = 0
        for i=1 to m-1 do
            prev_months <- prev_months + (get_days y i)
        for d=1 to (get_days y m) do
            res := List.append [ (y, m, d, (get_day_after start_day (d-1+prev_months))) ] (!res)
    | 5 ->
        let mutable prev_months = 0
        for i=1 to m-1 do
            prev_months <- prev_months + (get_days y i)
        for d=1 to (get_days y m) do
            res := List.append [ (y, m, d, (get_day_after start_day (d-1+prev_months))) ] (!res)
    | 6 ->
        let mutable prev_months = 0
        for i=1 to m-1 do
            prev_months <- prev_months + (get_days y i)
        for d=1 to (get_days y m) do
            res := List.append [ (y, m, d, (get_day_after start_day (d-1+prev_months))) ] (!res)
    | 7 ->
        let mutable prev_months = 0
        for i=1 to m-1 do
            prev_months <- prev_months + (get_days y i)
        for d=1 to (get_days y m) do
            res := List.append [ (y, m, d, (get_day_after start_day (d-1+prev_months))) ] (!res)
    | 8 ->
        let mutable prev_months = 0
        for i=1 to m-1 do
            prev_months <- prev_months + (get_days y i)
        for d=1 to (get_days y m) do
            res := List.append [ (y, m, d, (get_day_after start_day (d-1+prev_months))) ] (!res)
    | 9 ->
        let mutable prev_months = 0
        for i=1 to m-1 do
            prev_months <- prev_months + (get_days y i)
        for d=1 to (get_days y m) do
            res := List.append [ (y, m, d, (get_day_after start_day (d-1+prev_months))) ] (!res)
    | 10 ->
        let mutable prev_months = 0
        for i=1 to m-1 do
            prev_months <- prev_months + (get_days y i)
        for d=1 to (get_days y m) do
            res := List.append [ (y, m, d, (get_day_after start_day (d-1+prev_months))) ] (!res)
    | 11 ->
        let mutable prev_months = 0
        for i=1 to m-1 do
            prev_months <- prev_months + (get_days y i)
        for d=1 to (get_days y m) do
            res := List.append [ (y, m, d, (get_day_after start_day (d-1+prev_months))) ] (!res)
    | 12 ->
        let mutable prev_months = 0
        for i=1 to m-1 do
            prev_months <- prev_months + (get_days y i)
        for d=1 to (get_days y m) do
            res := List.append [ (y, m, d, (get_day_after start_day (d-1+prev_months))) ] (!res)
    | _ -> raise INVALID_INPUT
    (!res)

let rec first_sunday_counter (start_year : int) (start_month : int) (end_year : int) (end_month : int) =
    match start_year with
    | _ when start_year < end_year ->
        match start_month with
        | 12 -> 
            if ((List.exists (fun s -> s=(start_year, start_month, 1, "SUN")) (calendar_gen start_year start_month))) then 1+(first_sunday_counter (start_year+1) 1 end_year end_month)
            else (first_sunday_counter (start_year+1) 1 end_year end_month)
        | _ when (start_month>=1 || start_month<=11) -> 
            if ((List.exists (fun s -> s=(start_year, start_month, 1, "SUN")) (calendar_gen start_year start_month))) then 1+(first_sunday_counter start_year (start_month+1) end_year end_month)
            else (first_sunday_counter start_year (start_month+1) end_year end_month)
        | _ -> raise INVALID_INPUT
    | _ when start_year = end_year ->
        match start_month with
        | _ when start_month=end_month ->
            if ((List.exists (fun s -> s=(start_year, start_month, 1, "SUN")) (calendar_gen start_year start_month))) then 1
            else 0
        | _ when start_month < end_month ->
            if ((List.exists (fun s -> s=(start_year, start_month, 1, "SUN")) (calendar_gen start_year start_month))) then 1+(first_sunday_counter start_year (start_month+1) end_year end_month)
            else (first_sunday_counter start_year (start_month+1) end_year end_month)
        | _ -> raise INVALID_INPUT
    | _ -> raise INVALID_INPUT
    
let answer = first_sunday_counter 1901 1 2000 12

sw.Stop()
printfn "Answer : %d" answer
printfn "Elapsed Time : %f" sw.Elapsed.TotalMilliseconds