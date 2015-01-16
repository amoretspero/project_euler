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
    | _ when (start_year%4=0 && start_year%100<>0) -> 366 + (get_total_days (start_year+1) end_year)
    | _ -> raise INVALID_INPUT

let get_day_after (day : string) (num : int) =


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
        for i=1 to 31 do
            res := List.append [ (y, m, i, start_