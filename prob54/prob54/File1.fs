module File1

open System
open System.Collections
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Linq
open System.Numerics

exception InvalidInput

type suit = 
    CLOVER
    | DIAMOND
    | HEART
    | SPADE

type cardNumber = NUMBER of int | ALPHABET of string

type card (num : cardNumber, s : suit) =
    let input_number = num
    let input_suit = s
    let input_suit_abbr = if s = CLOVER then "C" else if s = DIAMOND then "D" else if s = HEART then "H" else "S"

    member c.number with get() = input_number
    member c.suit with get() = input_suit
    member c.suitAbbr with get() = input_suit_abbr

let card_gen (str : string) =
    let temp = List.map (fun x -> x.ToString()) (List.ofSeq(str.ToCharArray()))
    match temp.[1] with
    | "C" -> 
        let mutable cn = NUMBER -1
        try 
            cn <- NUMBER (System.Convert.ToInt32(temp.[0]))
        with
            | :? InvalidCastException -> cn <- ALPHABET temp.[0]
        card(cn, CLOVER)
    | "D" -> 
        let mutable cn = NUMBER -1
        try
            cn <- NUMBER (System.Convert.ToInt32(temp.[0]))
        with
            | :? InvalidCastException -> cn <- ALPHABET temp.[0]
        card(cn, DIAMOND)
    | "H" -> 
        let mutable cn = NUMBER -1
        try
            cn <- NUMBER (System.Convert.ToInt32(temp.[0]))
        with
            | :? InvalidCastException -> cn <- ALPHABET temp.[0]
        card(cn, HEART)
    | "S" -> 
        let mutable cn = NUMBER -1
        try
            cn <- NUMBER (System.Convert.ToInt32(temp.[0]))
        with
            | :? InvalidCastException -> cn <- ALPHABET temp.[0]
        card(cn, SPADE)
    | _ -> raise InvalidInput

/// Compares two cards. if (c1's number) >= (c2's number) then return 1, else return -1.
let cardCompare (c1 : card) (c2 : card) =
    match c1.number with
    | NUMBER n1 ->
        match c2.number with
        | NUMBER n2 ->
            if n1 >= n2 then 1 else -1
        | ALPHABET a2 -> -1
    | ALPHABET a1 ->
        match c2.number with
        | NUMBER n2 -> 1
        | ALPHABET a2 ->
            if a1 = "A" then 1
            else if a1 = "K" then
                if a2 = "A" then -1 else 1
            else if a1 = "Q" then
                if a2 = "A" || a2 = "K" then -1 else 1
            else 
                if a2 = "A" || a2 = "K" || a2 = "Q" then -1 else 1

let card_list = List.ofArray(File.ReadAllLines("poker.txt")) |> List.map (fun x -> List.ofArray(x.Split([| ' ' |])))
let player1_card_list = List.map (fun (x : string list) -> List.map (fun y -> card_gen(y)) x) (List.map (fun (x : string list) -> List.ofSeq(x.Take(5))) card_list)
let player2_card_list = List.map (fun (x : string list) -> List.map (fun y -> card_gen(y)) x) (List.map (fun (x : string list) -> List.ofSeq(x.Skip(5))) card_list)

/// Returns list of cards that have distinct numbers. If the original list contains more than one card of same number, first card's suit is chosen.
let distinctNumberedCards (h1 : card list) =
    let res = ref ([] : card list)
    let num_temp = ref ([] : cardNumber list)
    for elem in h1 do
        if (not (num_temp.Value.Contains(elem.number))) then
            res.Value <- List.append res.Value [elem]
            num_temp.Value <- List.append num_temp.Value [elem.number]
    res.Value

/// Finds highest value card and return its number.
let findHighCard (h1 : card list) =
    let sorted = List.sortWith cardCompare h1
    sorted.Last().number

/// Finds one pair(two cards of the same value) and return that pair's number. If there are none, return cardNumber -1.
let findOnePair (h1 : card list) =
    let sorted = List.sortWith cardCompare h1
    let mutable prevNumber = sorted.[0].number
    let mutable currNumber = sorted.[1].number
    let mutable cnt = 2
    let mutable loop_break = false
    let mutable res = NUMBER -1
    while cnt < 5 && (not loop_break) do
        if prevNumber = currNumber then
            loop_break <- true
            res <- currNumber
        else
            prevNumber <- currNumber
            currNumber <- sorted.[cnt].number
            cnt <- cnt + 1
    if res <> NUMBER -1 then
        let temp = distinctNumberedCards (List.filter (fun (x : card) -> x.number <> res) sorted)
        if temp.Count() <> 3 then
            res <- NUMBER -1
    res

/// Finds two pair(two different pairs) and return their pairs' numbers as sorted list. If there are none, return two-elem list of cardNumber -1.
let findTwoPairs (h1 : card list) =
    let sorted = List.sortWith cardCompare h1
    let dist = distinctNumberedCards sorted
    let res = ref ([] : cardNumber list)
    if dist.Count() = 3 then
        let num0 = dist.[0].number
        let lst0 = List.filter (fun (x : card) -> x.number = num0) sorted
        let len0 = lst0.Length
        let num1 = dist.[1].number
        let lst1 = List.filter (fun (x : card) -> x.number = num1) sorted
        let len1 = lst1.Length
        let num2 = dist.[2].number
        let lst2 = List.filter (fun (x : card) -> x.number = num2) sorted
        let len2 = lst2.Length
        if len0 * len1 * len2 = 4 then
            if len2 = 2 then res.Value <- num2 :: res.Value
            if len1 = 2 then res.Value <- num1 :: res.Value
            if len0 = 2 then res.Value <- num0 :: res.Value
    if res.Value.Length = 0 then
        res.Value <- [NUMBER -1; NUMBER -1]
    res.Value

/// Finds three of a kind(three cards of the same value) and return that triplet's number. If there are none, return NUMBER -1.
let findThreeOfAKind (h1 : card list) =
    let sorted = List.sortWith cardCompare h1
    let mutable num0 = sorted.[0].number
    let mutable num1 = sorted.[1].number
    let mutable num2 = sorted.[2].number
    let mutable cnt = 3
    let mutable loop_break = false
    let mutable res = NUMBER -1
    while cnt < 5 && (not loop_break) do
        if num0 = num1 && num1 = num2 then
            let temp = distinctNumberedCards (List.filter (fun (x : card) -> x.number <> num0) sorted)
            if temp.Count() = 2 then
                res <- num0
                loop_break <- true
        else
            num0 <- num1
            num1 <- num2
            num2 <- sorted.[cnt].number
            cnt <- cnt + 1
    res