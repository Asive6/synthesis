module Synthesis

let abelar inp =
   (inp>12) && (inp<3097) && (inp%12=0)
    

let area b h =
    match b<0.0 || h<0.0 with
    | true -> failwith "type and range exception"
    |_ -> 0.5*b*h
    

let zollo num =
    match num<0 with
    | true-> num * -1
    | false-> num * 2
    

let min a b =
    match a<b with
    | true -> a
    |_ -> b


let max a b =
    match a>b with 
    |true -> a
    |_ -> b
    

let ofTime h min sec = h * 3600 + min *60 + sec   
    
   

let toTime sec =
    match sec>=0 with
    |true ->(sec/3600,(sec%3600)/60,sec%60)
    |_ -> (0,0,0)
        

let digits d =
    let rec numDigit i acc=
        match i=0 with
        |true -> acc
        |_->numDigit(i/10) (1+acc)
    match d<>0 with
    |false ->1
    |_->numDigit d 0
    

let minmax (a, b, c, d) = min a b |> min c, min d, max a b |> max c |> max d
    

let isLeap slep =
    match slep<1582 with
    |true -> failwith "the input slep should be eqaul or greater then 1582"
    |false -> 
            match ((slep%4=0) && (slep%100=0) && (slep % 400=0)) || slep % 4=0 && slep%100<>0 with
            |true -> true
            |false ->false
    

let month a=
    match a<=0 || a>12 with 
    |true -> failwith "void month"
    |false->
        
    failwith "Not implemented"

let toBinary _ =
    failwith "Not implemented"

let bizFuzz _ =
    failwith "Not implemented"

let monthDay _ _ =
    failwith "Not implemented"

let coord _ =
    failwith "Not implemented"