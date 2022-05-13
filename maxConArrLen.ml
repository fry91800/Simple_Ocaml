open Printf
open Char;;
let liste = [9; 11; 12; 13; 14; 0];;
let conArrLen l =
  let rec recConArrLen l depth formerHead =
    match l with
    |[] -> 0
    |head::tail -> if depth = 0 then 1 + recConArrLen tail 1 head
    else if head = formerHead + 1 then 1 + recConArrLen tail 1 head
    else 0;
	
  in match l with
    | [] -> 0
    | head::tail -> recConArrLen l 0 0;;

let maxConArrLen l =
  let rec recMaxConArrLen l index maxLen indexOfMax =
    match l with
    |[] -> (indexOfMax, maxLen)
	|head::tail -> if conArrLen l > maxLen then recMaxConArrLen tail (index+1) (conArrLen l) index
    else recMaxConArrLen tail (index+1) maxLen indexOfMax;
    
  in match l with
    | [] -> (0,0)
    | head::tail -> recMaxConArrLen l 0 0 0;;

if maxConArrLen [1; 2] <> (0,2) then
  print_endline "Faux";
if maxConArrLen [1; 2] <> (0,2) then
  print_endline "Faux";
if (maxConArrLen [1; 0] <> (0,1) && maxConArrLen [1; 0] <> (1,1)) then
  print_endline "Faux";
if maxConArrLen [11; 12; 13; 14; 0] <> (0,4) then
  print_endline "Faux";
if maxConArrLen [1; 11; 12; 13; 14; 0] <> (1,4) then
  print_endline "Faux";;