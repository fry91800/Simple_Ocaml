let rec concatenate l1 l2 = match l1 with
    | [] -> l2
    | e::l -> e::(concatenate l l2);;

let explode s =
    let rec exp i l =
        if i < 0 then l else exp (i - 1) (s.[i] :: l) in
    exp (String.length s - 1) []

let repeater n l1 lorig =
    let rec repeaterImpl_ c list_c = match c with
        | 0 -> list_c
        | c -> repeaterImpl_ (c-1) (concatenate l1 list_c);	
    in match n with
		|_ when n < 0 -> lorig
		|_ -> repeaterImpl_ n lorig;;


let readInt l =
    let rec readIntImpl_ current_list n = match current_list with
        | '0'::tail -> readIntImpl_ tail (n*10)
        | '1'::tail -> readIntImpl_ tail (n*10 + 1)
        | '2'::tail -> readIntImpl_ tail (n*10 + 2)
        | '3'::tail -> readIntImpl_ tail (n*10 + 3)
        | '4'::tail -> readIntImpl_ tail (n*10 + 4)
        | '5'::tail -> readIntImpl_ tail (n*10 + 5)
        | '6'::tail -> readIntImpl_ tail (n*10 + 6)
        | '7'::tail -> readIntImpl_ tail (n*10 + 7)
        | '8'::tail -> readIntImpl_ tail (n*10 + 8)
        | '9'::tail -> readIntImpl_ tail (n*10 + 9)
        | head::tail -> (n,current_list)
        | [] -> (n,[]) ;(*error*)
    in match l with
        | [] -> (-1,[])
        | head::tail -> readIntImpl_ l 0;;

let isNumeric c =
	let (x,y) = readInt(explode(c))
		in match explode(c) with
			| [] -> false
			| h::t -> if y = [] then true else false;;

let getSubString l =
    let rec getSubStringImpl_ l = match l with
        | [] -> [],[]
        | h::t when isNumeric(String.make 1 h) = true -> [], h::t
        | h::t when isNumeric(String.make 1 h) = false -> h::fst(getSubStringImpl_ t), snd(getSubStringImpl_ t);
    in match l with
        | [] -> [],[]
        | h::t -> getSubStringImpl_ l;;

let decompress l =
    let rec decompressImpl_ current_list = match current_list with
        | [] -> []
        | h::t when isNumeric(String.make 1 h) = true ->
		repeater
		(fst(readInt(current_list)))
		(fst(getSubString(snd(readInt(current_list)))))
		(decompressImpl_ ((snd(getSubString(snd(readInt(current_list)))))))
        | h::t when isNumeric(String.make 1 h) = false ->
		repeater
		1
		(fst(getSubString(snd(readInt(t)))))
		(decompressImpl_ (snd(getSubString(snd(readInt(t))))));
    in match l with
        | [] -> []
        | h::t -> decompressImpl_ l;;


(*test*)
if (decompress (explode "1abc")) <> (explode "abc") then
    print_endline "Failed 1abc";;
if (decompress (explode "3abc")) <> (explode "abcabcabc") then
    print_endline "Failed 3abc";;
if (decompress (explode "3abc")) <> (repeater 3 (explode "abc") []) then
    print_endline "Failed 3abc";;
if (decompress (explode "3abc4edf")) <> (repeater 3 (explode "abc") (repeater 4 (explode "edf") [])) then
    print_endline "Failed 3abc4edf";;
if (decompress (explode "31abc94edf")) <> (repeater 31 (explode "abc") (repeater 94 (explode "edf") [])) then
    print_endline "Failed 31abc94edf";;

