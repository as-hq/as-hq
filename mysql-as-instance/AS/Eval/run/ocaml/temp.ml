

let rec sum lst = match lst with 
	| [] -> 0
	| x::xs -> x + (sum xs)
;;
print_string(Std.dump(sum [1;3]))