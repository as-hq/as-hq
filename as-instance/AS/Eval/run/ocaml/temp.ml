

let rec sum lst = match lst with 
	| [] -> 0.0
	| x::xs -> x +. (sum xs)
;;
print_string(Std.dump(sum [4.0;0.0;1.0;2.0]))