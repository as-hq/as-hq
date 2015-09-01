

let rec sum lst = match lst with 
	| [] -> 0.0
	| x::xs -> x +. (sum xs)
;;
print_string(Std.dump(sum [0.0;1.0;2.0;3.0;4.0;5.0;6.0;7.0;8.0;9.0;10.0;11.0;12.0;13.0;14.0;15.0;16.0;17.0;18.0;19.0]))