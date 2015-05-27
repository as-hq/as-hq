

let main () = 
	try
		print_string("a");
		print_string(Std.dump(1+1))
	with
	| _ -> failwith "Unknown"
;;
if !Sys.interactive then () else main ();;