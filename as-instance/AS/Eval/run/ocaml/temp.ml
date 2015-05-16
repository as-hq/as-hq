

let rec fib = function  | 0 -> 0  | 1 -> 1  | n -> fib (n-1) + fib (n-2);;
print_string(Std.dump(fib 5))