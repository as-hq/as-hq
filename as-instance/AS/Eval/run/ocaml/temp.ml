

let rec fibonacci n = match n with    | 0 -> 1    | 1 -> 1    | n -> fibonacci (n-1) + fibonacci (n-2);;;;
print_string(Std.dump(fibonacci(12)))