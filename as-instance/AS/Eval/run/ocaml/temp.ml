

open Core.Std;;
;;
print_string(Std.dump(List.fold ~init:0.0 ~f:(+.) [19.59;62.57;46.66;43.34]))