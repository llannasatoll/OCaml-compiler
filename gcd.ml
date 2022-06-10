(* find greatest common divisor of 12 + 6 and 3 * 8 *)

let rec gcd2 m n =
  if n = 0 then m
  else let amari = m mod n
       in gcd2 n amari
in

let rec gcd m n =
  if m <= n then gcd2 n m
	    else gcd2 m n
in

gcd (12 + 6) (3 * 8)
