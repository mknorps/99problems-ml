exception IllegalArgument

fun isPositive x =
  if x>0 then x
  else raise IllegalArgument

	     

(* P31: Determine whether a given integer number is prime. *)
fun isPrime x =
  let fun divisor y =
	if  ((isPositive x) mod y = 0)
	then false
	else if  (y*y >= x) then true
	else divisor (y+1)
  in
      if x=2 then true
      else divisor 2
  end
      
(* P32: Determine the greatest common divisor of two positive integer numbers.*)
      (* Euclidian algorythm *)
fun gcd_eucl x y =
  let fun eucl (x,y,r) =
	case r of
	    0 => y
	  | z => let val r1 = x mod y
		 in eucl (y,r1,y mod r1) 
  	         end
  in
      if x>y then eucl (x,y,x mod y )
      else eucl (y,x, y mod x)
  end

      
(* P33: Determine whether two positive integer numbers are coprime.*)
fun ifComprime x y = if (gcd_eucl x y) = 1 then true else false
							      

(* P34: Calculate Euler's totient function phi(m).*)
							      
fun totient x =
  let fun totient_acc (y,acc)=
         case y of
	     0 => acc
	   | y1 =>  if (ifComprime x y1)
		    then totient_acc(y1-1,acc+1)
		    else totient_acc(y1-1,acc)
  in
     totient_acc(x,0)
  end

(* P35: Determine the prime factors of a given positive integer. *)
(* TODO - optimization *)     
fun pFactors x =
  let fun primeDivisors (y,acc) =
	if (isPrime y andalso ((isPositive x) mod y = 0))
	then primeDivisors(y+1,y::acc)
	else if (y*y >= x) then acc
	else primeDivisors (y+1,acc)
	    
  in
      primeDivisors (2,[])
  end


(* P36: Determine the prime factors of a given positive integer and their multiplicity. *)
(* TODO count occurences and create tuples *)
fun pFactorsMult x =
  let fun primeDivisors (z,y,acc) =
        if isPrime z then z::acc
	else if (isPrime y andalso ((isPositive z) mod y = 0))
	then primeDivisors(z div y,y,y::acc)
	else primeDivisors (z,y+1,acc)
  in
      primeDivisors (x,2,[])
  end
