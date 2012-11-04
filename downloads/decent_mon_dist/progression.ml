open Types
open Ltl
open Utils
exception Error



let progress (form:ltl_formula) (e:event)  =
	 let f = simp form in
	 let rec progress_rec f =
	 	 match simp f with 
			  False -> False
			| True -> True
			| Var p -> if (List.mem p e) then True else False
			| Or (f1,f2) -> Or(progress_rec f1, progress_rec f2)
			| And(f1,f2) -> And(progress_rec f1, progress_rec f2)
	  		| Neg(f1) -> Neg (progress_rec f1)
			| Next(f1) -> f1
			| Until (f1,f2) -> Or(progress_rec f2, And(progress_rec f1, Until (f1,f2)))
			| Glob (f1) -> And(progress_rec f1, Glob f1)
			| Ev(f1) -> Or (progress_rec f1, Ev f1)
			| SHARP -> SHARP
			| Xor (f1,f2) -> Xor (progress_rec f1, progress_rec f2)
			| _ -> False 	
	in progress_rec f




let rec monitor_old (f:ltl_formula) (t:trace) (t_read:trace)= 
	match (simp f) with
		True -> (True,t_read)
		| False -> (False, t_read)
		| _ -> ( match t with  
			 [] -> raise Error
			| (e:event)::[] -> ( match (progress f e) with
							|True -> (True,t_read@[e])
							|_ -> (False,t_read@[e]) )
			| (e:event)::(remainder:trace) -> ( match (progress f e) with
							| True -> (True, t_read@[e])
							| False -> (False, t_read@[e])
							| obligation ->  monitor_old obligation remainder (t_read@[e]))
		)
		;;


let monitor (f:ltl_formula) (t:trace) = 
	let the_length = List.length t in
	let rec monitor_rec (f:ltl_formula) (current:int) =	
		match (simp f) with
			True -> (True,if current=0 then [] else (if current > (the_length-1) then t else prefix current t))
			| False -> (False, if current=0 then [] else (if current > (the_length - 1) then t else prefix current t))
			| _ -> (
				if (current = the_length-1) 
					then match (progress f (List.nth t current)) with
						True -> (True,t)
						| _ -> (False,t)
				else
					(
					if (current< the_length-1) then match (progress f (List.nth t current)) with
													 	| True -> (True, (prefix (current+1)t))
														| False -> (False, (prefix (current+1) t))
														| obligation ->  monitor_rec obligation (current+1)
						else raise Error
						)
				)
		in monitor_rec f  0