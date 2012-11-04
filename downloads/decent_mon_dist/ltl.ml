(* Copyright (c) 2008 Andreas Bauer <baueran@gmail.com> 
   Code is released under the GPL license version 3. *)

(*i*)
open Types
open Printf
(*i*)

(*s This is the main data structure representing an LTL formula.
  Notice that Var is of type string now. *)

type ltl_formula =
    True
  | False
  | Var of string
  | Or of ltl_formula * ltl_formula
  | And of ltl_formula * ltl_formula
  | Neg of ltl_formula
  | Iff of ltl_formula * ltl_formula
  | Imp of ltl_formula * ltl_formula
  | Until of ltl_formula * ltl_formula
  | Wuntil of ltl_formula * ltl_formula 
  | Next of ltl_formula
  | Glob of ltl_formula
  | Ev of ltl_formula
  | Previous of ltl_formula
  | Xor of ltl_formula * ltl_formula
  | SHARP
  
let rec size (f:ltl_formula) =
	match f with
	    True -> 1
	  | False -> 1
	  | Var x -> 1
	  | Or (f1,f2) -> 1+ size f1 + size f2
	  | And (f1,f2) -> 1 + size f1 +size f2
	  | Neg f -> 1 + size f
	  | Iff (f1,f2) -> 1+ size f1 + size f2
	  | Imp (f1,f2) -> 1+ size f1 + size f2
	  | Until (f1,f2) -> 1+ size f1 + size f2
	  | Wuntil (f1,f2) -> 1+ size f1 + size f2
	  | Next f -> 1 + size f
	  | Glob f -> 1 + size f
	  | Ev f -> 1 + size f
	  | Previous f -> 1 + size f
	  | Xor (f1,f2) -> 1 + size f1 + size f2
	  | SHARP -> 0
  
let rec size2 (f:ltl_formula) =
	match f with
	    True -> 1
	  | False -> 1
	  | Var x -> 1
	  | Or (f1,f2) -> 1+ size2 f1 + size2 f2
	  | And (f1,f2) -> 1 + size2 f1 +size2 f2
	  | Neg f -> 1 + size2 f
	  | Iff (f1,f2) -> 1+ size2 f1 + size2 f2
	  | Imp (f1,f2) -> 1+ size2 f1 + size2 f2
	  | Until (f1,f2) -> 1+ size2 f1 + size2 f2
	  | Wuntil (f1,f2) -> 1+ size2 f1 + size2 f2
	  | Next f -> 1 + size2 f
	  | Glob f -> 1 + size2 f
	  | Ev f -> 1 + size2 f
	  | Previous f -> 1
 	  | Xor (f1,f2) -> 1 + size f1 + size f2
	  | SHARP -> 0  
  
      
(*s This function prints a formula [f] on the standard output. *)
      
(*i This note was relevant when ltl_formula was of generic type:
  
  \textbf{Caveat}: However, right now it does not reflect the generic
  type defined in the data structure above.  What I ultimately want to
  do is restrict the data structure such that ['a] can only be of a
  type which provides its own [show] function, similar in style to
  type classes in Haskell using the [deriving] keyword.  i*)

	
let rec string_rep f =
  match f with
      Var x ->  "Var "^x
    | True ->  "True"
    | False ->  "False"
    | SHARP -> "#"
    | Glob x -> "Glob ("^string_rep x^")"
    | Ev x ->  "Ev ("^ string_rep x^")"
    | Neg x ->  "Neg ("^ string_rep x^")"
    | Next x ->  "Next ("^ string_rep x^  ")"
    | And (x, y) ->  "And ("^ string_rep x^  ", "^ string_rep y^  ")"
    | Or (x, y) ->  "Or ("^ string_rep x^  ", "^string_rep y^  ")"
    | Until (x, y) ->  "Until ("^ string_rep x^  ", "^string_rep y^  ")"
    | Wuntil (x, y) ->  "W-Until ("^ string_rep x^  ", "^string_rep y^  ")"
    | Iff (x, y) ->  "Iff ("^ string_rep x^  ", "^string_rep y^  ")"
    | Imp (x, y) ->  "Imp ("^ string_rep x^  ", "^string_rep y^  ")"
	| Previous x -> "Previous ("^string_rep x^")"
	| Xor (x,y) -> "Xor ("^string_rep x^","^string_rep y^")"

	
let rec number_of_previous (form:ltl_formula) =
	match form with
		Previous f -> 1+number_of_previous f
		| _ -> 0
	
(* Retrieve a formula nested with previous operator, i.e. retrieves Phi in X^m Phi 
It assumes the input formula to be in the form X^m Phi*)	
let rec retrieve_nested_formula_in_a_previous (form:ltl_formula) =
	match form with
		Previous f -> retrieve_nested_formula_in_a_previous f
		| f -> f
	
let rec number_of_next (form:ltl_formula) =
	match form with
		Next f -> 1+number_of_next f
		| _ -> 0
	
(* Retrieve a formula nested with previous operator, i.e. retrieves Phi in X^m Phi 
It assumes the input formula to be in the form X^m Phi*)	
let rec retrieve_nested_formula_in_a_next (form:ltl_formula) =
	match form with
		Next f -> retrieve_nested_formula_in_a_next f
		| f -> f
		
	
let rec string_rep_input f =
  match f with
      Var x ->  x
    | True ->  "True"
    | False ->  "False"
    | SHARP ->  "#"
    | Glob x -> "G ("^string_rep_input x^")"
    | Ev x ->  "F ("^ string_rep_input x^")"
    | Neg x ->  "- ("^ string_rep_input x^")"
    | And (x, y) -> "("^ string_rep_input x^  " & "^ string_rep_input y^")"
    | Or (x, y) -> "("^ string_rep_input x^  " | "^string_rep_input y^")"
    | Until (x, y) ->  "("^string_rep_input x^  " U "^string_rep_input y^")"
    | Wuntil (x, y) ->  "("^string_rep_input x^  " W "^string_rep_input y^")"
    | Iff (x, y) ->  "("^string_rep_input x^  " <-> "^string_rep_input y^")"
    | Imp (x, y) -> "("^string_rep_input x^  " -> "^string_rep_input y^")"
    | Next x -> "X ("^string_rep_input x^")"
    | Previous x -> "Y ("^string_rep_input x^")"
    | Xor (x,y) ->"("^ string_rep_input x^" % "^string_rep_input y^")"


let string_rep_input_formulae (formulae: ltl_formula array) =
	"[\n "^(Array.fold_left (fun x y -> if x="" then y else x^" ,\n"^y ) "" (Array.map string_rep_input formulae))^"\n]"
	
let show_formula f = print_endline (string_rep f)	

(*s This function takes a formula [f] and simplifies it according to
  the laws of Boolean algebra and LTL. *)


(* Here is an implementation of the simplification function implemented in Eagle *)
let rec simplify_eagle f =
	match f with
		| Xor (a,b) -> (match (simplify_eagle a), (simplify_eagle b) with
			| True, True -> False
			| True,f -> Neg f
			| e,f when (e=f) -> False 
			| f, True -> Neg f
			| False, f -> f
			| f, False -> f
			| e,f -> Xor (e,f)
			
		)
		| And(a,b) -> (match (simplify_eagle a), (simplify_eagle b) with
				| True, True -> True
				| True, f -> simplify_eagle f
				| f, True -> simplify_eagle f
				| False, False -> False
				| False, f -> False
				| f, False -> False
				| f,g when (f=g) -> simplify_eagle a
				| f, Xor(g,h) ->simplify_eagle (Xor(And(f,g),And(f,h)))
				| SHARP, f -> f
    			| f , SHARP -> f
    						
    			| e, Neg f when e = f -> False
    			| Neg e, f when e = f -> False
    			| e,f when ((e = Neg f) || (Neg e = f)) -> False
    			| e, f when (e = f) -> e
    
    			| e, Glob f when (e = f) -> Glob f
    						| Glob f, e when (e = f) -> Glob f
    						
    						| Neg e, Glob (And(f,g)) when ((e = f) || (e=g)) -> False
    						| Glob (And(f,g)),Neg e when ((e = f) || (e=g)) -> False
    						
        					| e, Ev f when (e = f) -> e
    						| Ev e, f when (e = f) -> e
    			
    						| Neg e, Glob f when (e = f) -> False
    						| Glob f, Neg e when (e = f) -> False
    						
    						| e, Glob f when ((Neg e = f) || (e = Neg f)) -> False
    						| Glob f, e when ((Neg e = f) || (e = Neg f)) -> False
    						
    			    						
    						| And(e,f), And(g,h) when ((e=g & f=h) || ((e=h) & f=g)) -> simplify_eagle (And(e ,g))
							
							| And(e,f), Or(g,h) when ( e =  g) -> simplify_eagle ( And (e, f) )    						
							| And(e,f), Or(g,h) when ( e =  h) ->  simplify_eagle ( And (e, f) )
							| Or(g,h), And(e,f) when ( e =  g) ->  simplify_eagle ( And (e, f) )   						
							| Or(g,h), And(e,f) when ( e =  h) ->  simplify_eagle ( And (e, f) )    						
							    						
    						| Neg e , And (f,g) when (( e =  f) || ( e= g)) -> False
    						| And (f,g), Neg e  when (( e =  f) || ( e= g)) -> False
    						| e , And (Neg f,g) when ( e = f) -> False
    						| e , And (f,Neg g) when ( e = g) -> False
    						| And (Neg f,g),e when ( e = f) -> False
    						| And (f,Neg g),e when ( e = g) -> False    						
    						
    						| Neg e , Or (f,g) when ( e =  f)  ->   simplify_eagle ( And (Neg e, g) )
    						| Neg e , Or (f,g) when ( e =  g)  ->  simplify_eagle ( And (Neg e, f) )
    						| Or (f,g), Neg e when (e = f)  ->  simplify_eagle ( And (Neg e, g))
    						| Or (f,g), Neg e when (e = g)  ->  simplify_eagle ( And (Neg e, f))
    						
    						| e, Or(f,g) when (( e =  f) || ( e= g)) ->  e
    						| Or(f,g),e  when (( e =  f) || ( e= g)) ->  e
   				    		| e, And(f,g) when ( e =  f) ->   (And(e, g))
   				    		| e, And(f,g) when ( e= g) ->  And (f, g)
							| And(f,g),e  when ( e =  f) -> And(f, g)
   				    		| And(f,g),e when ( e= g) ->  And (f, g)
   				    		
   							| e,f -> And (e,f)

				
		)
		| Or (a,b)-> simplify_eagle ( Xor( And(a,b), Xor(a,b) ) )
		| Neg a -> simplify_eagle (Xor (True,a))
		| Iff(a,b) -> simplify_eagle (Xor (Xor (True,a),b))
		| Imp (a,b) -> simplify_eagle (Xor(True,Xor(a,And(a,b))))
	    | Wuntil (e,f) -> simplify_eagle (Or(Glob e, Until(e,f)))
    	| Until (a, b) -> (match (simplify_eagle a), (simplify_eagle b) with 
    						True , _ -> True
    						| _, True -> True
    						| False, _ -> False
    						| e , False -> if (e=True) then True else False
    						| e, f when (e = f) -> e
    						| Neg e, f when (e = f) -> (Neg e)
    						| e, Neg f when (e = f) -> e
    						| And(e,f),g when (e = g) -> simplify_eagle (Until (f,g))
    						| And(e,f),g when (f = g) -> simplify_eagle (Until (e,g))
    						| e, And(f,g) when (f = e) -> simplify_eagle (Until (e,g))
    						| e, And(f,g) when (g = e) -> simplify_eagle (Until (e,f))
							| And(e,f),g -> simplify_eagle (And((Until(e,g)),(Until(f,g))))
							| e,Or(f,g) -> simplify_eagle (Or((Until(e,f)),(Until(e,g))))
							| e,Until (f,g) when (e=f) -> Until (f,g)
   							| e,f -> Until (e,f)
   					)
   		| Ev True -> True
	    | Ev False -> False
	    | Ev Ev e -> simplify_eagle (Ev e)
	    | Ev e -> (match (simplify_eagle e) with 
	    				True -> True
	    				| False -> False
	    				| And (f,Ev(And(g,h))) when ((f =g) || (f=h)) -> Ev (And (g,h))
	    				| And (Ev(And(g,h)),f) when ((f =g) || (f=h)) -> Ev (And (g,h))
	    				| Or(f,g) -> simplify_eagle (Or(Ev f,Ev (g)))
	    				| Ev f -> Ev f
	    				| f -> Ev f
	    			)
	    | Glob e -> (match (simplify_eagle e) with 
	    				True -> True
	    				| False -> False
	    				| Glob f -> Glob f
	    				| Or (f,Glob (Or(g,h))) when ((f=g) || (f=h)) -> simplify_eagle (Glob (Or(g,h)))
	    				| And (f,Glob (And(g,h))) when ((f=g) || (f=h)) -> simplify_eagle (Glob f) 
	    				| And (f, g) -> simplify_eagle (And (Glob f, Glob g))
	    				| f -> Glob f
	    			)
	    | Next e -> (match (simplify_eagle e) with 
	    		And (f,g) -> And(Next f,Next g)
	    		| Or (f,g) -> Or (Next f, Next g)
	    		| f -> Next f
	    		)
	    | Previous  e -> (match (simplify_eagle e) with 
	    		And (f,g) -> simplify_eagle (And(Previous f,Previous g))
	    		| Or (f,g) -> simplify_eagle (Or (Previous f, Previous g))
	    		| f -> Previous f
	    		)
		
			| _ -> f



let rec simplify form =
  match form with
    | Neg e -> (match (simplify e) with 
    			True -> False
    			| False -> True 
    			| Neg f -> simplify f
    			| Next f -> Next (simplify (Neg f))
    			(*
    			| And (f,g) -> simplify (Or (Neg f,Neg g))
    			| Or (f,g) -> simplify (And (Neg f, Neg g))
    			| Glob f -> simplify(Ev (simplify (Neg f)))
    			| Ev f -> simplify (Glob (Neg f))
    			*)
    			| f -> Neg f
    			)
    | And (a, b) -> (match (simplify a), (simplify b) with
    						SHARP, f -> f
    						| f , SHARP -> f
    						
    						| True , e -> e
    						| e, True -> e
    						| False, _ -> False
    						| _, False -> False
    						| e, Neg f when e = f -> False
    						| Neg e, f when e = f -> False
    						| e,f when ((e = Neg f) || (Neg e = f)) -> False
    						| e, f when (e = f) -> e
    
    						| e, Glob f when (e = f) -> Glob f
    						| Glob f, e when (e = f) -> Glob f
    						
    						| Neg e, Glob (And(f,g)) when ((e = f) || (e=g)) -> False
    						| Glob (And(f,g)),Neg e when ((e = f) || (e=g)) -> False
    						
        					| e, Ev f when (e = f) -> e
    						| Ev e, f when (e = f) -> e
    			
    						| Neg e, Glob f when (e = f) -> False
    						| Glob f, Neg e when (e = f) -> False
    						
    						| e, Glob f when ((Neg e = f) || (e = Neg f)) -> False
    						| Glob f, e when ((Neg e = f) || (e = Neg f)) -> False
    						
    			    						
    						| And(e,f), And(g,h) when ((e=g & f=h) || ((e=h) & f=g)) -> simplify (And(e ,g))
							
							| And(e,f), Or(g,h) when ( e =  g) -> simplify ( And (e, f) )    						
							| And(e,f), Or(g,h) when ( e =  h) ->  simplify ( And (e, f) )
							| Or(g,h), And(e,f) when ( e =  g) ->  simplify ( And (e, f) )   						
							| Or(g,h), And(e,f) when ( e =  h) ->  simplify ( And (e, f) )    						
							    						
    						| Neg e , And (f,g) when (( e =  f) || ( e= g)) -> False
    						| And (f,g), Neg e  when (( e =  f) || ( e= g)) -> False
    						| e , And (Neg f,g) when ( e = f) -> False
    						| e , And (f,Neg g) when ( e = g) -> False
    						| And (Neg f,g),e when ( e = f) -> False
    						| And (f,Neg g),e when ( e = g) -> False    						
    						
    						| Neg e , Or (f,g) when ( e =  f)  ->   simplify ( And (Neg e, g) )
    						| Neg e , Or (f,g) when ( e =  g)  ->  simplify ( And (Neg e, f) )
    						| Or (f,g), Neg e when (e = f)  ->  simplify ( And (Neg e, g))
    						| Or (f,g), Neg e when (e = g)  ->  simplify ( And (Neg e, f))
    						
    						| e, Or(f,g) when (( e =  f) || ( e= g)) ->  e
    						| Or(f,g),e  when (( e =  f) || ( e= g)) ->  e
   				    		| e, And(f,g) when ( e =  f) ->   (And(e, g))
   				    		| e, And(f,g) when ( e= g) ->  And (f, g)
							| And(f,g),e  when ( e =  f) -> And(f, g)
   				    		| And(f,g),e when ( e= g) ->  And (f, g)
   				    		
   							| e,f -> And (e,f)
   					)
    						
    | Or (a, b) ->  (match (simplify a), (simplify b) with 
    						True , _ -> True
    						| _, True -> True
    						| False, e -> e
    						| e, False -> e
    						| e, Neg f when e = f -> True
    						| Neg e, f when e = f -> True
    						
    						| e, Glob f when e = f -> e
    						| Glob f, e when e = f -> e
    						
    						| Neg e, Ev (Or (f,g)) when (e = f || e= g) -> True
    						| Ev (Or (f,g)), Neg e when (e = f || e= g) -> True

    						
    						| Neg e, Ev f when (e = f) -> True
    						| Ev f, Neg e when (e = f) -> True
    						
    						| e, Ev f when (e = f) -> Ev f
    						| Ev f, e when (e = f) -> Ev f
    						    						
    						| e, Ev f when ((Neg e = f) || (e = (Neg f))) -> True
    						| Ev f, e when (((Neg e) = f) || (e = (Neg f))) -> True
    						
   						
    						
    						| Or(e,f), Or(g,h) when ((e=g & f=h) || ((e=h) & (f=g))) -> simplify (Or(e , f))
    						| And(e,f), Or(g,h) when (e = g) ->  simplify ( Or (e, And (Neg e, h)))
    						| And(e,f), Or(g,h) when (e = h) ->  simplify ( Or (e, And (Neg e, g)))
    						| Or(g,h), And(e,f) when (e = g) ->  simplify ( Or (e, And (Neg e, h)) )
    						| Or(g,h), And(e,f) when (e = h) ->  simplify ( Or (e, And (Neg e, g)))
    						
    						
    						| Neg e, Or (f,g) when (e=g || e=f) -> True
    						| Or (f,g), Neg e when (e=g || e=f) -> True
    						
    						| e, Or(f,g) when (e = f) ->  Or(e, g)
    						| e, Or(f,g) when (e = g) ->  Or( e, f)
    						| Or(f,g),e when (e = f) ->  Or(e, g)
    						| Or(f,g),e when ( e = g) ->  Or( e, f)
    						
    						| e, And(f,g) when ((e =  f) || (e = g)) ->   e
							| And(f,g),e when ((e =  f) || (e = g)) ->   e
   						
    						
 				    		
   				    		| e, f when (e = f) -> e
   				    		| e, f when (e = Neg f) -> True
   				    		| e, f when (Neg e = f) -> True
   							| e,f -> Or (e,f)
   					)

    | Until (a, b) -> (match (simplify a), (simplify b) with 
    						True , _ -> True
    						| _, True -> True
    						| False, _ -> False
    						| e , False -> if (e=True) then True else False
    						| e, f when (e = f) -> e
    						| Neg e, f when (e = f) -> (Neg e)
    						| e, Neg f when (e = f) -> e
    						| And(e,f),g when (e = g) -> simplify (Until (f,g))
    						| And(e,f),g when (f = g) -> simplify (Until (e,g))
    						| e, And(f,g) when (f = e) -> simplify (Until (e,g))
    						| e, And(f,g) when (g = e) -> simplify (Until (e,f))
							| And(e,f),g -> simplify (And((Until(e,g)),(Until(f,g))))
							| e,Or(f,g) -> simplify (Or((Until(e,f)),(Until(e,g))))
							| e,Until (f,g) when (e=f) -> Until (f,g)
   							| e,f -> Until (e,f)
   					)		
    
    | Iff (a, b) -> (match (simplify a), (simplify b) with
        				 True, e -> e
        				| e,True -> e
        				| False, e -> simplify (Neg e)
        				| e,False -> simplify (Neg e)
        				| e, Neg f when (e = f)  -> False	
  						| e,f when (e= f) -> True
    					| e,f -> simplify (And (Imp (e,f), Imp (f,e)))
    				)
    | Imp (a, b) -> (match (simplify a), (simplify b) with
    					 False,_ -> True
    					| _,True -> True
    					| True,e -> e
    					| e,False -> simplify (Neg e)
    					| e,f when (e=f) -> True
    					| e,f -> simplify (Or (Neg e, f))
    				)

    | Ev True -> True
    | Ev False -> False
    | Ev Ev e -> simplify (Ev e)
    | Ev e -> (match (simplify e) with 
    				True -> True
    				| False -> False
    				| And (f,Ev(And(g,h))) when ((f =g) || (f=h)) -> Ev (And (g,h))
    				| And (Ev(And(g,h)),f) when ((f =g) || (f=h)) -> Ev (And (g,h))
    				| Or(f,g) -> simplify (Or(Ev f,Ev (g)))
    				| Ev f -> Ev f
    				| f -> Ev f
    			)
    | Glob e -> (match (simplify e) with 
    				True -> True
    				| False -> False
    				| Glob f -> Glob f
    				| Or (f,Glob (Or(g,h))) when ((f=g) || (f=h)) -> simplify (Glob (Or(g,h)))
    				| And (f,Glob (And(g,h))) when ((f=g) || (f=h)) -> simplify (Glob f) 
    				| And (f, g) -> simplify (And (Glob f, Glob g))
    				| f -> Glob f
    			)
    | Next e -> Next (simplify e) 
    
    (* Next e -> (match (simplify e) with 
    		And (f,g) -> And(Next f,Next g)
    		| Or (f,g) -> Or (Next f, Next g)
    		| f -> Next f
    		)
    	*)
    | Previous  e -> (match (simplify e) with 
    		And (f,g) -> simplify (And(Previous f,Previous g))
    		| Or (f,g) -> simplify (Or (Previous f, Previous g))
    		| f -> Previous f
    		)
    | Wuntil (e,f) -> simplify (Or(Glob e, Until(e,f)))
    | SHARP -> SHARP
    | _ -> form


let simp = simplify

	
(*s Returns the closure of LTL formula [f] in the form of a list, e.g.,
   [closure (Glob (Var "a"))] would return [[Var "a"; Glob (Var
   "a")]]. *)

let rec closure f =
  match f with
      And (a, b) -> f::(closure a)@(closure b)
    | Or (a, b) -> f::(closure a)@(closure b)
    | Var a -> [Var a]
    | Neg a -> f::(closure a)
    | Imp (a, b) -> closure (simp (Imp (a, b)))
    | Iff (a, b) -> closure (simp (Iff (a, b)))
    | Next a -> f::(closure a)
    | Previous a -> f::(closure a)
    | Until (a, b) -> f::(closure a)@(closure b)
    | Wuntil (a, b) -> f::(closure a)@(closure b)
    | Glob a -> f::(closure a)
    | Ev a -> f::(closure a)
    | Xor (a, b) -> f::(closure a)@(closure b)
    | _ -> [f; Neg f]
    
let rec urgent_closure f =
	match  f with 
	 And (a, b) | Or (a,b) |  Iff (a,b) | Imp(a,b) | Xor (a,b) -> (urgent_closure a)@(urgent_closure b) 
    | Previous _ -> [f]
    | Neg a -> urgent_closure a
    | _ -> []
    
    (*
let rec urgent_closure f =
	match  f with 
	 And (a, b) -> (urgent_closure a)@(urgent_closure b) 
	 | Or (a,b) -> (urgent_closure a)@(urgent_closure b)
	 | Until (a,b) -> (urgent_closure a)@(urgent_closure b)
	 | Wuntil (a,b) -> (urgent_closure a)@(urgent_closure b)
	 | Iff (a,b) -> (urgent_closure a)@(urgent_closure b)
    | Previous a -> [f]
    | Neg a -> urgent_closure a
   
    | _ -> []

*)

(*s Returns a list of used variables in a formula [f], e.g., if
   [f] is [Glob (Var "a")], then [[Var "a"]] is returned. *)

let rec variables f = 
  match f with
     And (a, b) -> (variables a) @ (variables b)
    | Until (a, b) -> (variables a) @ (variables b)
    | Wuntil (a, b) -> (variables a) @ (variables b)
    | Or (a, b) -> (variables a) @ (variables b)
    | Glob a -> variables a
    | Neg a -> variables a
    | Ev a -> variables a
    | Next a -> variables a
    | Previous a -> variables a
    | Var x -> [Var x]
    | Xor (a, b) -> (variables a) @ (variables b)
    | _ -> []
