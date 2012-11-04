(* Copyright (c) 2008 Andreas Bauer <baueran@gmail.com>, Ylies Falcone <ylies.falcone@gmail.com> 
   Code is released under the GPL license version 3. *)
(* The following functions require Ocaml's preprocessor [camlp4]. *)

(*i*)
open Genlex
open Stream
open Printf
open Types
open IO
open Std
open List
(*i*)

(*s Bit of an ugly definition to make a lexer consisting of the
  keywords in the given list.  [lexer] is basically a function which
  takes an input stream and produces an output token stream. *)
  
let lexerTrace = 
  Genlex.make_lexer 
    ["{"; "}"; ";"; ","; "|"]

(*s Definition of a recursive-descent stream parser. [parse_formula]
   is a function which takes a token stream and returns an LTL
   formula.  It is mutually recursive, giving rise to precedence of
   operators.  *)

let rec parse_trace = parser
  | [< t1 = parse_event; stream >] -> 
  	( parser 
  		 | [< 'Kwd ";" ; t2 = parse_trace >] -> [t1]@t2
 		 | [< >] -> [t1] ) stream
  
and parse_event = parser
 	[< 'Kwd "{" ; e=parse_comma ; 'Kwd "}">] -> e   
	
and parse_comma = parser
  | [< e1 = parse_atom; stream >] ->
      (parser  
      	| [< 'Kwd ","; e2 = parse_comma >] -> [e1]@e2 
      	| [< >] -> [e1] ) stream
  | [< >] -> []
  
 and parse_atom = parser
 	| [< 'Ident c ; stream >] -> c
      
let parse_trace_string trace_string =
	parse_trace (lexerTrace (Stream.of_string trace_string))
  
let parse_trace_in_file file_name =
	List.map parse_trace_string (input_list (open_in file_name))



let rec parse_dtrace = parser
  | [< t1 = parse_devent; stream >] -> 
  	( parser 
  		 | [< 'Kwd ";" ; t2 = parse_dtrace >] -> [t1]@t2
 		 | [< >] -> [t1] ) stream
    
and parse_devent = parser
 	[< 'Kwd "{" ; e=parse_mid ; 'Kwd "}">] -> e   
	
and parse_mid = parser
  | [< e1 = parse_comma; stream >] ->
      (parser  
      	| [< 'Kwd "|"; e2 = parse_mid >] -> [e1]@e2 
      	| [< >] -> [e1] ) stream
  | [< >] -> []
  
and parse_comma = parser
  | [< e1 = parse_atom; stream >] ->
      (parser  
      	| [< 'Kwd ","; e2 = parse_comma >] -> [e1]@e2 
      	| [< >] -> [e1] ) stream
  | [< >] -> []

and parse_atom = parser
 	| [< 'Ident c ; stream >] -> c

let parse_dtrace_string dtrace_string =
	parse_dtrace (lexerTrace (Stream.of_string dtrace_string))
  
let parse_trace_in_file file_name =
	List.map parse_trace_string (input_list (open_in file_name))
