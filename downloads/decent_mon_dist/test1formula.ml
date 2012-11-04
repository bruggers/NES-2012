(* Copyright (c) 2008 Andreas Bauer <baueran@gmail.com>, Ylies Falcone <ylies.falcone@gmail.com> 
   Code is released under the GPL license version 3.
 *)
   
open Types
open Ltl
open Progression
open D_progression
open Ltl_parser
open Ltl_generator
open Alphabet_parser
open Trace
open Trace_parser
open Unix
open Arg
open Common_test

(* default values*)
let verbose = ref false
let formula = ref ""
let dtrace = ref ""
let dalphabet = ref ""
 
let usage = "usage: " ^ Sys.argv.(0) ^ " [-v] [-s string] [-d int]"
 
let speclist = [
    ("-v", Arg.Unit   (fun () -> verbose := true), ": verbose mode");
    ("-f", Arg.String (fun f -> formula := f),      ": what follows -f sets the formula");
    ("-dt", Arg.String (fun dt -> dtrace := dt),      ": what follows -dt sets the dtrace");
    ("-dalpha", Arg.String (fun da -> dalphabet := da),      ": what follows -dalpha sets the dalphabet");
  ]
 
let () =
  Random.self_init();

  (* Read the arguments *)
  Arg.parse
    speclist
    (fun x -> raise (Arg.Bad ("Bad argument : " ^ x)))
    usage;
 
 Printf.printf " %b '%s' '%s'\n" !verbose !formula !dtrace;
 if(!verbose) then (
 	Printf.printf "\nFormula '%s'" !formula;
 	Printf.printf "\nD-Trace '%s'" !dtrace;
 	Printf.printf "\nD-Alphabet '%s'" !dalphabet;
)
 if (not ((!formula)="") && (not ((!dtrace) = "")) && (not ((!dalphabet) = ""))) then
 	if (!verbose) then 
 		test_d_monitor_1_form_1_trace_verbose (parse_formula_string !formula) (parse_dalphabet_string !dalphabet) (parse_dtrace_string !dtrace)
	else (
		test_d_monitor_1_form_1_trace (parse_formula_string !formula) (parse_dalphabet_string !dalphabet) (parse_dtrace_string !dtrace)
	)
else
	Printf.printf "Wrong arguments passed to the program.\n Arguments -f and -dt -dalpha are mandatory.\nYou provided '%s', '%s', '%s' \n" !formula !dtrace !dalphabet; 
  
