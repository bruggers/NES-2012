(* Copyright (c) 2008 Andreas Bauer <baueran@gmail.com>,
	Ylies Falcone <ylies.falcone@gmail.com> 
   Code is released under the GPL license version 3. *)

open Types
open Common_test
open Alphabet_parser
open Ltl
open Trace
open Headers

let nbtests = ref 0
let sizeform = ref 0
let maxsizeform = ref 0
let sizetrace = ref 0
let dalphabet_string = ref ""
let dalphabet = ref []
let alphabet_string = ref ""
let alphabet = ref alphabet
let abscence = ref false
let existence = ref false
let bexistence = ref false
let universality = ref false
let response = ref false
let precedence_chain = ref false
let precedence = ref false
let response_chain= ref false
let constrained_chain= ref false



let cent_trace_len = ref 0 and decent_trace_len = ref 0 and cent_num_mess = ref 0 and decent_num_mess = ref 0 and cent_size_mess = ref 0 and decent_size_mess = ref 0

let delay = ref 0
let formula = ref False
 
let max_delay = ref 0
 
let rec gen_space (n:int) =
	if (n<=0) then ""
	else " "^gen_space(n-1)
	
let gen_cell (s:string) (n:int)=
	gen_space((n-(String.length s))/2)^s^gen_space((n-(String.length s))/2)

let adjust_string (s:string) (n:int) =
	if (String.length s = n) then s
	else (if (String.length s <= n) then s^(gen_space(n-(String.length s))) else s) 

let usage = "usage: " ^ Sys.argv.(0) ^ " [-n int]"
 
let speclist = [
    ("-n", Arg.Int    (fun n -> nbtests := n),  ": the number of tests to run");
    ("-sf", Arg.Int    (fun sf -> sizeform := sf),      ": the size of the formula");
    ("-msf", Arg.Int    (fun msf -> maxsizeform := msf),      ": the maximum size of the formula (will test from size 1 to the value provided)");
    ("-st", Arg.Int    (fun st -> sizetrace := st),      ": the size of the trace");
    ("-dalpha", Arg.String  (fun dalpha -> dalphabet_string := dalpha),     ": the decentralized alphabet");
    ("-alpha", Arg.String  (fun alpha -> alphabet_string := alpha),     ": the centralized alphabet (will consider possible dalphabets generated from it)");
    ("-abs", Arg.Bool  (fun abs -> abscence := abs),     ": testing for abscence patterns");
    ("-exis", Arg.Bool  (fun exis -> existence := exis),     ": testing for existence patterns");
    ("-bexis", Arg.Bool  (fun bexis -> bexistence := bexis),     ": testing for bounded existence patterns");
    ("-univ", Arg.Bool  (fun univ -> universality := univ),     ": testing for universality patterns");
    ("-prec", Arg.Bool  (fun prec -> precedence := prec),     ": testing for precedence patterns");
    ("-resp", Arg.Bool  (fun resp -> response := resp),     ": testing for response patterns");
    ("-precc", Arg.Bool  (fun precc -> precedence_chain := precc),     ": testing for precedence chain patterns");
    ("-respc", Arg.Bool  (fun respc -> response_chain:= respc),     ": testing for response chain patterns");
    ("-consc", Arg.Bool  (fun consc -> constrained_chain:= consc),     ": testing for constrained chain patterns");
    ("-prt_trace_mess", Arg.Bool  (fun prttracemess -> print_trace_and_mess_stats := prttracemess),     ": printing trace and number of messages statistics");
    ("-prt_delay", Arg.Bool  (fun prtdelay -> print_delay_stats := prtdelay),     ": printing delay statistics");
    ("-prt_full", Arg.Bool  (fun prtfull -> print_full_stats := prtfull),     ": printing full statistics");
    ("-flipcoin", Arg.Unit (fun x -> Trace.the_distrib := FLIPCOIN), "Using the flipcoin probability distribution (uniform distribution with probabily 0.5)");
    ("-bernouilli", Arg.Float (fun seed -> Trace.the_distrib := BERNOUILLI; Trace.seed := seed), "Using the BERNOUILLI probability distribution (uniform distribution with a probability given as an argument)");
    ("-expo", Arg.Float (fun seed -> Trace.the_distrib := EXPO; Trace.seed := seed), "Using the EXPONENTIAL probability distribution (the rate parameter is given as an argument)");
    ("-beta", Arg.Tuple [Arg.Float (fun seed -> Trace.the_distrib := BETA; Trace.seeda := seed);
    	Arg.Float (fun seed -> Trace.seedb := seed)],
    	"Using the BETA probability distribution (the rate parameters are given as arguments)");

    	
    ("-only_changes", Arg.Unit  (fun x -> Common_test.mode := SEND_CHANGES),": components send the value of their propositions only if there is a change in its value. More precisely, if among the monitors, there is atleast the value of one proposition that is modified wrt the previous event, the component has to send a message to the monitor");

  ]
  
  

let perform_test (size_form:int) =
    !cent_trace_len <- 0;
    !decent_trace_len <- 0;
    !cent_num_mess <- 0;
    !decent_num_mess <- 0;
    !cent_size_mess <- 0;
    !decent_size_mess <- 0;
    !max_delay <- 0;
	for i=1 to !nbtests do
		let (cent_trace_len_tmp,decent_trace_len_tmp,cent_num_mess_tmp,decent_num_mess_tmp,cent_size_mess_tmp,decent_size_mess_tmp) = generate_compared_results_efficient !dalphabet size_form !sizetrace in (
		    !cent_trace_len <- !cent_trace_len+cent_trace_len_tmp;
		    !decent_trace_len <- !decent_trace_len + decent_trace_len_tmp;
		    !cent_num_mess <- !cent_num_mess + cent_num_mess_tmp;
		    !decent_num_mess <- !decent_num_mess + decent_num_mess_tmp;
			!delay <- !delay + decent_trace_len_tmp - cent_trace_len_tmp;	
		    if (!max_delay < (decent_trace_len_tmp-cent_trace_len_tmp))
		    	then (!max_delay <- decent_trace_len_tmp-cent_trace_len_tmp)
		    );
		   (* !cent_size_mess <- !cent_size_mess + cent_size_mess_tmp;
		    !decent_size_mess <- !decent_size_mess + decent_size_mess_tmp; *)
		
		    
	done;
		let r_sizeform = string_of_int (size_form)
			 and r_size_alpha =  if (String.length (string_rep_alphabet (globalAlphabet(!dalphabet))) <21) then string_rep_alphabet (globalAlphabet(!dalphabet)) else "-"
			 and r_size_dalpha = if (String.length (string_rep_dalphabet !dalphabet) <21) then string_rep_dalphabet !dalphabet else "-"
			 and r_cent_trace_len = string_of_float (float_of_int(!cent_trace_len) /. float_of_int(!nbtests))
			 and r_cent_num_mess = string_of_float(float_of_int(!cent_num_mess) /. float_of_int(!nbtests))
			 and r_decent_trace_len = string_of_float(float_of_int(!decent_trace_len) /. float_of_int(!nbtests))
			 and r_decent_num_mess = string_of_float(float_of_int(!decent_num_mess) /. float_of_int(!nbtests))
		 	 and r_diff_trace_len = string_of_float(floor (10000.*. float_of_int(!decent_trace_len) /.float_of_int(!cent_trace_len) )/. 10000.)
			 and r_diff_num_mess = string_of_float(floor (10000.*. float_of_int(!decent_num_mess)/. float_of_int(!cent_num_mess) )/. 10000.)
			 and r_delay = string_of_float(floor (10000.*. float_of_int(!delay)/. float_of_int(!delay) )/. 10000.)
             and r_max = string_of_int(!max_delay)
			(* and r_cent_size_mess = string_of_float(float_of_int(!cent_size_mess) /. float_of_int(!nbtests)) *)
			 (* and r_decent_size_mess = string_of_float(float_of_int(!decent_size_mess) /. float_of_int(!nbtests)) *)
			(* and r_diff_size_mess = string_of_float(floor (10000.*. float_of_int(!decent_size_mess)/. float_of_int(!cent_size_mess) )/. 10000.) *)
			 in
		
		let r_sizeform = gen_cell r_sizeform 8
			and r_size_alpha = gen_cell r_size_alpha 20
			and r_size_dalpha = gen_cell r_size_dalpha 20
			and r_cent_trace_len = gen_cell r_cent_trace_len 11
			and r_cent_num_mess = gen_cell r_cent_num_mess 9
			and r_decent_trace_len = gen_cell r_decent_trace_len 11
			and r_decent_num_mess = gen_cell r_decent_num_mess 9
			and r_diff_trace_len = gen_cell r_diff_trace_len 11
			and r_diff_num_mess = gen_cell r_diff_num_mess 11
			and r_delay = gen_cell r_delay 7
			and r_max = gen_cell r_max 5
		(*	and r_cent_size_mess = gen_cell r_cent_size_mess 9 *)
		(* and r_decent_size_mess = gen_cell r_decent_size_mess 9*)
		(* and r_diff_size_mess = gen_cell r_diff_size_mess 11 *)
		
 in
			 
			 let r_sizeform = adjust_string r_sizeform 8
			 and r_size_alpha = adjust_string r_size_alpha 20
			 and r_size_dalpha = adjust_string r_size_dalpha 20
			 and r_cent_trace_len = adjust_string r_cent_trace_len 11
			 and r_cent_num_mess =  adjust_string r_cent_num_mess 9
			 and r_decent_trace_len =  adjust_string r_decent_trace_len 11
			 and r_decent_num_mess =  adjust_string r_decent_num_mess 9
			 and r_diff_trace_len = adjust_string r_diff_trace_len 11
			 and r_diff_num_mess = adjust_string r_diff_num_mess 11
			 and r_delay = adjust_string r_delay 7
			 and r_max = adjust_string r_max 5
			(* and r_decent_size_mess =  adjust_string r_decent_size_mess 9
			 and r_cent_size_mess =  adjust_string r_cent_size_mess 9 *)
			 (* and r_diff_size_mess = adjust_string r_diff_size_mess 11 *)
		 in
		if (!print_full_stats) then ( 
(*			print_endline("--------------------------------------------------||-------------------------------||-------------------------------||-----------------------------------||-------------");
*)
			print_separator0 ();
			print_endline (r_sizeform^"|"^r_size_alpha^"|"^r_size_dalpha^"||"^r_cent_trace_len^"|"^r_cent_num_mess^"||"^r_decent_trace_len^"|"^r_decent_num_mess^"||"^r_diff_trace_len^"|"^r_diff_num_mess^"||"^r_delay^"|"^r_max)
		);
		if (!print_delay_stats) then ( 
			print_endline("--------------------------------------------------||--------------");
			print_endline (r_sizeform^"|"^r_size_alpha^"|"^r_size_dalpha^"||"^r_delay^"|"^r_max)
		);
		if (!print_trace_and_mess_stats) then ( 
			print_endline("--------------------------------------------------||---------------------||---------------------||----------------------");
			print_endline (r_sizeform^"|"^r_size_alpha^"|"^r_size_dalpha^"||"^r_cent_trace_len^"|"^r_cent_num_mess^"||"^r_decent_trace_len^"|"^r_decent_num_mess^"||"^r_diff_trace_len^"|"^r_diff_num_mess)
		)
		
	(*	
		print_endline (r_sizeform^"|"^r_size_alpha^"|"^r_size_dalpha^"||"^r_cent_trace_len^"|"^r_cent_num_mess^"|"^r_cent_size_mess^"||"^r_decent_trace_len^"|"^r_decent_num_mess^"|"^r_decent_size_mess^"||"^r_diff_trace_len^"|"^r_diff_num_mess^"|"^r_diff_size_mess^"||"^r_delay^"|"^r_max)
		);
		if (!print_delay_stats) then ( 
			print_endline("--------------------------------------------------||--------------");
			print_endline (r_sizeform^"|"^r_size_alpha^"|"^r_size_dalpha^"||"^r_delay^"|"^r_max)
		);
		if (!print_trace_and_mess_stats) then ( 
			print_endline("--------------------------------------------------||---------------------||---------------------||----------------------");
			print_endline (r_sizeform^"|"^r_size_alpha^"|"^r_size_dalpha^"||"^r_cent_trace_len^"|"^r_cent_num_mess^"|"^r_cent_size_mess^"|"^r_cent_size_mess^"||"^r_decent_trace_len^"|"^r_decent_num_mess^"|"^r_decent_size_mess^"||"^r_diff_trace_len^"|"^r_diff_num_mess^"|"^r_diff_size_mess)
		)
		*)
		



let perform_test_patterns (kind:string) =
    !cent_trace_len <- 0;
    !decent_trace_len <- 0;
    !cent_num_mess <- 0;
    !decent_num_mess <- 0;
    !cent_size_mess <- 0;
    !decent_size_mess <- 0;
    !max_delay <- 0;
	for i=1 to !nbtests do
		let (form,cent_trace_len_tmp,decent_trace_len_tmp,cent_num_mess_tmp,decent_num_mess_tmp,cent_size_mess_tmp,decent_size_mess_tmp) = generate_compared_results_efficient_patterns !dalphabet kind !sizetrace in (
		    !cent_trace_len <- !cent_trace_len+cent_trace_len_tmp;
		    !decent_trace_len <- !decent_trace_len + decent_trace_len_tmp;
		    !cent_num_mess <- !cent_num_mess + cent_num_mess_tmp;
		    !cent_size_mess <- !cent_size_mess + cent_size_mess_tmp;
		    !decent_num_mess <- !decent_num_mess + decent_num_mess_tmp;
		    !decent_size_mess <- !decent_size_mess + decent_size_mess_tmp;
		    formula := form;
			if (!max_delay < (decent_trace_len_tmp-cent_trace_len_tmp))
		    	then (!max_delay <- decent_trace_len_tmp-cent_trace_len_tmp)
		    )
		done;
		let r_form = kind
			 and r_size_alpha =  if (String.length (string_rep_alphabet (globalAlphabet(!dalphabet))) <21) then string_rep_alphabet (globalAlphabet(!dalphabet)) else "-"
			 and r_size_dalpha = if (String.length (string_rep_dalphabet !dalphabet) <21) then string_rep_dalphabet !dalphabet else "-"
			 and r_cent_trace_len = string_of_float (float_of_int(!cent_trace_len) /. float_of_int(!nbtests))
			 and r_cent_num_mess = string_of_float(float_of_int(!cent_num_mess) /. float_of_int(!nbtests))
			 and r_decent_trace_len = string_of_float(float_of_int(!decent_trace_len) /. float_of_int(!nbtests))
			 and r_decent_num_mess = string_of_float(float_of_int(!decent_num_mess) /. float_of_int(!nbtests))
			 and r_diff_trace_len = string_of_float(floor (10000.*. float_of_int(!decent_trace_len) /.float_of_int(!cent_trace_len) )/. 10000.)
			 and r_diff_num_mess = string_of_float(floor (10000.*. float_of_int(!decent_num_mess)/. float_of_int(!cent_num_mess) )/. 10000.)
 			 and r_delay =  string_of_float((float_of_int(!decent_trace_len)-.float_of_int(!cent_trace_len)) /. float_of_int(!nbtests))
 			 and r_max = string_of_int(!max_delay)
		(*	and r_cent_size_mess = string_of_float(float_of_int(!cent_size_mess) /. float_of_int(!nbtests))
 			and r_decent_size_mess = string_of_float(float_of_int(!decent_size_mess) /. float_of_int(!nbtests))
 		 	and r_diff_size_mess = string_of_float(floor (10000.*. float_of_int(!decent_size_mess)/. float_of_int(!cent_size_mess) )/. 10000.) *)
 		
 in
		
		let r_form = gen_cell r_form 11
			and r_size_alpha = gen_cell r_size_alpha 22
			and r_size_dalpha = gen_cell r_size_dalpha 22
			and r_cent_trace_len = gen_cell r_cent_trace_len 11
			and r_cent_num_mess = gen_cell r_cent_num_mess 9
			and r_decent_trace_len = gen_cell r_decent_trace_len 11
			and r_decent_num_mess = gen_cell r_decent_num_mess 9
			and r_diff_trace_len = gen_cell r_diff_trace_len 11
			and r_diff_num_mess = gen_cell r_diff_num_mess 10
		(*	and r_cent_size_mess = gen_cell r_cent_size_mess 9
			and r_decent_size_mess = gen_cell r_decent_size_mess 9
			and r_diff_size_mess = gen_cell r_diff_size_mess 11 *)

			and r_delay = gen_cell r_delay 8
			and r_max = gen_cell r_max 8
			in
			 
		let r_form = adjust_string r_form 11
			 and r_size_alpha = adjust_string r_size_alpha 22
			 and r_size_dalpha = adjust_string r_size_dalpha 22
			 and r_cent_trace_len = adjust_string r_cent_trace_len 11
			 and r_cent_num_mess = adjust_string r_cent_num_mess 9
		     and r_decent_trace_len = adjust_string r_decent_trace_len 11
			 and r_decent_num_mess = adjust_string r_decent_num_mess 9
			 and r_diff_trace_len = adjust_string r_diff_trace_len 11
			 and r_diff_num_mess = adjust_string r_diff_num_mess 10
		(*	and r_decent_size_mess =  adjust_string r_decent_size_mess 9
			and r_cent_size_mess =  adjust_string r_cent_size_mess 9
			and r_diff_size_mess = adjust_string r_diff_size_mess 11 *)
			and r_delay = adjust_string r_delay 8
			 and r_max = adjust_string r_max 8
		 in
 		if (!print_full_stats) then ( 
print_separator_pattern0 ();
	(*	print_endline(r_form^"|"^r_size_alpha^"|"^r_size_dalpha^"||"^r_cent_trace_len^"|"^r_cent_num_mess^"|"^r_cent_size_mess^"||"^r_decent_trace_len^"|"^r_decent_num_mess^"|"^r_decent_size_mess^"||"^r_diff_trace_len^"|"^r_diff_num_mess^"|"^r_diff_size_mess^"||"^r_delay^"|"^r_max)
		);
		*)
				print_endline(r_form^"|"^r_size_alpha^"|"^r_size_dalpha^"||"^r_cent_trace_len^"|"^r_cent_num_mess^"||"^r_decent_trace_len^"|"^r_decent_num_mess^"||"^r_diff_trace_len^"|"^r_diff_num_mess^"||"^r_delay^"|"^r_max)
		);
		
		
		if (!print_delay_stats) then ( 
			print_endline("---------------------------------------------------------||---------------");
			print_endline (r_form^"|"^r_size_alpha^"|"^r_size_dalpha^"||"^r_delay^"|"^r_max)
		);
		if (!print_trace_and_mess_stats) then ( 
(*print_endline("---------------------------------------------------------||-------------------------------||-------------------------------||-----------------------------------");
*)
print_endline("---------------------------------------------------------||----------------------||----------------------||--------------------------");
			print_endline (r_form^"|"^r_size_alpha^"|"^r_size_dalpha^"||"^r_cent_trace_len^"|"^r_cent_num_mess^"||"^r_decent_trace_len^"|"^r_decent_num_mess^"||"^r_diff_trace_len^"|"^r_diff_num_mess)
			
		(*			print_endline (r_form^"|"^r_size_alpha^"|"^r_size_dalpha^"||"^r_cent_trace_len^"|"^r_cent_num_mess^"|"^r_cent_size_mess^"||"^r_decent_trace_len^"|"^r_decent_num_mess^"|"^r_decent_size_mess^"||"^r_diff_trace_len^"|"^r_diff_num_mess^"|"^r_diff_size_mess)	
		*)
		)


let do_pattern_test_group_results (kind:string) (dalphabets_to_consider: d_alphabet list) (alpha:alphabet) =
	!cent_trace_len <- 0;
    !decent_trace_len <- 0;
    !cent_num_mess <- 0;
    !decent_num_mess <- 0;
	for i=0 to (List.length dalphabets_to_consider)-1 do
			dalphabet:= List.nth dalphabets_to_consider i;
			for i=1 to !nbtests do
				let (form,cent_trace_len_tmp,decent_trace_len_tmp,cent_num_mess_tmp,decent_num_mess_tmp,cent_size_mess_tmp,decent_size_mess_tmp) = 
					generate_compared_results_efficient_patterns !dalphabet kind !sizetrace in (
		    !cent_trace_len <- !cent_trace_len+cent_trace_len_tmp;
		    !decent_trace_len <- !decent_trace_len + decent_trace_len_tmp;
		    !cent_num_mess <- !cent_num_mess + cent_num_mess_tmp;
		    !decent_num_mess <- !decent_num_mess + decent_num_mess_tmp;
		    !cent_size_mess <- !cent_size_mess + cent_size_mess_tmp;
		    !decent_size_mess <- !decent_size_mess + decent_size_mess_tmp;
		    formula := form;
		    !delay <- !delay + decent_trace_len_tmp - cent_trace_len_tmp;	
			if (!max_delay < (decent_trace_len_tmp-cent_trace_len_tmp))
		    	then (!max_delay <- decent_trace_len_tmp-cent_trace_len_tmp)
		    )
		    
			done;
	done;
	let r_form = kind
			 and r_size_alpha =  string_rep_alphabet alpha
			 and r_size_dalpha = string_of_int(List.length (List.hd dalphabets_to_consider))
			 and r_cent_trace_len = string_of_float (floor (100.*.(float_of_int(!cent_trace_len) /. float_of_int(!nbtests) /. float_of_int ((List.length dalphabets_to_consider)))) /. 100.)
			 and r_cent_num_mess = string_of_float(floor (100.*. float_of_int(!cent_num_mess) /. float_of_int(!nbtests) /. float_of_int ((List.length dalphabets_to_consider))) /. 100.)
			 and r_decent_trace_len = string_of_float(floor (100.*. float_of_int(!decent_trace_len) /. float_of_int(!nbtests) /. float_of_int ((List.length dalphabets_to_consider)))/. 100.)
			 and r_decent_num_mess = string_of_float(floor (100.*. float_of_int(!decent_num_mess) /. float_of_int(!nbtests) /. float_of_int ((List.length dalphabets_to_consider)))/. 100.) 
			 and r_diff_trace_len = string_of_float(floor (10000.*. float_of_int(!decent_trace_len) /.float_of_int(!cent_trace_len) )/. 10000.)
			 and r_diff_num_mess = string_of_float(floor (10000.*. float_of_int(!decent_num_mess)/. float_of_int(!cent_num_mess) )/. 10000.)
			 and r_delay = string_of_float(floor (10000.*. float_of_int(!delay)/. float_of_int(!delay) )/. 10000.)
 			 and r_max = string_of_int(!max_delay)
 		in
		
		let r_form = gen_cell r_form 11
			and r_size_alpha = gen_cell r_size_alpha 12
			and r_size_dalpha = gen_cell r_size_dalpha 14
			and r_cent_trace_len = gen_cell r_cent_trace_len 11
			and r_cent_num_mess = gen_cell r_cent_num_mess 9
			and r_decent_trace_len = gen_cell r_decent_trace_len 11
			and r_decent_num_mess = gen_cell r_decent_num_mess 9
			and r_diff_trace_len = gen_cell r_diff_trace_len 11
			and r_diff_num_mess = gen_cell r_diff_num_mess 9
			and r_delay = gen_cell r_delay 5
			and r_max = gen_cell r_max 5 
 in
			 
		let r_form = adjust_string r_form 11
			 and r_size_alpha = adjust_string r_size_alpha 12
			 and r_size_dalpha = adjust_string r_size_dalpha 14
			 and r_cent_trace_len = adjust_string r_cent_trace_len 11
			 and r_cent_num_mess = adjust_string r_cent_num_mess 9
			 and r_decent_trace_len = adjust_string r_decent_trace_len 11
			 and r_decent_num_mess = adjust_string r_decent_num_mess 9
			 and r_diff_trace_len = adjust_string r_diff_trace_len 11
			 and r_diff_num_mess = adjust_string r_diff_num_mess 9
			  and r_delay = adjust_string r_delay 5
			 and r_max = adjust_string r_max 5
		 in
print_endline("-----------|------------|--------------||-----------|---------||-----------|---------||-----------|----------");
		print_endline(r_form^"|"^r_size_alpha^"|"^r_size_dalpha^"||"^r_cent_trace_len^"|"^r_cent_num_mess^"||"^r_decent_trace_len^"|"^r_decent_num_mess^"||"^r_diff_trace_len^"|"^r_diff_num_mess^"||"^r_delay^"|"^r_max)



 let do_pattern_test (name:string) =
 		if (!dalphabet_string <> "") then (
	  			perform_test_patterns(name);
	  		)
	  	else (if (!alphabet_string <> "") then (
				for size_dalpha=1 to List.length !alphabet do
					let dalphabets_to_consider = generate_compatible_dalphabet !alphabet size_dalpha in
						do_pattern_test_group_results name dalphabets_to_consider !alphabet
				done;	  		
	  		)
	  	)


let _ =

	welcome();
	
	
	 
    (* Read the arguments *)
  Arg.parse
    speclist
    (fun x -> raise (Arg.Bad ("Bad argument : " ^ x)))
    usage;
  Random.self_init();
  
  (* 
  Check that the arguments are correctly passed to the program
  *)
  if (!nbtests<=0) then
  	print_endline ("Please provide a positive number of tests [-n int]");
  if (!sizeform<=0 & !maxsizeform <=0 & not !abscence & not !existence & not !bexistence & not !universality & not !precedence & not !response & not !response_chain & not !precedence_chain & not !constrained_chain) then
  	print_endline ("Please provide a positive number for the size of the formula (either [-sf int] or [-msf int]) or a specification pattern to be tested");
  if (!sizetrace<=0) then
  	print_endline ("Please provide a positive number for the size of the trace [-st int]");
  (if (!dalphabet_string ="" & !alphabet_string="") then
  	  	print_endline ("Please provide a dalphabet or an alphabet [-dalpha string_representation_of_the_d_alphabet] or [-alpha string_representation_of_the_alphabet]")
  else
  	if (!dalphabet_string <> "") then dalphabet := parse_dalphabet_string !dalphabet_string;
  	if (!alphabet_string <> "") then alphabet := parse_alphabet_string !alphabet_string;
	);
	
	(* Check the options for displaying statistics *)
	if ((not !print_full_stats & not !print_delay_stats & not !print_trace_and_mess_stats)) then
	print_endline("Please provide some statistics to be displayed: either -prt_trace_mess [bool] or -prt_delay [bool] or -prt_full [bool]");
	
	(* If one of the options has failed, then exit *)
	if (!nbtests<=0 || (!sizeform<=0 & !maxsizeform <=0 & not !abscence & not !existence & not !bexistence & not !universality & not !precedence & not !response & not !response_chain & not !precedence_chain & not !constrained_chain) || !sizetrace<=0 || (!dalphabet_string ="" & !alphabet_string="") || (not !print_full_stats & not !print_delay_stats & not !print_trace_and_mess_stats))
	then exit(1);

	
	
	(* Recall the options requested by the user in an explicit manner *)
	print_endline("For each entry line, number of formulae tested: "^string_of_int !nbtests);
	if (!sizeform >0) then
		print_endline("Running benchmarks for formulae of size: "^string_of_int !sizeform);
	if (!maxsizeform >0) then
		print_endline("Running benchmarks for formulae of maximum size: "^string_of_int !maxsizeform^" (i.e., it will do a complete bench for each formula size between 1 and "^string_of_int !maxsizeform^")");
	if (!abscence || !existence || !universality || !precedence|| !response || !response_chain || !precedence_chain || !constrained_chain) then (
		print_string("The following LTL specification pattern(s) will be tested: ");
		if (!abscence) then print_string(" abscence ");
		if (!existence) then print_string(" existence ");
		if (!bexistence) then print_string(" bounded_existence ");
		if (!universality) then print_string(" universality ");
		if (!precedence) then print_string(" precedence ");
		if (!response) then print_string(" response ");
		if (!response_chain) then print_string(" response_chain ");
		if (!precedence_chain) then print_string(" precedence_chain ");
		if (!constrained_chain) then print_string(" constrained_chain ");
		print_endline("");
	)
	if (!sizetrace >0) then
		print_endline("Each formula will be monitored against a freshly randomly generated trace of size: "^string_of_int !sizetrace);
		
	print_endline ("The probability distribution that will be used for the trace is "^(match !Trace.the_distrib with FLIPCOIN -> "flipcoin" | BERNOUILLI -> "bernouilli" | EXPO -> "exponential" | BETA -> "beta"))
	if (!dalphabet_string <>"") then
		print_endline("Distributed alphabet used: "^ !dalphabet_string);
		
	
	if (!Common_test.mode = SEND_CHANGES)	then print_endline("Components send the value of their propositions only if there is a change in its value");
	
	if (!print_full_stats) then
		print_endline("Full statistics will be displayed.");
	if (!print_delay_stats) then
		print_endline("Delay statistics will be displayed.");
	if (!print_trace_and_mess_stats) then
		print_endline("Trace length and messages statistics will be displayed.");
	
	
	
	(* Run benchmarks and display results *)
	print_endline("\nResults:\n");
	if (!sizeform>0 || !maxsizeform >0) then (
	  	print_header();
	  	if (!dalphabet_string <> "") then (
			if (!maxsizeform > 0) then
				for sizeform_tmp=1 to !maxsizeform do
					perform_test(sizeform_tmp);
				
				done;
			if (!sizeform > 0) then 
				perform_test(!sizeform);
		);
		if (!alphabet_string <> "") then (
			for size_dalpha=1 to List.length !alphabet do
				let dalphabets_to_consider = generate_compatible_dalphabet !alphabet size_dalpha in
				for cpt=0 to (List.length dalphabets_to_consider)-1 do
					dalphabet:= List.nth dalphabets_to_consider cpt;
					if (!maxsizeform > 0) then
						for sizeform_tmp=1 to !maxsizeform do
							perform_test(sizeform_tmp);
				
						done;
					if (!sizeform > 0) then 
						perform_test(!sizeform);
				done;
			done;
		)
	)
	else (
		if (!dalphabet_string <> "") then print_header_patterns();
		if (!alphabet_string <> "") then print_header_patterns_alpha();
		if (!abscence) then (
	  		do_pattern_test("absence");
	  	);
	  	if (!existence) then (
	  		do_pattern_test("exist");
	  	);
	  	if (!bexistence) then (
	  		do_pattern_test("bexist");
	  	);
	  	if (!universality) then (
	  		do_pattern_test("unive");
	  	);
	  	if (!precedence) then (
	  		do_pattern_test("prec");
	  	);
	  	if (!response) then (
	  		do_pattern_test("response");
	  	);
	  	if (!precedence_chain) then (
	  		do_pattern_test("prec_chain");
	  	);	  	
	  	if (!response_chain) then (
	  		do_pattern_test("resp_chain");
	  	);
	  	if (!constrained_chain) then (
	  		do_pattern_test("consc_chain");
	  	);
			
	)