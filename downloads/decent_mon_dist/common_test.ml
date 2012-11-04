(* Copyright (c) 2008 Andreas Bauer <baueran@gmail.com>, Ylies Falcone <ylies.falcone@gmail.com> 
   Code is released under the GPL license version 3. *)
open Types
open Ltl
open Progression
open D_progression
open Ltl_parser
open Ltl_generator
open Trace
open Trace_parser
open Alphabet_parser
open Unix
open List
open Utils

let alphabet = ["a";"b";"c"]
let dalphabet = [["a"];["b"];["c"]]


let test_simp_1_form  (f:ltl_formula) = 
	print_endline ("Simplifying "^string_rep_input f);
	print_endline (" -> "^(string_rep_input (simp f)))
	
let test_simp_1_form_random (alpha:alphabet) (size_form:int) (past:bool) =
	test_simp_1_form (gen_1_form size_form alpha past)

let test_monitor_1_trace_1_formula_old  (f:ltl_formula) (t:trace) = 
	print_endline ("Monitoring "^string_rep_input (simp f));
	print_endline (" against the trace "^(string_rep_trace t));
	let (verdict,witness) = monitor_old (simp f) t [] in
		print_endline (" -> "^string_rep verdict);
		print_endline (" after "^(string_rep_trace witness))
		
		
let test_monitor_1_trace_1_formula  (f:ltl_formula) (t:trace) = 
	print_endline ("Monitoring "^string_rep_input (simp f));
	print_endline (" against the trace "^(string_rep_trace t));
	let (verdict,witness) = monitor (simp f) t in
		print_endline (" -> "^string_rep verdict);
		print_endline (" after "^(string_rep_trace witness))
		
let test_monitor_1_randomTrace_1_randomFormula (alpha:alphabet) (size_form:int) (size_trace:int) (past:bool) = 
	test_monitor_1_trace_1_formula (gen_1_form size_form alpha past) (gen_1_trace size_trace alpha)


let test_count_message (f:ltl_formula) (t:d_trace) (alpha:d_alphabet) =
	print_endline ("Monitoring of "^string_rep_input (simp f));
	print_endline ("against "^string_rep_dtrace t);
	let n= nb_messages_sent f t alpha in
	print_endline ("   needs "^string_of_int n^" messages")

let test_count_message_1_randomTrace_1_randomFormula (alpha:d_alphabet) (size_form:int) (size_trace:int) (past:bool) =
	test_count_message (gen_1_form size_form (globalAlphabet alpha) past) (gen_1_dtrace size_trace alpha) alpha

let test_dprogress_1_event_1_formula_in_a_dTrace (form:ltl_formula) (alpha:d_alphabet) (t:d_trace) (eventNum:int) (compNum:int)=
	if (eventNum >= List.length t) || (compNum >= List.length alpha) then raise Error;
	let event = List.nth (List.nth t eventNum) compNum in
		print_endline ("Considering formula = "^string_rep_input form);
		print_endline ("Considering the trace = "^(string_rep_dtrace t));
		print_endline ("Over the alphabet "^(string_rep_dalphabet alpha));
		print_endline ("Considering the d_event "^string_rep_devent (List.nth t eventNum)^" and the event "^string_rep_event event);
		let result = simp (d_progress_form form event alpha t eventNum compNum) in
		print_endline ("-> the result is: "^string_rep_input result)


let test_dprogress_1_event_1_randomFormula_in_a_dTrace (alpha:d_alphabet) (t:d_trace) (eventNum:int) (compNum:int) (size_form:int) =
	let formula = simp (gen_1_form size_form (globalAlphabet alpha) false) in
		test_dprogress_1_event_1_formula_in_a_dTrace formula alpha t eventNum compNum


let test_dprogress_1_event_formulae_in_a_dTrace (form:ltl_formula array) (alpha:d_alphabet) (t:d_trace) (eventNum:int)=
	if (eventNum >= List.length t) then raise Error;
	let devent = List.nth t eventNum  in
		print_endline ("Considering formulae = "^string_rep_input_formulae form);
		print_endline ("Considering the trace = "^(string_rep_dtrace t));
		print_endline ("Over the alphabet "^(string_rep_dalphabet alpha));
		print_endline ("Considering the d_event "^string_rep_devent devent);
		let result = Array.map simp (d_progress form devent alpha t eventNum) in
		print_endline ("-> the result is: "^string_rep_input_formulae result)

let test_dprogress_1_event_randomFormulae_in_a_dTrace (alpha:d_alphabet) (t:d_trace) (eventNum:int) (size_form:int) =
	let formulae = Array.make (List.length alpha) (simp (gen_1_form size_form (globalAlphabet alpha) false)) in
		test_dprogress_1_event_formulae_in_a_dTrace formulae alpha t eventNum



let test_dprogress_1_dtrace_1_formula (form:ltl_formula) (alpha:d_alphabet) (t:d_trace) =
	print_endline ("Considering formula = "^string_rep_input form);
	print_endline ("Considering the trace = "^(string_rep_dtrace t));
	print_endline ("Over the alphabet "^(string_rep_dalphabet alpha));
	for i=0 to (List.length t)-1 do
		print_endline ("For d_event number "^string_of_int i^": "^string_rep_devent (List.nth t i));
		let edi = List.nth t i in
		for j=0 to (List.length edi)-1 do
			let pr = simp (d_progress_form form (List.nth edi j) alpha t i j) in
			print_endline ("\t for event number "^string_of_int j^": "^string_rep_event (List.nth edi j)^", the result is: "^string_rep_input pr);
		done
	done
	
	


let test_dprogress_1_randomTrace_1_randomFormula (alpha:d_alphabet) (size_form:int) (size_trace:int) =
	let formula = simp (gen_1_form size_form (globalAlphabet alpha) false) and dtrace=gen_1_dtrace size_trace alpha in
		test_dprogress_1_dtrace_1_formula formula alpha dtrace
		


let test_urgency_1_form (form: ltl_formula) = 
	let ur = urgency (simp form) in
		print_endline("Urgency of "^string_rep_input (simp form));
		print_endline("is "^string_of_int ur)
		
let test_urgency_1_randomForm (size:int) (alpha:alphabet)  = 
	let form = gen_1_form size alpha true in test_urgency_1_form form
	
let test_mou_1_form (form: ltl_formula) = 
	print_endline("Testing the urgency of the formula "^string_rep_input form);
	try
	let mou = most_urgent_obligation (form) in
		print_endline("\t -> the most urgent obligation is: "^string_rep_input mou);
	with Not_Urgent_Formula(formula) -> 
		print_endline("\t -> not urgent")
	
let test_mou_1_randomForm (size:int) (alpha:alphabet)=
	let form = simp (gen_1_form size alpha true) in test_mou_1_form form
	
let test_compute_obligations_simp (formulae:ltl_formula array) (alpha:d_alphabet) =
	print_endline ("Considering the dalphabet :"^string_rep_dalphabet alpha);
	print_endline (" and the formulae "^string_rep_input_formulae formulae);
	let obligations = compute_obligations_simp formulae alpha in
		print_endline("\t the obligations are: "^string_rep_input_formulae obligations)
	
	
let test_compute_obligations_simp_randomFormulae (size_array:int) (size_form:int) (alpha:d_alphabet) =
	let array = generate_array_formulae_same size_array size_form (globalAlphabet alpha) true in
		test_compute_obligations_simp array alpha
	
let test_d_monitor_1_form_1_trace (formula:ltl_formula) (alpha:d_alphabet) (t:d_trace) =
	let size = List.length alpha in
		let array_of_form = Array.make size formula and init_oblig = Array.make size True in
		d_monitor array_of_form alpha t 0 init_oblig t [] 0 0

	
let test_d_monitor_1_form_1_trace_old (formula:ltl_formula) (alpha:d_alphabet) (t:d_trace) =
	let size = List.length alpha in
		let array_of_form = Array.make size formula and init_oblig = Array.make size True in
		d_monitor_old array_of_form alpha t 0 init_oblig t [] 0 0
	

let test_d_monitor_1_form_1_trace_verbose (formula:ltl_formula) (alpha:d_alphabet) (t:d_trace) =
	let size = List.length alpha in
		let array_of_form = Array.make size formula and init_oblig = Array.make size True in
		print_endline ("Monitoring of "^string_rep_input formula);
		print_endline ("Against the trace "^string_rep_dtrace t);
		let (result,trace_needed,nmess,smess) = d_monitor_display_obligs array_of_form alpha t 0 init_oblig t [] 0 0in
			print_endline ("\t Result -> "^(string_rep_input result));
			print_endline ("\t After -> "^string_rep_dtrace trace_needed);
			print_endline ("\t With -> "^string_of_int nmess^" messages");
			print_endline ("\t  of total size "^string_of_int smess)


let test_d_monitor_1_randomTrace_1_randomFormula (alpha:d_alphabet) (size_form:int) (size_trace:int) =
 let trace = gen_1_dtrace size_trace alpha and formula = simp (gen_1_form_nontrivial size_form (globalAlphabet alpha) false) in
	test_d_monitor_1_form_1_trace formula alpha trace

type test_result = ltl_formula * d_trace * ltl_formula * ltl_formula


let rec how_many_props_differ (e1:event) (e2:event) (alpha:alphabet):int =
	match alpha with
		[] -> 0
		| p::tail -> (if ((List.mem p e1 && List.mem p e2) || not (List.mem p e1 || List.mem p e2)) then 0 else 1) + how_many_props_differ e1 e2 tail
	
(*
let number_ofupdown_fronts (t:d_trace) (dalpha:d_alphabet) = 
	let rec number_of_updwon_fronts_rec (t:d_trace) (dalpha:d_alphabet) (pos:int) =
		if (pos=0) then List.length dalpha + number_of_updwon_fronts_rec t dalpha (pos+1)
		else
			if (pos < length t) then
				let pdevent = List.nth t (pos-1) and devent = List.nth t pos in
					let fronts_current_event = List.fold_left (fun x y -> x + (if y>0 then 1 else 0)) 0 (map3 how_many_props_differ pdevent devent dalpha) in
						fronts_current_event + number_of_updwon_fronts_rec t dalpha (pos+1)
			else
				0
	in
		number_of_updwon_fronts_rec t dalpha 0
*)

let number_ofupdown_fronts (t:d_trace) (dalpha:d_alphabet) = 
	let rec number_of_updwon_fronts_rec (t:d_trace) (dalpha:d_alphabet)  (acc:int) =
		match t with
			[] -> acc
			| elt::[] -> acc
			| e1::e2::remainder -> 
					let fronts_current_event = List.fold_left (fun x y -> x + (if y>0 then 1 else 0)) 0 (map3 how_many_props_differ e1 e2 dalpha) in
						number_of_updwon_fronts_rec (e2::remainder) dalpha (fronts_current_event+acc)
	in number_of_updwon_fronts_rec t dalpha (List.length dalpha)



type message_passing = SEND_EVERYTHING | SEND_CHANGES

let mode  = ref SEND_EVERYTHING 

let number_of_messages_centralized_case (traceNeededCent:trace) (traceNeededDecent:d_trace) (dalpha:d_alphabet) =
	(**
	print_endline("**********************************");
	print_endline("**********************************");
	print_endline ("Trace needed_cent: "^(string_rep_trace traceNeededCent));
	print_endline ("Trace needed_decent: "^(string_rep_dtrace traceNeededDecent));
	
	*)
	match !mode with
		| SEND_EVERYTHING -> (List.length traceNeededCent) * (List.length dalpha)
		| SEND_CHANGES -> 
			let the_prefix = (prefix ((length traceNeededCent)) traceNeededDecent) in 
				let tmp = number_ofupdown_fronts the_prefix dalpha in
					(*
					print_endline (if the_prefix = [] then "empty" else (string_rep_dtrace the_prefix));
					print_int tmp; 
					*)
					tmp (* a bit tricky here. We want the prefix of the d_trace of length of the (c_)trace; because the function number_of_updown_fronts uses the d_trace directly and we want to know what would be the number of messages if there was a central observation point that collects the observation coming from the d_tracec *)
		

(*
Compares centralized and decentralized monitoring in terms of trace needed to get a verdict and number of messages exchanged
*)
let compare_results (formula:ltl_formula) (alpha:d_alphabet) (trace:d_trace) =
	let size = List.length alpha in (
		let global_trace = globalTrace trace and array_of_form = Array.make size formula
		and init_oblig = Array.make size True in
			let (verdict_centralized,traceTmp) = monitor formula global_trace
			and (verdict_decent,traceTmpDecent,num_mess_decent,size_mess_decent) = d_monitor array_of_form alpha trace 0 init_oblig trace [] 0 0 in (
			let num_mess_cent = number_of_messages_centralized_case traceTmp traceTmpDecent alpha
				in let size_mess_cent = num_mess_cent in (*the total number of exchanged messages is the size of the d_alphabet * the length of the trace*)
					if (List.length traceTmp > List.length traceTmpDecent) then (
						print_endline ("Error for formula :"^string_rep_input formula);
						print_endline ("initial dtrace : "^string_rep_dtrace trace);
						print_endline ("trace : "^string_rep_trace traceTmp);
						print_endline ("dtrace : "^string_rep_dtrace traceTmpDecent);
						);
					(formula, trace, verdict_centralized, verdict_decent, traceTmp, traceTmpDecent, num_mess_cent,num_mess_decent,size_mess_cent,size_mess_decent)
				)
			)


let compare_results_save (formula:ltl_formula) (alpha:d_alphabet) (trace:d_trace) =
	let size = List.length alpha in (
		let global_trace = globalTrace trace and array_of_form = Array.make size formula
		and init_oblig = Array.make size True in
			let (verdict_centralized,traceTmp) = monitor formula global_trace
			and (verdict_decent,traceTmpDecent,num_mess_decent,size_mess_decent) = d_monitor array_of_form alpha trace 0 init_oblig trace [] 0 0 in (
			let num_mess_cent = (List.length traceTmp) * (List.length alpha)
				in let size_mess_cent = num_mess_cent in (*the total size of the messages exchanged is the size of the d_alphabet* the length of the trace*)
					if (List.length traceTmp > List.length traceTmpDecent) then (
						print_endline ("Error for formula :"^string_rep_input formula);
						print_endline ("initial dtrace : "^string_rep_dtrace trace);
						print_endline ("trace : "^string_rep_trace traceTmp);
						print_endline ("dtrace : "^string_rep_dtrace traceTmpDecent);
						);
					(formula, trace, verdict_centralized, verdict_decent, traceTmp, traceTmpDecent, num_mess_cent,num_mess_decent,size_mess_cent,size_mess_decent)
				)
			) 




(*
Compares centralized and decentralized monitoring in terms of:
- trace needed to get a verdict
- number of messages exchanged
- size of obligations

let compare_results_trace_mess_sizeoblig (formula:ltl_formula) (alpha:d_alphabet) (trace:d_trace) =
	let size = List.length alpha in (
		let global_trace = globalTrace trace and array_of_form = Array.make size formula
		and init_oblig = Array.make size True in
			let (verdict_centralized,traceTmp) = monitor formula global_trace []
			and (verdict_decent,traceTmpDecent,num_mess_decent) = d_monitor array_of_form alpha trace 0 init_oblig trace [] 0 in (
			let num_mess_cent = (List.length traceTmp)* (List.length alpha) in
					if (List.length traceTmp > List.length traceTmpDecent) then (
						print_endline ("Error for formula :"^string_rep_input formula);
						print_endline ("initial dtrace : "^string_rep_dtrace trace);
						print_endline ("trace : "^string_rep_trace traceTmp);
						print_endline ("dtrace : "^string_rep_dtrace traceTmpDecent);
						);
					(formula, trace, verdict_centralized, verdict_decent, traceTmp, traceTmpDecent, num_mess_cent,num_mess_decent)
				)
			)
*)

let generate_compared_results_verbose (alpha:d_alphabet) (size_form:int) (size_trace:int)  =
	let file_ERROR = "ERROR" in
	let file = open_out_gen [Open_append] 644 file_ERROR in
	let trace = gen_1_dtrace size_trace alpha
	and formula = simp (gen_1_form_nontrivial size_form (globalAlphabet alpha) false) in
		let (formula, trace, verdict_cent,verdict_decent,traceNeededCent,traceNeededDeCent,ncent,ndecent,scent,sdecent) = compare_results formula alpha trace in
			print_endline ("Monitoring of "^string_rep_input formula);
			print_endline ("on the alphabet "^string_rep_dalphabet alpha);
			print_endline ("Against:");
			print_endline("\t the Dtrace "^string_rep_dtrace trace);
			print_endline("\t the trace "^string_rep_trace (globalTrace trace));
			print_endline("\t Delay-decentralized: "^string_of_int (ndecent-ncent));
			if (simp verdict_cent) <> (simp verdict_decent) then (
				print_endline("ERROR:");
				print_endline("\t verdict centralized: "^string_rep_input verdict_cent);
				print_endline("\t verdict decentralized: "^string_rep_input verdict_decent);
				output_string file ("\n##\n#######################\n##\n");
				output_string file ("Formula: "^string_rep_input formula^"\n");
				output_string file ("Trace: "^string_rep_dtrace trace^"\n");
				output_string file ("\t verdict centralized: "^string_rep_input verdict_cent^"\n");
				output_string file ("\t verdict decentralized: "^string_rep_input verdict_decent^"\n");
				output_string file ("\n#### DEBUG INFO ####\n\n");
				output_string file ("let formula= Ltl_parser.parse_formula_string \""^string_rep_input formula^"\";;\n");
				output_string file ("let size = List.length alphabet ;;\n");
				output_string file ("let formulae = Array.make size formula ;;\n");
				output_string file ("let init_oblig = Array.make size True ;;\n");
				output_string file ("let dtrace= Trace_parser.parse_dtrace_string \""^string_rep_dtrace trace^"\";;\n");
				output_string file ("let trace= Types.globalTrace dtrace ;;\n");
				output_string file ("Test.test_monitor_1_trace_1_formula formula trace ;;\n");
				output_string file ("Test.test_d_monitor_1_form_1_trace formula dalphabet dtrace ;;\n");
				output_string file ("Test.test_dprogress_1_event_formulae_in_a_dTrace formulae dalphabet dtrace 0;;\n");

				output_string file ("\n##\n#######################\n##\n");
			)
			else (
				print_endline("OK: both produced verdict "^string_rep_input verdict_cent);
			);
			close_out file


let generate_compared_results_file (alpha:d_alphabet) (size_form:int) (size_trace:int)  =
	let file_LOG= "LOG" in
	let file = open_out_gen [Open_append] 644 file_LOG in
	let trace = gen_1_dtrace size_trace alpha in
	let start_time = Sys.time() and formula = simp (gen_1_form_nontrivial size_form (globalAlphabet alpha) false) in
		let (formula, trace, verdict_cent,verdict_decent,traceNeededCent,traceNeededDeCent,ncent,ndecent,scent,sdecent) = compare_results formula alpha trace in
			let diff = Sys.time() -. start_time in
				output_string file ("\n##\n#######################\n##\n");
				output_string file ("Formula: "^string_rep_input formula^"\n");
				output_string file ("Trace: "^string_rep_dtrace trace^"\n");
				output_string file ("\t verdict centralized: "^string_rep_input verdict_cent^"\n");
				output_string file ("\t verdict decentralized: "^string_rep_input verdict_decent^"\n");
				output_string file ("Trace needed centralized: "^string_rep_trace traceNeededCent^"\n");
				output_string file ("Trace needed de-centralized: "^string_rep_dtrace traceNeededDeCent^"\n");									output_string file ("Num of mess needed centralized: "^string_of_int ncent^"\n");
				output_string file ("Num of mess needed decentralized: "^string_of_int ndecent^"\n");
	    		output_string file (string_of_float diff);
				output_string file ("\n##\n#######################\n##\n");
			close_out file


(*
let generate_compared_results_silent (alpha:d_alphabet) (size_form:int) (size_trace:int)  =
	let trace = gen_1_dtrace size_trace alpha
	and formula = simp (gen_1_form_nontrivial size_form (globalAlphabet alpha) false) in
	compare_results formula alpha trace
	
*)	

let generate_compared_results_efficient (alpha:d_alphabet) (size_form:int) (size_trace:int)  =
	let trace = gen_1_dtrace size_trace alpha
		and formula = simp (gen_1_form_nontrivial size_form (globalAlphabet alpha) false) in
			let (formula, trace, verdict_cent,verdict_decent,traceNeededCent,traceNeededDeCent,ncent,ndecent,smesscent,smessdecent) = 
				compare_results formula alpha trace in
					(List.length traceNeededCent, List.length traceNeededDeCent,ncent,ndecent,smesscent,smessdecent)
	
	

let generate_compared_results_efficient_trace_message_formsize (alpha:d_alphabet) (size_form:int) (size_trace:int)  =
	let trace = gen_1_dtrace size_trace alpha
		and formula = simp (gen_1_form_nontrivial size_form (globalAlphabet alpha) false) in
			let (formula, trace, verdict_cent,verdict_decent,traceNeededCent,traceNeededDeCent,ncent,ndecent,scent,sdecent) = 
				compare_results formula alpha trace in
					(List.length traceNeededCent, List.length traceNeededDeCent,ncent,ndecent)

	
let generate_compared_results_efficient_patterns (alpha:d_alphabet) (kind:string) (size_trace:int)  =
	let trace = gen_1_dtrace size_trace alpha
	and formula =
		if (kind="absence") then simp (gen_1_form_abscence (globalAlphabet alpha))
		else if (kind="exist") then simp (gen_1_form_existence (globalAlphabet alpha))
		else if (kind="unive") then simp (gen_1_form_universality (globalAlphabet alpha))
		else if (kind="bexist") then simp (gen_1_form_boundedexistence (globalAlphabet alpha))
		else if (kind="prec") then simp (gen_1_form_precedence (globalAlphabet alpha))
		else if (kind="response") then simp (gen_1_form_response (globalAlphabet alpha))
		else if (kind="prec_chain") then simp (gen_1_form_precedence_chain (globalAlphabet alpha))
		else if (kind="resp_chain") then simp (gen_1_form_response_chain (globalAlphabet alpha))
		else if (kind="resp_chain") then simp (gen_1_form_response_chain (globalAlphabet alpha))
		else if (kind="consc_chain") then simp (gen_1_form_constrained_chain (globalAlphabet alpha))

		else simp (gen_1_form_abscence (globalAlphabet alpha)) in
				(* print_endline(string_rep_input(formula)); *)

	let (formula, trace, verdict_cent,verdict_decent,traceNeededCent,traceNeededDeCent,ncent,ndecent,scent,sdecent) = compare_results formula alpha trace in
	(formula,List.length traceNeededCent, List.length traceNeededDeCent,ncent,ndecent,scent,sdecent)
	
	
	
	
	
