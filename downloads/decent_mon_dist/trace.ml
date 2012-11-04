open Types
open List
open Utils
open Random

type probability=float (* between 0 and 1 *)
type probability_sample = probability list

type probadistrib = | FLIPCOIN | BERNOUILLI | EXPO | BETA

let the_distrib = ref FLIPCOIN
let seed = ref (Random.float 0.)

let seeda = ref (Random.float 5.0)
let seedb = ref (Random.float 5.0)


(* Given an alphabet, it generates an event by selecting a symbol in the alphabet with probability 0.5
*)
let gen_event (alpha: alphabet) = 
 List.filter (fun x -> (Random.int 2)=0) alpha

(* A generic function that generates a trace using a generator for events (centralized or decentralized)*)
let rec gen_1_trace_generique (size:int) generator alpha =
	if (size=0)
		then
			[generator alpha]
		else
			(generator alpha)::(gen_1_trace_generique (size-1) generator alpha)

(* Generates a trace of a given size using a given alphabet *)
let gen_1_trace (size:int) (alpha:alphabet) = gen_1_trace_generique size gen_event alpha

(* Generates a decentralized event using a decentralized alphabet *)
let gen_devent (alpha:d_alphabet) = map gen_event alpha



(* A function that generates an event over an alphabet using a given probability *)
let gen_event_with_proba (alpha:alphabet) (p:probability) =
	let tmp = abs ((int_of_float (1./.p))) in
	let n = if tmp <>0 then min tmp 100000 else 1 in 
 		List.filter (fun x -> (Random.int n)=0) alpha


(* A generic function that generates a trace using a generator for events (centralized or decentralized)*)
let rec gen_1_trace_generique_with_proba (size:int) generator alpha (ps:probability_sample) (acc:'a list)=
	let (head,remainder)= match ps with [] -> (0.5, []) | h::r -> (h,r) in
	if (size=0)
		then
			(generator alpha head)::acc
		else
			gen_1_trace_generique_with_proba (size-1) generator alpha remainder ((generator alpha head)::acc)

(* Generates a trace of a given size using a given alphabet *)
let gen_1_trace_with_proba (size:int) (alpha:alphabet) (ps:probability_sample) = gen_1_trace_generique_with_proba size gen_event_with_proba alpha ps []

(* Generates a decentralized event using a decentralized alphabet *)
let gen_devent_with_proba (alpha:d_alphabet) (p:probability) = map (fun x -> gen_event_with_proba x p) alpha 
 
(* Generates a decentralized trace of a given size using a decentralized alphabet *)

let gen_1_dtrace_with_proba (size:int) (alpha:d_alphabet) (ps:probability_sample) = gen_1_trace_generique_with_proba size gen_devent_with_proba alpha ps []


let flipcoin_distrib (i:int):float = 0.5
let bernouilli_distrib (i:int) (p:float):float = p 
let exponential_distrib (i:int) (lambda:float) = let e = 878. /. 323. in min 1.0 (lambda *. (e** ((-.lambda) *. float_of_int(i))))

let beta_distrib (i:int) (total_size:int) (alpha:float) (beta:float):float=
	let pi = 4.*. atan 1. and e = 878. /. 323. in (* Two well-known and reasonable approximations of these mathematical constants *)
	let gamma (x:float) = sqrt ( 2.*.pi *. x ) *. (x/.e)**x  
	and x = (float_of_int i) /. (float_of_int total_size) in
		let galpha = gamma alpha and gbeta = gamma beta in
			((galpha+.gbeta) /. (galpha*.gbeta)) *. x **(alpha-.1.) *. (1. -. x)** (beta-.1.)    	

let gen_1_trace_flipcoin (size:int) (alpha:alphabet) = gen_1_trace_with_proba size alpha (make_list size flipcoin_distrib)

let gen_1_trace_bernouilli (size:int) (alpha:alphabet) =
	let seed = (Random.float 1.0) in gen_1_trace_with_proba size alpha (make_list size (fun x -> bernouilli_distrib x seed))

let gen_1_trace_exponential (size:int) (alpha:alphabet) =
	let seed = (Random.float 1.0) in gen_1_trace_with_proba size alpha (make_list size (fun x -> exponential_distrib x seed))

let gen_1_dtrace_flipcoin (size:int) (alpha:d_alphabet) = gen_1_dtrace_with_proba size alpha (make_list size flipcoin_distrib)

let gen_1_dtrace_bernouilli (size:int) (alpha:d_alphabet) =
	gen_1_dtrace_with_proba size alpha (make_list size (fun x -> bernouilli_distrib x !seed))

let gen_1_dtrace_exponential (size:int) (alpha:d_alphabet) =
	gen_1_dtrace_with_proba size alpha (make_list size (fun x -> exponential_distrib x !seed))

let gen_1_dtrace_beta (size:int) (alpha:d_alphabet) =
	let a = !seeda and b = !seedb in
		gen_1_dtrace_with_proba size alpha (make_list size (fun x -> beta_distrib x size a b))

(* Generates a decentralized trace of a given size using a decentralized alphabet *)
let gen_1_dtrace (size:int) (alpha:d_alphabet) = 
	match !the_distrib with
	| FLIPCOIN -> gen_1_dtrace_flipcoin size alpha
	| BERNOUILLI -> gen_1_dtrace_bernouilli size alpha
	| EXPO -> gen_1_dtrace_exponential size alpha
	| BETA -> gen_1_dtrace_beta size alpha


