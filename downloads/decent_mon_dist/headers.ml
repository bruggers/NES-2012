let print_full_stats = ref false
let print_delay_stats = ref false
let print_trace_and_mess_stats = ref false
let print_size_mess = ref false


let welcome () = 
  print_endline("######################################################");
  print_endline("############ D E C E N T   M O N I T O R ############ ");
  print_endline("######################################################\n")
 
 
let print_separator0 () =
print_endline("--------------------------------------------------||---------------------"^if (!print_size_mess) then "----------" else ""^"||---------------------"^if (!print_size_mess) then "----------" else ""^"||-----------------------"^if (!print_size_mess) then "----------" else ""^"||-------------")

let print_separator_pattern0 () =
print_endline("---------------------------------------------------------||---------------------"^if (!print_size_mess) then "-----------" else ""^"||---------------------"^if (!print_size_mess) then "----------" else ""^"||----------------------"^if (!print_size_mess) then "-----------" else ""^"||-------------")


let print_header0()=
if (!print_size_mess) then
	print_endline
	("                                                  ||   Centralized Case  ||  Decentralized Case ||         Ratio                ||    Delays   ")
else
	print_endline
	("                                                  ||   Centralized Case  ||  Decentralized Case ||         Ratio         ||    Delays   ")


 
let print_header() = (
if (!print_full_stats) then (
print_header0();
print_separator0();
print_endline(" |form| |      alphabet      |     d_alphabet     ||  |trace|  |  # msg  "^if (!print_size_mess) then "|  |msg|  " else ""^"||  |trace|  |  # msg  "^if (!print_size_mess) then "|  |msg|  " else ""^"||  |trace|  |  # msg    "^if (!print_size_mess) then "|  |msg|  " else ""^"||  Avg  | Max ");
print_separator0()
);
if (!print_delay_stats) then (
print_endline("                                                  ||     Delays  ");
print_endline("--------------------------------------------------||---------------");
print_endline(" |form| |       alphabet      |     d_alphabet    ||   Avg  | Max ");
print_endline("--------------------------------------------------||--------------")
);
if (!print_trace_and_mess_stats) then (
print_endline("                                                  ||  Centralized Case   || Decentralized Case  ||         Ratio        ");
print_endline("--------------------------------------------------||---------------------||---------------------||----------------------");
print_endline(" |form| |      alphabet      |     d_alphabet     ||  |trace|  |  # msg  ||  |trace|  |  # msg  ||  |trace|  |  # msg   ");
print_endline("--------------------------------------------------||---------------------||---------------------||----------------------")
);
)


let print_header_patterns() = (
if (!print_full_stats) then (
print_endline
("                                                         ||   Centralized Case  ||  Decentralized Case ||        Ratio         ||    Delays   ");
print_separator_pattern0 ();
print_endline
("  pattern  |       alphabet       |      d_alphabet      ||  |trace|  |  # msg  "^if (!print_size_mess) then "|  |msg|  " else ""^"||  |trace|  |  # msg  "^if (!print_size_mess) then "|  |msg|  " else ""^"||  |trace|  |   # msg  "^if (!print_size_mess) then "|  |msg|  " else ""^"||  Avg  |  Max ");
print_separator_pattern0 ()
);
if (!print_delay_stats) then (
print_endline("                                                         ||     Delays  ");
print_endline("---------------------------------------------------------||---------------");
print_endline("  patterns |       alphabet       |      d_alphabet      ||   Avg  | Max ");
print_endline("---------------------------------------------------------||---------------")
);
if (!print_trace_and_mess_stats) then (
print_endline("                                                         ||         Centralized Case      ||       Decentralized Case      ||              Ratio                ");
print_endline("---------------------------------------------------------||-------------------------------||-------------------------------||-----------------------------------");
print_endline("  pattern  |       alphabet       |      d_alphabet      ||  |trace|  |  # msg  |  |msg|  ||  |trace|  |  # msg  |  |msg|  ||  |trace|  |   # msg    |  |msg|   ");
print_endline("---------------------------------------------------------||-------------------------------||-------------------------------||-----------------------------------")
);
)

let print_header_patterns_alpha() = 
print_endline("                                                     ||         Centralized Case      ||       Decentralized Case      ||              Ratio                ");
print_endline("-----------------------------------------------------||-------------------------------||-------------------------------||----------------------------------");
print_endline("  pattern  |      alphabet      |    |d_alphabet|    ||  |trace|  |  # msg  |  |msg|  ||  |trace|  |  # msg  |  |msg|  ||  |trace|  |   # msg    |  |msg|   ");
print_endline("-----------------------------------------------------||-------------------------------||-------------------------------||----------------------------------")