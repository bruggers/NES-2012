PDFREADER = evince \

SOURCES = utils.ml \
	types.ml \
	enum.mli \
	enum.ml \
	extString.mli \
	extString.ml \
	IO.mli \
	IO.ml \
	std.mli \
	std.ml \
	ltl.ml \
	ltl_parser.ml \
	ltl_generator.ml \
	progression.ml \
	trace.ml \
	d_progression.ml \
	trace_parser.ml \
	alphabet_parser.ml \

CMO = utils.cmo \
	types.cmo \
	enum.cmo \
	extString.cmo \
	IO.cmo \
	std.cmo	\
	ltl.cmo	\
	ltl_parser.cmo \
	ltl_generator.cmo \
	progression.cmo	\
	trace.cmo \
	d_progression.cmo \
	trace_parser.cmo \
	alphabet_parser.cmo \
	common_test.cmo \

MLSOURCES = utils.ml \
	types.ml \
	ltl.ml \
	ltl_parser.ml \
	ltl_generator.ml \
	progression.ml \
	trace.ml \
	d_progression.ml \
	trace_parser.ml \
	alphabet_parser.ml \

all: decentmon test1form toplevel

test: *.ml
	ocamlc -I +camlp4 -pp camlp4of.opt $(SOURCES) common_test.ml test.ml -o decentmon

decentmon:
	ocamlopt -I +camlp4 -pp camlp4of.opt $(SOURCES) common_test.ml headers.ml test.ml -o decentmon
	chmod u+x decentmon
	
decentmon_nonat:
	ocamlc -I +camlp4 -pp camlp4of.opt $(SOURCES) common_test.ml headers.ml test.ml -o decentmon

test1form: *.ml
	ocamlc -I +camlp4 -pp camlp4of.opt $(SOURCES) common_test.ml test1formula.ml -o test1form

debug:
	ocamlc -I +camlp4 -pp camlp4of.opt $(SOURCES) -g test.ml -o decentmon

toplevel:
	ocamlmktop -o mytoplevel $(CMO)
	chmod 700 mytoplevel

prof:
	ocamlopt -I +camlp4 -pp camlp4of.opt $(SOURCES) common_test.ml headers.ml test.ml -o decentmon -p
	chmod u+x decentmon
	./decentmon -n 1000 -msf 6 -st 1000 -dalpha "{a|b|c}" -prt_full true -only_changes true;
	gprof decentmon
	
	
testperfs1:
	ocamlopt -I +camlp4 -pp camlp4of.opt $(SOURCES) common_test.ml perf_test_cmonitor.ml -o perftest1
	chmod u+x perftest1
	
testperfs2:
	ocamlopt -I +camlp4 -pp camlp4of.opt $(SOURCES) common_test.ml perf_test_dmonitor.ml -o perftest2
	chmod u+x perftest2

testperfs2prof:
	ocamlopt -I +camlp4 -pp camlp4of.opt $(SOURCES) common_test.ml perf_test_dmonitor.ml -o perftest2 -p
	chmod u+x perftest2
	./perftest2
	gprof perftest2

dist:
	mkdir Decent_Mon_dist
	cp $(SOURCES) common_test.ml headers.ml test.ml test1formula.ml Decent_Mon_dist/.
	cp Makefile Decent_Mon_dist/.

doc:
	ocamlweb $(MLSOURCES) > doc.tex
	latex doc
	dvipdf doc.dvi
	$(PDFREADER) doc.pdf &

clean:
	rm -rf *~ *.o *.cmi *.cmx *.cmo *.aux *.dvi *.log *.pdf;
	rm decentmon;
	rm mytoplevel;
	rm test1form;
	
