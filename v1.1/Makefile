CC = ocamlopt
OPT = 
LIBS = -I +xml-light -I +cairo nums.cmxa xml-light.cmxa cairo.cmxa

all: masines.out

%.out: %.ml
	$(CC) $(OPTS) $(LIBS) $< -o $@

primes: masines.out
	./$< pdf examples/primes.xml > primes.pdf
#open primes.pdf

clean:
	rm -f *.out *.cmi *.cmx *.cmo *.o *.svg *.pdf *~ \#*\#
