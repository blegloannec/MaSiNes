CC = ocamlopt
TGZ = MaSiNes-$(shell date +"%d.%m.%Y").tgz

all: masines.out masines2d.out masines_tikz.out

%.out: %.ml
	$(CC) nums.cmxa $< -o $@

adder1: masines.out examples_sm/adder1.sm
	./$< 10 6 300 500 40.0 40.0 5.5 < examples_sm/adder1.sm > adder1.svg

test4: masines.out examples_sm/test4.sm
	./$< 350 230 700 500 3.0 3.0 0.0 < examples_sm/test4.sm > test4.svg

koch_2: masines.out examples_sm/koch_2.sm
	./$< 30 0 400 800 100.0 100.0 4.1 < examples_sm/koch_2.sm > koch_2.svg

%.sm: examples_xml/%.xml
	python xml2sm.py $< > $@

zenon: masines.out zenon.sm
	./$< 30 0 500 500 800.0 400.0 0.1 < zenon.sm > zenon.svg

alea: masines.out
	python alea_xml.py 10 > alea.xml
	python xml2sm.py alea.xml > alea.sm
	./$< 50 100 600 600 8.0 8.0 30.0 < alea.sm > alea.svg
	display alea.svg
#	svg2pdf alea.svg alea.pdf
#	open alea.pdf

ex3d: masines2d.out
	python xml2sm2d.py examples_xml/ex3d.xml > ex3d.sm2d
	./$< 5 3 500 500 400.0 400.0 0.0 < ex3d.sm2d > ex3d.svg3d
	python proj_svg3d.py all ex3d.svg3d 0.75 0.25 0 0.35 -0.35 1 > ex3d.svg

zenon2d: masines2d.out
	python xml2sm2d.py examples_xml/zenon2d.xml > zenon2d.sm2d
	./$< 50 0 500 500 400.0 400.0 0.0 < zenon2d.sm2d > zenon2d.svg3d
	python proj_svg3d.py all zenon2d.svg3d 0.75 0.25 0 0.35 -0.35 1 > zenon2d_v0.svg
	python proj_svg3d.py all zenon2d.svg3d > zenon2d_v2d.svg
#	display zenon3d.svg

sierp2d: masines2d.out
	python xml2sm2d.py examples_xml/sierp2d.xml > sierp2d.sm2d
	./$< 17 0 500 500 400.0 400.0 0.0 < sierp2d.sm2d > sierp2d.svg3d
	python proj_svg3d.py all sierp2d.svg3d 0.6 0.4 0 0.35 -0.9 1 > sierp2d_v0.svg
	python proj_svg3d.py all sierp2d.svg3d > sierp2d_v2d.svg

sierpsvg:
	python proj_svg3d.py all svg_svg/sierp2d.svg3d 0.5 0.4 0 0.35 -0.9 1 > sierp2d_v0.svg
#	svg2pdf sierp2d_v0.svg sierp2d_v0.pdf
#	open sierp3d_v0.pdf

eca: masines.out
	python examples_xml/eca2xml.py 18 010 > eca.xml
	python xml2sm.py eca.xml > eca.sm
	./$< 250 0 550 650 8.0 5.0 65.0 < eca.sm > eca.svg
	display eca.svg
#	svg2pdf eca.svg eca.pdf
#	open eca.pdf

primes: masines.out
	python xml2sm.py examples_xml/primes-fisher.xml |  ./masines.out 500 0 2000 2000 20.0 20.0 0.5 > primes.svg
	svg2pdf primes.svg primes.pdf
	open primes.pdf

primes_odd: masines.out
	python xml2sm.py examples_xml/primes-fisher3.xml |  ./masines.out 200 0 1000 1000 20.0 20.0 0.5 > primes_odd.svg
	svg2pdf primes_odd.svg primes_odd.pdf
	open primes_odd.pdf

mult: masines.out
	python examples_xml/mult2xml.py 16 17 > examples_xml/mult.xml
	python xml2sm.py examples_xml/mult.xml |  ./masines.out 200 0 400 400 20.0 20.0 0.5 > mult.svg
	svg2pdf mult.svg mult.pdf
	open mult.pdf

tri: masines.out
	python xml2sm.py examples_xml/tri-pair-impair-adhoc.xml |  ./masines.out 50 0 600 300 20.0 20.0 1.0 > tri.svg
	svg2pdf tri.svg tri.pdf
	open tri.pdf

archive:
	tar czvf $(TGZ) examples_sm/*.sm examples_xml/*.xml examples_xml/eca2xml.py svg_svg/*.svg3d MaSiVi3d/*.py Makefile masines.ml masines2d.ml masines_tikz.ml README README-TIKZ xml2sm.py alea_xml.py xml2sm2d.py proj_svg3d.py xml2smt.py

clean:
	rm -f *.out *.cmi *.cmx *.cmo *.o *.svg MaSiNes-*.tgz alea.xml *.sm *.sm2d *.svg3d *.pdf *.xml *~
