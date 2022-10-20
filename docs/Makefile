all: design.html tutorial.html code

design.html:
	asciidoctor -a toc rucksack.adoc -o design.html

tutorial.html:
	erudite --output-type=markdown rucksack-tutorial.lisp > rucksack-tutorial.md
	pandoc -H rt-style.html --metadata pagetitle="Rucksack Tutorial" -s -o tutorial.html rucksack-tutorial.md

code:
	sbcl --load code.lisp --quit
