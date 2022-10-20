index.html:
	asciidoctor -a toc rucksack.adoc -o index.html
rucksack-tutorial:
	erudite rucksack-tutorial.lisp > rucksack-tutorial.tex
	latex2html -split +0 -info "" -no_navigation rucksack-tutorial.tex
