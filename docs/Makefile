all: intro.html tutorial.html
intro.html:
	asciidoctor -a toc rucksack.adoc -o intro.html
tutorial.html:
	erudite --output-type=markdown rucksack-tutorial.lisp > rucksack-tutorial.md
	pandoc -H rt-style.html --metadata pagetitle="Rucksack Tutorial" -s -o tutorial.html rucksack-tutorial.md
