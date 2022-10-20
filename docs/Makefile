index.html:
	asciidoctor -a toc rucksack.adoc -o index.html
rucksack-tutorial:
	erudite --output-type=markdown rucksack-tutorial.lisp > rucksack-tutorial.md
	pandoc -H rt-style.html --metadata pagetitle="Rucksack Tutorial" -s -o rucksack-tutorial.html rucksack-tutorial.md
