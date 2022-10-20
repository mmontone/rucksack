(require :qbook)
(require :rucksack)

(qbook:publish-system-qbook :rucksack 'qbook:html-generator :output-directory #p"./rucksack-code/" :title "Rucksack code")
