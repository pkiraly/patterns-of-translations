# Script for analysing the translations of László Kraszahorkai's works

## krasznahorkai.R

input: 
* data_raw/post1930-with-regions.rds
* krasznahorkai-magyarul.csv
* krasznahorkai-english-titles.csv
* krasznahorkai-normalized.csv

output:
* images/krasznahorkai7.png
* images/krasznahorkai10.en.png

## krasznahorkai-languages.R

input: 
* data_raw/post1930-with-regions.rds
* krasznahorkai-magyarul.csv
* krasznahorkai-english-titles.csv
* krasznahorkai-normalized.csv
* krasznahorkai-normalized-elso-kiadasok.csv

output:
* images/krasznahorkai-languages7.en.png

## krasznahorkai2-english.R

input: 
* data_raw/post1930-with-regions.rds
* krasznahorkai-magyarul.csv
* krasznahorkai-english-titles.csv
* krasznahorkai-normalized.csv
* krasznahorkai-normalized-elso-kiadasok.csv

output:
* images/krasznahorkai13.en.png