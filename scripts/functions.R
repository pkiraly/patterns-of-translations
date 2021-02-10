library(tidyverse)
library(ggplot2)
library("stringdist")

clean_year <- function(year) {
  year <- gsub(
    '^(o\\. ?[jJ]\\.?|s\\. ?a\\.?|b\\. ?r\\.?|Jahr unbek\\.|é\\. n\\.|s\\. ?d\\.|b\\. ?g\\.|Datum fehlt|Datum unbek\\.|Ohne Jahresang\\.|f\\. ?a\\.|w\\. y\\.|without year|bez\\. god\\.|sans date)$',
    'NA', year)
}

clean_city <- function(city) {
  print(dim(city))

  city <- gsub(', [aA]nno', '', city) 
  city <- gsub('[\\.,]$', '', city)
  city <- gsub('(\\(|\\)|\\[|\\])', '', city)
  city <- gsub('[\\.,]$', '', city)
  
  # if (city == 'Angouleme') {
  #   city <- 'Angoulème'
  # } else if (city == 'Gross-kanizsa' || city == 'Gross-Kanizsa') {
  #   city <- 'Nagykanizsa'
  # } else if (city == 'Budapeszt' || city == 'W Budapeszcie' ||city == 'Budapestini' ||
  #            city == 'Budapešť' || city == 'Budapeszcie'|| city == 'Budae' || city == 'Pesth' ||
  #            city == 'Pesten' || city == 'Budapesta' || city == 'Budapet' || city == 'Budapesst' ||
  #            city == 'Budapesť' || city == 'Budapešt' || city == 'Будапешт' || 
  #            city == 'Budapset' || city == 'Budimpešta') {
  #   city <- 'Budapest'
  # } else if (city == 'Harlem' || city == 'haarlem') {
  #   city <- 'Haarlem'
  # } else if (city == 'Konstanz am Bodensee' || city == 'Konstanz, a. Bodensee' || 
  #            city == 'Konstanz, am Bodensee') {
  #   city <- 'Konstanz'
  # } else if (city == 'Liptsymikuláš' || city == 'Lipt. Sv. Mikuláš' ||
  #            city =='Liptovsky Sv. Mikuláš') {
  #   city <- 'Liptovský Sv. Mikuláš'
  # } else if (city == 'Lodz' || city == 'Lódz' || city == 'Łodz' || city == 'Łódz') {
  #   city <- 'Łódź'
  # } else if (city == 'Losonc' || city == 'Losoncon' || city == 'Losoncz' || city == 'Losonczon') {
  #   city <- 'Lučenec'
  # } else if (city == 'Lwow' || city == 'Lwów') {
  #   city <- 'Lviv'
  # } else if (city == 'Minden i/Westfalien' || city == 'Minden in Westfailen' ||
  #            city == 'Minden, in Westfalien') {
  #   city <- 'Minden'
  # } else if (city == 'Kairó') {
  #   city <- 'Kairo'
  # } else if (city == 'Kassa') {
  #   city <- 'Košice'
  # } else if (city == 'Krakow') {
  #   city <- 'Kraków'
  # } else if (city == 'Torčiansky Sv. Martin' || city == 'Turčiansky Sv. Martin' || city == 'Turčiánsky Sv. Martin' || city == 'Sv. Martin' || city == 'Turoczszentmárton') {
  #   city <- 'Martin'
  # } else if (city == 'Temesvár') {
  #   city <- 'Timișoara'
  # } else if (city == 'Postdam' || city == 'Potsdamm') {
  #   city <- 'Potsdam'
  # } else if (city == 'Miskolcz') {
  #   city <- 'Miskolc'
  # } else if (city == 'Munich') {
  #   city <- 'München'
  # } else if (city == 'Murska−Sobota') {
  #   city <- 'Murska Sobota'
  # } else if (city == 'hermannstadt' || city == 'Hermannstadt' || city == 'Hermanstadt' || 
  #            city == 'Nagyszeben' || city == 'Sibiiu') {
  #   city <- 'Sibiu'
  # } else if (city == 'Páris') {
  #   city <- 'Paris'
  # } else if (city == 'Róma') {
  #   city <- 'Roma'
  # } else if (city == 'Tel-Áviv') {
  #   city <- 'Tel-Aviv'
  # } else if (city == 'V Trnava' || city == 'V Trnave') {
  #   city <- 'Trnava'
  # } else if (city == 'Warszava') {
  #   city <- 'Warszawa'
  # } else if (city == 'Winipeg') {
  #   city <- 'Winnipeg'
  # } else if (city == 'Raab') {
  #   city <- 'Győr'
  # } else if (city == 'Helsingfors' || city == 'Helsingissä') {
  #   city <- 'Helsinki'
  # } else if (city == 'Halle a. S' || city == 'Halle, a. S' || city == 'Halle, a/S' || 
  #            city == 'Halle, am Saale' || city == 'Halle, am. Saale') {
  #   city <- 'Halle am Saale'
  # } else if (city == 'Breslau' || city == 'Breslau, Breslau' || city == 'Bresslau' || 
  #            city == 'Brslau') {
  #   city <- 'Wrocław'
  # } else if (city == 'Darmstad' || city == 'Darmstadtt') {
  #   city <- 'Darmstadt'
  # } else if (city == 'Buenos-Aires') {
  #   city <- 'Buenos Aires'
  # } else if (city == 'Bydgeszcz') {
  #   city <- 'Bydgoszcz'
  # } else if (city == 'Brünn') {
  #   city <- 'Brno'
  # } else if (city == 'Brussel') {
  #   city <- 'Bruxelles'
  # } else if (city == 'Fiuma') {
  #   city <- 'Rijeka'
  # } else if (city == 'Genève') {
  #   city <- 'Genf'
  # } else if (city == 'Firenza') {
  #   city <- 'Firenze'
  # } else if (city == '\'s Gravenhage' || city == '\'S Gravenhage' || city == '\'s Gravenhagen' || 
  #            city == '\'S Gravenhagen' || city == 's-Gravenhage' || city == 'Den Haagn') {
  #   city <- 'Den Haag'
  # } else if (city == 'B-Bystrica' || city == 'B. − Bystrica' || city == 'B. Bystrica' || 
  #            city == 'B.-Bystrica' || city == 'B.-Bytrica' || city == 'B.−Bystrica' ||
  #            city == 'Besztercebánya') {
  #   city <- 'Banská Bystrica'
  # } else if (city == 'Brasso') {
  #   city <- 'Braşov'
  # } else if (city == 'Elberfeld und Leipzig') {
  #   city <- 'Elberfeld−Leipzig'
  # } else if (city == 'Franckfurt am Main' || city == 'Franckfurt a/M' || 
  #            city == 'Franckfurt, a/M' || city == 'Franckfurt, am Main' ||
  #            city == 'Frankfurt, a. M' || city == 'Frankfurt, a. Main' ||
  #            city == 'Frankfurt, a.M' || city == 'Frankfurt, a/M' || city == 'Frankfurt, am Main') {
  #   city <- 'Frankfurt am Main'
  # } else if (city == 'Kolozsvár−Cluj' || city == 'Kolozsvár-Cluj' || city == 'Cluj' || 
  #            city == 'Clus' || city == 'Cluş' || city == 'Klausenburg' || city == 'Kolozsvár') {
  #   city <- 'Cluj-Napoca'
  # } else if (city == 'Pozsony Presbourg' || city == 'Pozsony, Presbourg' || city == 'Pozsony' || 
  #            city == 'Pozsona Presbourg' || city == 'Pozsony Prebourg' || 
  #            city == 'Pozsony Pressbourg' || city == 'Bartislava' || city == 'Pressburg') {
  #   city <- 'Bratislava'
  # } else if (city == 'New-York') {
  #   city <- 'New York'
  # } else if (city == 'Bukarest' || city == 'Bucarest' || city == 'Bucareşti' || 
  #            city == 'Bucuresti' || city == 'Bukurest') {
  #   city <- 'Bucureşti'
  # }

  return(city)
}  