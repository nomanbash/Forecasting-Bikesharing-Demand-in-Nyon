rm(list=ls())
#loading the df
library(tidyverse)
load('Bikesharing_dataset.Rdata')

nyon <- series %>% filter(name == c('Nyon, Changins',
                                    'Nyon, Débarcadère',
                                    'Nyon, Gare Sud',
                                    'Nyon, Piscine du Cossy',
                                    'Nyon, Hôpital',
                                    'Nyon, Petit Perdtemps',
                                    'Nyon, Hostel',
                                    'Nyon, Place de Savoie',
                                    'Nyon, La Plage',
                                    'Nyon, Stade de Colovray',
                                    'Nyon, Triangle de l\'Etraz',
                                    'Nyon, Gare Nord',
                                    'Nyon, Château'))


changins <- series[['Nyon, Changins']]
debarcadere <- series[['Nyon, Debarcadere']]
garesud <- series[['Nyon, Gare Sud']]
piscine <- series[['Nyon, Piscine du Cossy']]
hopital <- series[['Nyon, Hôpital']]
perdtemps <- series[['Nyon, Petit Perdtemps']]
hostel <- series[['Nyon, Hostel']]
savoie <- series[['Nyon, Place de Savoie']]
plage <- series[['Nyon, La Plage']]
colovray <- series[['Nyon, Stade de Colovray']]
triangle <- series[['Nyon, Triangle de l\'Etraz']]
gare_nord <- series[['Nyon, Gare Nord']]
chateau <- series[['Nyon, Château']]

total <- rbind(changins, debarcadere, garesud, piscine, hopital, perdtemps, hostel, savoie, plage, colovray, triangle, gare_nord, chateau)

total$date <- as.POSIXct(total$date)

total.ts <- as_tsibble(total, index = date, key = name, regular = FALSE)

total.ts

total.ts %>% group_by() %>% summarize(Count = sum(Bike)) %>% autoplot(Count)
total.ts %>% gg_season()
count_gaps(total.ts)
?scan_gaps

total.ts    

changins.ts <- as_tsibble(changins, index = date, regular = FALSE)

total.ts %>% filter(name == 'Nyon, Gare Sud') %>%  ACF(Bike) %>% autoplot()
View(total.ts %>% filter(name == 'Nyon, Gare Sud'))
