#loading the df
library(tidyverse)
library(tidyr)
library(fpp3)
library(ggplot2)
library(zoo)
library(timeDate)
library(data.table)
library(GGally)

load('Bikesharing_dataset.Rdata')

changins <- series[['Nyon, Changins']]
debarcadere <- series[['Nyon, Débarcadère']]
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
total.ts <- as_tsibble(total, index = date, key = name, regular = TRUE)
total.ts %>% autoplot(Bike) + facet_wrap(vars(name))

#we can see long periods of the same number of bikes. To deal with NAs, we'll just replace the NAs with the last value
total.ts <- total.ts %>% fill_gaps(.full = TRUE)
total.ts
total.ts <- na.locf(total.ts)
total.ts %>% scan_gaps()

#let's add a sum of the two bikes
total.ts <- total.ts %>% mutate(TotalBikes = Bike + `E-Bike`)

#plotting e-bikes and bikes together
total.ts %>%
  group_by() %>%
  summarize(bikecount = sum(Bike), ebikecount = sum(`E-Bike`)) %>% 
  gather("id", "value", 2:3) %>% 
  ggplot(., aes(date, value)) +
  geom_line() +
  facet_wrap(~id)

#Let's see if there's a correlation between e-bike count and bike-count:
total.ts %>% ggplot() + geom_point(aes(Bike, `E-Bike`)) + facet_wrap(vars(name))
with(total.ts, cor(Bike, `E-Bike`))

#there's a lot of data at 10-minute intervals, let's join it together to make an hourly dataset
hourly.ts <- total.ts %>% 
  group_by_key() %>%
  index_by(Date_Time = ~ lubridate::floor_date(., "1 hour")) %>%
  summarise(Bike = sum(Bike), `E-Bike` = sum(`E-Bike`), TotalBikes = sum(TotalBikes))

hourly.ts %>% autoplot() + facet_wrap(vars(name))

#let's try summarizing bikes by day:
daily.ts <- total.ts %>%
  group_by_key() %>%
  index_by(Date = date(date)) %>%
  summarise(
    total = sum(Bike)
  )


daily.ts %>% filter(name == 'Nyon, Gare Nord') %>% gg_lag(total, geom = 'point')
daily.ts %>% ACF(total) %>% autoplot() + facet_wrap(vars(name))
daily.ts %>% autoplot() + facet_wrap(vars(name))

total.ts$weekend = ifelse(isWeekday(total.ts$date), 0, 1)
holidays <- seq(as.Date("2022-04-15"), as.Date("2022-05-01"), by = "1 day")
holidays
total.ts$holiday <- ifelse(total.ts$date %in% holidays, 1, 0)

table(total.ts$holiday)
total.ts


#loading weather data
weather <- read.csv("Nyon.csv")

weather <- weather %>% select(-c('maxtempC', 'mintempC', 'sunHour', 'moon_illumination', 'moonrise', 'moonset', 'sunrise', 'sunset',
                     'DewPointC', 'pressure', 'FeelsLikeC', 'winddirDegree', 'location'))

total.ts$date_time <-  format(total.ts$date, format = '%F %H:00:00')

bikes <- merge(total.ts, weather, by = "date_time")
bikes.ts <- as_tsibble(bikes, index = date, key = name, regular = TRUE)

bikes.ts


#we don't need bikes, e-bikes, date_time to run the model
bikes.simplified <- bikes.ts %>% select(!c(Bike, `E-Bike`, date_time))

#now let's build a TSLM model
bikes.lm <- bikes.simplified %>% model(lm = TSLM(TotalBikes ~ weekend + holiday + totalSnow_cm + uvIndex + HeatIndexC + WindChillC +
                                       WindGustKmph + cloudcover + humidity + precipMM + tempC + visibility + windspeedKmph))

#checking model vs fitted values (for 1 place)
augment(bikes.lm) %>% filter(name == 'Nyon, Gare Sud') %>% 
  ggplot(aes(x = date)) +
  geom_line(aes(y = TotalBikes, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) + xlab("Year") + ylab(NULL) +
  guides(colour = guide_legend(title=NULL))


#relationship between actual and fitted values
augment(bikes.lm) %>% filter(name == 'Nyon, Gare Sud') %>% 
  ggplot(aes(x = TotalBikes, y = .fitted)) + geom_point() +
  ylab("Fitted (predicted values)") + xlab("Data (actual values)") +
  geom_abline(intercept = 0, slope = 1)

#checking residuals
bikes.lm %>% filter(name == 'Nyon, Gare Sud') %>% gg_tsresiduals()