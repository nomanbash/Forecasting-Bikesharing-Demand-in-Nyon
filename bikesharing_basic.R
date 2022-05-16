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

stations <- c("Nyon, Changins", "Nyon, Château","Nyon, Débarcadère",
              "Nyon, Gare Nord","Nyon, Gare Sud",
              "Nyon, Hôpital",
              "Nyon, Hostel",
              "Nyon, La Plage",
              "Nyon, Petit Perdtemps",
              "Nyon, Piscine du Cossy",
              "Nyon, Place de Savoie",
              "Nyon, Stade de Colovray",
              "Nyon, Triangle de l'Etraz")

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
total.ts %>% autoplot(Bike, show.legend = FALSE) + facet_wrap(vars(name)) + ggtitle("Bikes by Station")

#we can see long periods of the same number of bikes. To deal with NAs, we'll just replace the NAs with the last value
total.ts <- total.ts %>% fill_gaps(.full = TRUE)
total.ts <- na.locf(total.ts)
total.ts %>% scan_gaps()

#we don't need capacity
total.ts <- total.ts %>% select(!Capacity)

#let's add a sum of the two bikes
total.ts <- total.ts %>% mutate(TotalBikes = Bike + `E-Bike`)

total.ts %>% summarize(count = sum(TotalBikes)) %>% autoplot() + ggtitle("Bike Demand in Nyon", subtitle = "Combined Bikes and E-Bikes Demand") + xlab("Time")
total.ts %>% summarize(bikecount = sum(Bike), ebikecount = sum(`E-Bike`)) %>% ggplot(aes(x = date)) + geom_line(aes(y = bikecount), col = "red") + geom_line(aes(y = ebikecount), col = "blue")

#plotting e-bikes and bikes together
total.ts %>%
  group_by() %>%
  summarize(bikecount = sum(Bike), ebikecount = sum(`E-Bike`)) %>% 
  gather("id", "value", 2:3) %>% 
  ggplot(., aes(date, value)) +
  geom_line() +
  facet_wrap(~id)

#plotting correlation between Bike and E-BIke
total.ts %>%
  group_by() %>%
  summarize(bikecount = sum(Bike), ebikecount = sum(`E-Bike`)) %>%
  ggplot() + geom_point(aes(bikecount, ebikecount)) + geom_abline(aes(intercept = 0,slope = 1))

#there doesn't seem to be any great correlation between the two.

#Let's see if there's a correlation between e-bike count and bike-count based on location
total.ts %>% ggplot() + geom_point(aes(Bike, `E-Bike`)) + facet_wrap(vars(name))
with(total.ts, cor(Bike, `E-Bike`))

#there's a lot of data at 10-minute intervals, let's join it together to make an hourly dataset
hourly.ts <- total.ts %>% 
  group_by_key() %>%
  index_by(Date_Time = ~ lubridate::floor_date(., "1 hour")) %>%
  summarise(Bike = sum(Bike), `E-Bike` = sum(`E-Bike`), TotalBikes = sum(TotalBikes))

#plotting hourly by location
hourly.ts %>% autoplot(TotalBikes, show.legend = FALSE) + facet_wrap(vars(name))

#let's try summarizing bikes by day:
daily.ts <- total.ts %>%
  group_by_key() %>%
  index_by(Date = date(date)) %>%
  summarise(
    total = sum(Bike)
  )

#plotting daily by location
daily.ts %>% filter(name == 'Nyon, Gare Nord') %>% gg_lag(total, geom = 'point')
daily.ts %>% ACF(total) %>% autoplot() + facet_wrap(vars(name))
daily.ts %>% autoplot() + facet_wrap(vars(name))

total.ts$weekday = as.factor(ifelse(isWeekday(total.ts$date), "Weekday", "Weekend"))
table(total.ts$weekday)
holidays <- c(as.Date("2022-04-15"),as.Date("2022-04-17"),as.Date("2022-04-18"),as.Date("2022-04-25"), as.Date("2022-05-01"))
holidays
total.ts$holiday <- ifelse(as.Date(total.ts$date) %in% holidays, 1, 0)
table(total.ts$holiday)

#loading weather data
weather <- read.csv("Nyon.csv")

weather <- weather %>% select(-c('tempC', 'mintempC', 'sunHour', 'moon_illumination', 'moonrise', 'moonset', 'sunrise', 'sunset',
                     'DewPointC', 'pressure', 'FeelsLikeC', 'winddirDegree', 'location'))

total.ts$date_time <-  format(total.ts$date, format = '%F %H:00:00')

bikes <- merge(total.ts, weather, by = "date_time")
bikes.ts <- as_tsibble(bikes, index = date, key = name, regular = TRUE)

nightday <- function(datetime) {
  paste(
    c("Night", "Morning", "Morning Rush", "Morning", "Lunch", "Afternoon", "Afternoon Rush", "Evening", "Night")[
      cut(as.numeric(format(datetime, "%H%M")), c(0, 530, 0700, 0800, 1100, 1300, 1600, 1700 ,2000, 2359))
    ]
  )
}
 

bikes.ts$timeofday <- as.factor(nightday(bikes.ts$date))
bikes.ts$timeofday[bikes.ts$timeofday == "NA"] <- "Night"
table(bikes.ts$timeofday)
bikes.ts$timeofday <- bikes.ts$timeofday %>%  droplevels()
table(bikes.ts$timeofday)


#we don't need bikes, e-bikes, date_time to run the model
bikes.simplified <- bikes.ts %>% select(!c(Bike, `E-Bike`, date_time))

#let's check all vars
bikes.simplified %>% select(TotalBikes, weekday) %>% ggpairs()

#now let's build a TSLM model
bikes.lm <- bikes.simplified %>% model(TSLM(TotalBikes ~ weekday + totalSnow_cm + uvIndex + HeatIndexC + WindChillC +
                                       WindGustKmph + cloudcover + humidity + precipMM + maxtempC + visibility + windspeedKmph +
                                         timeofday + holiday))

bikes.lm %>% filter(name == 'Nyon, Gare Sud') %>% report()

#checking model vs fitted values (for 1 place)
augment(bikes.lm) %>% filter(name == 'Nyon, Gare Sud') %>% 
  ggplot(aes(x = date)) +
  geom_line(aes(y = TotalBikes, colour = "Data"), size = 1) +
  geom_line(aes(y = .fitted, colour = "Fitted"), size = 1) + xlab("Year") + ylab(NULL) +
  guides(colour = guide_legend(title=NULL))

#relationship between actual and fitted values
augment(bikes.lm) %>% filter(name == 'Nyon, Gare Sud') %>% 
  ggplot(aes(x = TotalBikes, y = .fitted)) + geom_point() +
  ylab("Fitted (predicted values)") + xlab("Data (actual values)") +
  geom_abline(intercept = 0, slope = 1)

#checking residuals
bikes.lm %>% filter(name == 'Nyon, Gare Sud') %>% gg_tsresiduals()

#for TSLM
bikes.hts <- bikes.simplified %>%
  aggregate_key(name, Total = sum(TotalBikes), weekday = weekday, totalSnow_cm = totalSnow_cm, uvIndex = uvIndex, HeatIndexC = HeatIndexC, WindChillC = WindChillC,
                  WindGustKmph = WindGustKmph, cloudcover = cloudcover, humidity = humidity,  precipMM = precipMM, maxtempC = maxtempC,  visibility = visibility, windspeedKmph = windspeedKmph,
                  timeofday = timeofday, holiday = holiday)

bikes.hts <- bikes.hts[!duplicated(bikes.hts$date),]

bikes.hts2 <- bikes.simplified %>% aggregate_key(name, Total = sum(TotalBikes))

#Aggregated Model
bikes.TSLM <- bikes.hts %>% filter(is_aggregated(name)) %>% model(TSLM(Total ~ weekday + totalSnow_cm + uvIndex + HeatIndexC + WindChillC +
                                                                    WindGustKmph + cloudcover + humidity + precipMM + maxtempC + visibility + windspeedKmph +
                                                                    timeofday + holiday))
augment(bikes.TSLM) %>% 
  ggplot(aes(x = date)) +
  geom_line(aes(y = Total, colour = "Data"), size = 1) +
  geom_line(aes(y = .fitted, colour = "Fitted"), size = 1) + xlab("Year") + ylab(NULL) +
  guides(colour = guide_legend(title=NULL))

bikes.hts3 <- as.data.frame(bikes.hts2)

area.chart <- bikes.hts3 %>% group_by(date, name) %>% summarize(total = sum(Total)) %>% mutate(freq = total/sum(total))
area.chart

#let's try using an ets model on the aggregated one
bikes.agg.ets <- bikes.hts2 %>% filter(is_aggregated(name)) %>% model(ETS(Total ~ error("A") + trend("Ad") + season("A")))

augment(bikes.agg.ets) %>% 
  ggplot(aes(x = date)) +
  geom_line(aes(y = Total, colour = "Data"), size = 1) +
  geom_line(aes(y = .fitted, colour = "Fitted"), size = 1) + xlab("Year") + ylab(NULL) +
  guides(colour = guide_legend(title=NULL))                                                                  

bikes.agg.ets %>% augment()

bikes.agg.ets %>% forecast(h = 240) %>% autoplot(bikes.hts2)
#forecast appears usless

#let's check last weeks data

bikes.hts %>% filter(date > as.Date("2022-05-10")) %>% autoplot()


#how about a simple ARIMA?
bikes.hts %>% features(Total, unitroot_kpss)
bikes.hts %>% mutate(diff_Total = difference(Total)) %>% features(diff_Total, unitroot_kpss)
bikes.hts %>% features(Total, unitroot_ndiffs)

fit <- bikes.hts %>% model(ARIMA(Total))
report(fit)                                   
fit %>% forecast(h = 240) %>% autoplot(bikes.hts)
                               
bikes.hts %>% gg_tsdisplay(plot_type = "partial")
    
#also a fairly useless forecast



#let's try DYNAMIC REGRESSION now

dr.agg <- bikes.hts %>% model(ARIMA(Total ~ weekday + totalSnow_cm + uvIndex + HeatIndexC + WindChillC +
                                      WindGustKmph + cloudcover + humidity + precipMM + maxtempC + visibility + windspeedKmph +
                                      timeofday + holiday))

augment(dr.agg) %>% 
  ggplot(aes(x = date)) +
  geom_line(aes(y = Total, colour = "Data"), size = 1) +
  geom_line(aes(y = .fitted, colour = "Fitted"), size = 1) + xlab("Year") + ylab(NULL) +
  guides(colour = guide_legend(title=NULL))  

#horrible overfit

#without aggregation - DON'T RUN THIS. WILL TAKE 20 YEARS TO RUN
mod.dr <- bikes.simplified %>% model(ARIMA(TotalBikes ~ weekday + totalSnow_cm + uvIndex + HeatIndexC + WindChillC +
                                             WindGustKmph + cloudcover + humidity + precipMM + maxtempC + visibility + windspeedKmph +
                                             timeofday + holiday))


#So, either the aggregated TSLM model or the regular TSLM model can be used for forecasting. The next step is to just forecast


