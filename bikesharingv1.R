#loading the df
library(fpp3)
library(zoo)
library(timeDate)
library(broom)
library(patchwork)
library(tidyverse)

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
total.ts %>% autoplot(Bike, show.legend = FALSE) + facet_wrap(vars(name)) + ggtitle("Bikes by Station")
total.ts <- total.ts %>% select(!Capacity)
total.ts <- total.ts %>% mutate(TotalBikes = Bike + `E-Bike`)
test.ts <- total.ts %>% filter(as.Date(date) >= as.Date("2022-05-11"))
total.ts <- total.ts %>% filter(as.Date(date) < as.Date("2022-05-11"))
test.hts <- test.ts %>% aggregate_key(name, Total = sum(TotalBikes))

#we can see long periods of the same number of bikes. To deal with NAs, we'll just replace the NAs with the last value
total.ts %>% scan_gaps()
total.ts <- total.ts %>% fill_gaps(.full = TRUE)
total.ts <- na.locf(total.ts)
total.ts %>% scan_gaps()

total.ts %>% 
  summarize(count = sum(TotalBikes)) %>% 
  autoplot() + 
  ggtitle("Bike Demand in Nyon", 
          subtitle = "Combined Bikes and E-Bikes Demand") + 
  xlab("Time") + 
  ylab("Count of Bikes")

#plotting e-bikes and bikes together
total.ts %>%
  group_by() %>%
  summarize(bikecount = sum(Bike), ebikecount = sum(`E-Bike`)) %>% 
  gather("id", "value", 2:3) %>% 
  ggplot(., aes(date, value)) +
  geom_line() +
  facet_wrap(~id)
  labs(title = "Comparison of Bike and E-Bike Demand", x = 'Date', y = 'Total Bikes')

#plotting correlation between Bike and E-BIke
total.ts %>%
  group_by() %>%
  summarize(bikecount = sum(Bike), ebikecount = sum(`E-Bike`)) %>%
  ggplot() + 
  geom_jitter(aes(bikecount, ebikecount)) +
  geom_smooth(se = FALSE, method = 'lm', aes(bikecount, ebikecount)) +
  labs(title = "Correlation between Bike Demand and E-Bike Demand", x = "Bikes", y = "E-Bikes")

#Let's see if there's a correlation between e-bike count and bike-count based on location
total.ts %>% 
  ggplot() + 
  geom_jitter(aes(Bike, `E-Bike`)) + 
  facet_wrap(vars(name))

with(total.ts, cor(Bike, `E-Bike`))

#now that we've determined that there's a fair bit off correlation, it's time to work on the dataset
#we know weekday demand should be theoretically higher than weekend demand. Let's add.
total.ts$weekday = as.factor(ifelse(isWeekday(total.ts$date), "Weekday", "Weekend"))
table(total.ts$weekday)

total.df <- as.data.frame(total.ts)

total.df %>%
  group_by(weekday) %>% 
  summarize(averagebikedemand = mean(TotalBikes))

#looks like there isn't much of a difference between demand during weekdays and weekends.
ggplot(total.ts) +
  geom_boxplot(aes(x = name, y = TotalBikes, color = weekday)) +
  coord_flip()


holidays <- c(as.Date("2022-04-15"),as.Date("2022-04-17"),as.Date("2022-04-18"),as.Date("2022-04-25"), as.Date("2022-05-01"))
total.ts$holiday <- as.factor(ifelse(as.Date(total.ts$date) %in% holidays, 1, 0))

#now let's see if there's a difference between holidays and non-holidays
ggplot(total.ts) +
  geom_boxplot(aes(x = name, y = TotalBikes, color = holiday)) +
  coord_flip()

#loading weather data
weather <- read.csv("Data/weather_data.csv")
weather$date_time

weather <- weather %>% select(-c('tempC', 'mintempC', 'sunHour', 'moon_illumination', 'moonrise', 'moonset', 'sunrise', 'sunset',
                     'DewPointC', 'pressure', 'FeelsLikeC', 'winddirDegree', 'location'))

total.ts$date_time <-  format(total.ts$date, format = '%F %H:00:00')

bikes <- merge(total.ts, weather, by = "date_time")
bikes.ts <- as_tsibble(bikes, index = date, key = name, regular = TRUE)

#now we just need an EDA with weather! -- APPENDIX

#we don't need bikes, e-bikes, date_time to run the model
bikes.simplified <- bikes.ts %>% select(!c(Bike, `E-Bike`, date_time))

bikes.simplified %>% filter(name == 'Nyon, Gare Nord') %>% ggplot() +
  geom_line(aes(x = date, y = TotalBikes, color = maxtempC)) +
  scale_color_gradient(low = "blue", high = "red")

#let's try and detect outliers in the data using PCA --APPENDIX
pca <- bikes.simplified %>% keep(is.numeric) %>% prcomp(scale = TRUE)
summary(pca)$importance[, 1:4]

bikes.df <- as.data.frame(bikes.simplified)
pca_for_chart <- pca %>% augment(bikes.df)
pca_for_chart
ggplot(pca_for_chart, aes(x = .fittedPC1, y = .fittedPC2, color = maxtempC)) +
  geom_point() + theme(aspect.ratio = 1)


#using an IQR of 3 to detect outliers
ggplot(bikes.simplified) +
  geom_boxplot(aes(x = name, y = TotalBikes), coef = 3) +
  coord_flip() + 
  labs(title = 'Outlier Detection', subtitle = "Which Station had points beyond 3*IQR", y = 'Number of Bikes at Stattion', x = NULL)


#only La Plage has outliers. Let's see where they are
outlierdetection <- bikes.simplified %>% filter(name == 'Nyon, La Plage')

outlier <- boxplot.stats(outlierdetection$TotalBikes)$out
out_ind <- which(outlierdetection$TotalBikes %in% c(outlier))
outlierdetection

ggplot(outlierdetection[out_ind,]) + geom_jitter(aes(x = date, y = TotalBikes))
#outliers are only on 28th March and 18th April. 18th April was Easter Monday hence more bikes at station"









#Finally, let's test out some models

#let's start with STL Decomposition
stldcmp <- bikes.simplified %>%
  model(
    STL(TotalBikes,
        robust = TRUE)
  ) %>%
  components()

stldcmp %>% filter(name == 'Nyon, La Plage') %>% autoplot()

my_dcmp_spec <- decomposition_model(
  STL(TotalBikes,
      robust = TRUE),
  ETS(season_adjust ~ season("N")))

fc <- bikes.simplified %>%
  model(my_dcmp_spec)
  
fc %>% filter(name == 'Nyon, La Plage') %>% augment()

#fitted values
augment(fc) %>% filter(name == 'Nyon, Gare Nord') %>% 
  ggplot(aes(x = date)) +
  geom_line(aes(y = TotalBikes, colour = "Data"), size = 1) +
  geom_line(aes(y = .fitted, colour = "Fitted"), size = 1) + xlab("Year") + ylab(NULL) +
  guides(colour = guide_legend(title=NULL))

#forecast for 5 days
fcast.stl <- fc %>% forecast(h = "5 days")
accuracymatrix <- fcast.stl %>% accuracy(test.ts) %>% select(1:6)


#no aggregation TSLM model
bikes.lm <- bikes.simplified %>% model(TSLM = TSLM(TotalBikes ~ weekday + totalSnow_cm + uvIndex + HeatIndexC + WindChillC +
                                       WindGustKmph + cloudcover + humidity + precipMM + maxtempC + visibility + windspeedKmph +
                                       holiday))

bikes.lm %>% filter(name == 'Nyon, Changins') %>% report()

#checking model vs fitted values (for 1 place)
augment(bikes.lm) %>% filter(name == 'Nyon, Gare Nord') %>% 
  ggplot(aes(x = date)) +
  geom_line(aes(y = TotalBikes, colour = "Data"), size = 1) +
  geom_line(aes(y = .fitted, colour = "Fitted"), size = 1) + xlab("Year") + ylab(NULL) +
  guides(colour = guide_legend(title=NULL))

#relationship between actual and fitted values -- APPENDIX
augment(bikes.lm) %>% filter(name == 'Nyon, Gare Sud') %>% 
  ggplot(aes(x = TotalBikes, y = .fitted)) + geom_point() +
  ylab("Fitted (predicted values)") + xlab("Data (actual values)") +
  geom_abline(intercept = 0, slope = 1)

#checking residuals --APPENDIX
bikes.lm %>% filter(name == 'Nyon, Gare Sud') %>% gg_tsresiduals()

#forecasting accuracy:
#All right, now that we've done all of that nonsense, let's forecast!
newdata <- read.csv("Data/newdata.csv")
newdata <- newdata %>% select(-c('tempC', 'mintempC', 'sunHour', 'moon_illumination', 'moonrise', 'moonset', 'sunrise', 'sunset',
                                 'DewPointC', 'pressure', 'FeelsLikeC', 'winddirDegree', 'location'))

date <- seq.POSIXt(from = as.POSIXct('2022-05-11 00:00:00', tz = 'UTC'), to = as.POSIXct('2022-05-15 23:50:00', tz = 'UTC'), by = '10 min')
date <- as.data.frame(date)
date$joindates <- format(date$date, format = '%F %H:00:00')
date$joindates <- as.POSIXct(date$joindates, tz = 'UTC')
newdata$date_time <- as.POSIXct(newdata$date_time, format = '%d/%m/%Y %H:00',  tz = 'UTC')

newdata <- left_join(date, newdata, by = c('joindates' = 'date_time'))
newdata <- newdata %>% select(-joindates)

#adding the relevant columns

newdata$weekday <- as.factor(ifelse(isWeekday(newdata$date), "Weekday", "Weekend"))
newdata$holiday <- 0

#turning it into a tsibble and getting rid of date_time since we have date
newdata.ts <- as_tsibble(newdata, index = date, key = name)

fcast.lm <- bikes.lm %>% forecast(new_data = newdata.ts)
fcast.lm %>% accuracy(test.ts)
fcast.lm %>% filter(name == 'Nyon, Changins') %>% autoplot(bikes.simplified, level = 90)

accuracymatrix <- rbind(accuracymatrix, fcast.lm %>% accuracy(test.ts) %>% select(1:6))


#aggregated model
bikes.hts <- bikes.simplified %>%
  aggregate_key(name, Total = sum(TotalBikes))

#let's try using an ets model on the aggregated one --APPENDIX
bikes.agg.ets <- bikes.hts %>% model(AggregateETS = ETS(Total))
bikes.ets <- bikes.simplified %>% model(SimpleETS = ETS(TotalBikes ~ error("A") + season("A")))

fcast.agg.ets <- bikes.agg.ets %>% forecast(h = "5 days")
fcast.ets <- bikes.ets %>% forecast(h = "5 days")

accuracymatrix <- rbind(accuracymatrix, fcast.ets %>% accuracy(test.ts) %>% select(1:6))

#show the forecast
bikes.ets %>% filter(name == 'Nyon, Gare Nord') %>% forecast(h = "5 days") %>% autoplot(bikes.simplified)
bikes.agg.ets %>% filter(name == 'Nyon, Gare Nord') %>% forecast(h = "5 days", method = 'bu') %>% autoplot(bikes.hts)
                                                             

#how about a simple ARIMA? --APPENDIX
bikes.hts %>% features(Total, unitroot_kpss)
bikes.hts %>% mutate(diff_Total = difference(Total)) %>% features(diff_Total, unitroot_kpss)
bikes.hts %>% features(Total, unitroot_ndiffs)

bikes.arima <- bikes.simplified %>% model(ARIMA(TotalBikes))
fcast.arima <- bikes.arima %>% forecast(h = "5 days")
fcast.arima %>% accuracy(test.ts)
fcast.arima %>% filter(name == 'Nyon, Gare Nord') %>% autoplot(bikes.simplified)
accuracymatrix <- rbind(accuracymatrix, fcast.arima %>% accuracy(test.ts) %>% select(1:6))

write.csv(accuracymatrix, "accuracymatrix.csv")

#getting the accuracies of all the models
accuracymatrix %>% group_by(.model) %>% summarize(ME = mean(ME), RMSE = mean(RMSE), MAE = mean(MAE))

#plotting the models to see the confidence intervals. ETS and ARIMA perform really poorly
p1 <- fcast.lm %>% filter(name == 'Nyon, Gare Nord') %>% autoplot(bikes.simplified) + ggtitle('TSLM Model')
p2 <- fcast.stl %>% filter(name == 'Nyon, Gare Nord') %>% autoplot(bikes.simplified) + ggtitle('STL Model')
p3 <- fcast.ets %>% filter(name == 'Nyon, Gare Nord') %>% autoplot(bikes.simplified) + ggtitle('ETS Model')
p4 <- fcast.arima %>% filter(name == 'Nyon, Gare Nord') %>% autoplot(bikes.simplified) + ggtitle('ARIMA')

(p1 | p2) / (p3 | p4)

#testing accuracy based on just 24 hour forecast
oneday <- newdata.ts %>% filter(as.Date(date) < as.Date("2022-05-12"))
oneday

#creating the forecasts
m1 <- bikes.lm %>% forecast(new_data = oneday, h = "1 day") 
m2 <- fc %>% forecast(h = "1 day") 
m3 <- bikes.ets %>% forecast(h = '1 day')
m4 <- bikes.arima %>% forecast(h = '1 day')

#plotting
c1 <- m1 %>% filter(name == 'Nyon, Gare Nord') %>% autoplot(bikes.simplified) + ggtitle('TSLM Model')
c2 <- m2 %>% filter(name == 'Nyon, Gare Nord') %>% autoplot(bikes.simplified) + ggtitle('STL Model')
c3 <- m3 %>% filter(name == 'Nyon, Gare Nord') %>% autoplot(bikes.simplified) + ggtitle('ETS Model')
c4 <- m4 %>% filter(name == 'Nyon, Gare Nord') %>% autoplot(bikes.simplified) + ggtitle('ARIMA')

(c1 | c2) / (c3 | c4)

#testing the accuracy
a <- rbind(
          m1 %>% accuracy(test.ts) %>% select(1:6),
          m2 %>% accuracy(test.ts) %>% select(1:6),
          m3 %>% accuracy(test.ts) %>% select(1:6),
          m4 %>% accuracy(test.ts) %>% select(1:6))

#seeing the accuracy. The DCMP model outperforms all easily
a %>% group_by(.model) %>% summarize(ME = mean(ME), RMSE = mean(RMSE), MAE = mean(MAE))


#we've chosen our model. Now, it remains to be seen whether we want to do an aggregated one, 
#a bike/e-bike separate one or combined one. There are some models to choose from here. HTS and non

#we've done 1. So let's do 2,3,4 and decide. We'll be using the total.ts dataset

#creating an aggregated dataset
total.hts <- total.ts %>% aggregate_key(name, TotalBikes = sum(TotalBikes))
test.hts <- test.ts %>% aggregate_key(name, TotalBikes = sum(TotalBikes))

hts.components <- total.hts %>%
                      model(
                        STL(TotalBikes,
                            robust = TRUE)
                      ) %>%
                      components()

hts_dcmp_spec <- decomposition_model(
  STL(TotalBikes,
      robust = TRUE),
  ETS(season_adjust ~ season("N"))
)

htsfcast <- total.hts %>% model(base = hts_dcmp_spec)
accmatrix <- htsfcast %>% reconcile(bu = bottom_up(base)) %>% forecast(h = "1 day") %>% accuracy(test.hts)

write.csv(accmatrix, "accmatrix.csv")
write.csv(accuracymatrix, "accurmatrix.csv")
accuracymatrix2 <- accuracymatrix %>% filter(.model != "AggregateETS")
######################################

#FINAL COMPARISON. HTS OR NON-HTS

final_acc <- rbind(accuracymatrix %>% filter(.model == "my_dcmp_spec"), accmatrix %>% select(1:6))
final_acc %>% group_by(.model) %>% summarize(ME = mean(ME), RMSE = mean(RMSE), MAE = mean(MAE))

ME <- cbind(accuracymatrix %>% filter(.model == "my_dcmp_spec") %>% select(1,2,4),
  accmatrix %>% filter(!is_aggregated(name) & .model == "bu") %>% select(1,2,4))

RMSE <- cbind(accuracymatrix %>% filter(.model == "my_dcmp_spec") %>% select(1,5),
              accmatrix %>% filter(!is_aggregated(name) & .model == "bu") %>% select(1,2,5))

RMSE

#FINAL FORECAST

#the aggregated hts model outperforms the regular model by some distance using both RMSE and MAE

#using the entire dataset to recreate model:

load('final_dataset.Rdata')

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
  
all.ts <- as_tsibble(total, index = date, key = name, regular = TRUE)
all.ts <- all.ts %>% select(!Capacity)
all.ts <- all.ts %>% fill_gaps(.full = TRUE)
all.ts <- na.locf(all.ts)
all.ts %>% scan_gaps()
all.ts <- all.ts %>% mutate(TotalBikes = Bike + `E-Bike`)

all.hts <- all.ts %>% aggregate_key(name, TotalBikes = sum(TotalBikes))
all.hts

hts.components <- all.hts %>%
  model(
    STL(TotalBikes,
        robust = TRUE)
  ) %>%
  components()

hts_dcmp_spec <- decomposition_model(
  STL(TotalBikes,
      robust = TRUE),
  ETS(season_adjust ~ season("N"))
)

htsfcast <- all.hts %>% model(base = hts_dcmp_spec)
finalforecast <- htsfcast %>% reconcile(bu = bottom_up(base)) %>% forecast(h = "2 days")

finalforecast %>% filter(is_aggregated(name) & .model == 'bu') %>% autoplot(all.hts, level = 90)
export <- finalforecast %>% filter(!is_aggregated(name) & .model == 'bu') %>% hilo(90)
export

write.csv(export, "forecast.csv")

