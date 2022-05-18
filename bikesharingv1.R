#loading the df
library(fpp3)
library(zoo)
library(timeDate)
library(GGally)
library(broom)

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
total.ts <- total.ts %>% filter(as.Date(date) < as.Date("2022-05-11"))



#we can see long periods of the same number of bikes. To deal with NAs, we'll just replace the NAs with the last value
total.ts %>% scan_gaps()
total.ts <- total.ts %>% fill_gaps(.full = TRUE)
total.ts <- na.locf(total.ts)
total.ts %>% scan_gaps()

#let's add a sum of the two bikes
total.ts <- total.ts %>% mutate(TotalBikes = Bike + `E-Bike`)

total.ts %>% summarize(count = sum(TotalBikes)) %>% autoplot() + ggtitle("Bike Demand in Nyon", subtitle = "Combined Bikes and E-Bikes Demand") + xlab("Time") + ylab("Count of Bikes")

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
total.ts %>% ggplot() + geom_jitter(aes(Bike, `E-Bike`)) + facet_wrap(vars(name))
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

#there's clearly a difference. Some places have more demand on holidays, some have less
# nightday <- function(datetime) {
#   paste(
#     c("Night", "Morning", "Morning Rush", "Morning", "Lunch", "Afternoon", "Afternoon Rush", "Evening", "Night")[
#       cut(as.numeric(format(datetime, "%H%M")), c(0, 530, 0700, 0800, 1100, 1300, 1600, 1700 ,2000, 2359))
#     ]
#   )
# }
# 
# 
# total.ts$timeofday <- as.factor(nightday(total.ts$date))
# total.ts$timeofday[total.ts$timeofday == "NA"] <- "Night"
# total.ts$timeofday <- total.ts$timeofday %>%  droplevels()

#now let's see if timeofday makes a difference:

# total.ts %>%
#   group_by(timeofday) %>% 
#   summarize(count = mean(TotalBikes)) %>%
#   ggplot() + geom_boxplot(aes(x = timeofday, y = count)) +
#   labs(title = "Comparison of Time of Day", x = "Time of Day", y = "Average Bikes at Station")

#loading weather data
weather <- read.csv("Nyon.csv")
weather$date_time

weather <- weather %>% select(-c('tempC', 'mintempC', 'sunHour', 'moon_illumination', 'moonrise', 'moonset', 'sunrise', 'sunset',
                     'DewPointC', 'pressure', 'FeelsLikeC', 'winddirDegree', 'location'))

total.ts$date_time <-  format(total.ts$date, format = '%F %H:00:00')

bikes <- merge(total.ts, weather, by = "date_time")
bikes.ts <- as_tsibble(bikes, index = date, key = name, regular = TRUE)

#now we just need an EDA with weather! -- APPENDIX

#we don't need bikes, e-bikes, date_time to run the model
bikes.simplified <- bikes.ts %>% select(!c(Bike, `E-Bike`, date_time))

bikes.simplified %>% ggplot() +
  geom_jitter(aes(x = date, y = TotalBikes, color = maxtempC)) +
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

#no aggregation TSLM model
bikes.lm <- bikes.simplified %>% model(TSLM(TotalBikes ~ weekday + totalSnow_cm + uvIndex + HeatIndexC + WindChillC +
                                       WindGustKmph + cloudcover + humidity + precipMM + maxtempC + visibility + windspeedKmph +
                                       holiday))

bikes.lm %>% filter(name == 'Nyon, Changins') %>% report()

#checking model vs fitted values (for 1 place)
augment(bikes.lm) %>% filter(name == 'Nyon, La Plage') %>% 
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

#aggregated model
bikes.hts <- bikes.simplified %>%
  aggregate_key(name, Total = sum(TotalBikes), weekday = weekday, totalSnow_cm = totalSnow_cm, uvIndex = uvIndex, HeatIndexC = HeatIndexC, WindChillC = WindChillC,
                  WindGustKmph = WindGustKmph, cloudcover = cloudcover, humidity = humidity,  precipMM = precipMM, maxtempC = maxtempC,  visibility = visibility, windspeedKmph = windspeedKmph,
                  holiday = holiday)

bikes.hts2 <- bikes.simplified %>% aggregate_key(name, Total = sum(TotalBikes))

bikes.hts <- bikes.hts[!duplicated(bikes.hts[c(1,2)]),]
bikes.hts


#Aggregated Model --BOTTOM UP AND TOPDOWN APPROACHES
bikes.lm.agg <- bikes.hts %>% model(tl = TSLM(Total ~ weekday + totalSnow_cm + uvIndex + HeatIndexC + WindChillC +
                                                                         WindGustKmph + cloudcover + humidity + precipMM + maxtempC + visibility + windspeedKmph +
                                                                         holiday))

augment(bikes.TSLM2) %>% 
  ggplot(aes(x = date)) +
  geom_line(aes(y = Total, colour = "Data"), size = 1) +
  geom_line(aes(y = .fitted, colour = "Fitted"), size = 1) + xlab("Year") + ylab(NULL) +
  guides(colour = guide_legend(title=NULL))

#let's try using an ets model on the aggregated one --APPENDIX
bikes.agg.ets <- bikes.hts %>% model(etsmod = ETS(Total)) %>% forecast()
bikes.ets <- bikes.simplified %>% model(ETS(TotalBikes ~ error("A") + trend("N") + season("A")))

#show the forecast
bikes.ets %>% filter(name == 'Nyon, Gare Nord') %>% forecast(h = 1000) %>% autoplot(bikes.simplified)

#show the fitted values
augment(bikes.ets) %>% filter(name == 'Nyon, Gare Nord') %>% 
  ggplot(aes(x = date)) +
  geom_line(aes(y = TotalBikes, colour = "Data"), size = 1) +
  geom_line(aes(y = .fitted, colour = "Fitted"), size = 1) + xlab("Year") + ylab(NULL) +
  guides(colour = guide_legend(title=NULL))                                                                  

bikes.agg.ets

#show the forecast for agg model
bikes.agg.ets %>% forecast(h = "24 hours", method = 'bu') %>% autoplot(bikes.hts)
#forecast appears usless

#let's check last weeks data

#how about a simple ARIMA? --APPENDIX
bikes.hts %>% features(Total, unitroot_kpss)
bikes.hts %>% mutate(diff_Total = difference(Total)) %>% features(diff_Total, unitroot_kpss)
bikes.hts %>% features(Total, unitroot_ndiffs)

fit <- bikes.hts %>% model(ARIMA(Total))
report(fit)                                   
fit %>% forecast(h = 240) %>% autoplot(bikes.hts)
                               
bikes.hts %>% gg_tsdisplay(plot_type = "partial")
#also a fairly useless forecast



#let's try DYNAMIC REGRESSION now --APPENDIX
dr.agg <- bikes.hts %>% model(ARIMA(Total ~ weekday + totalSnow_cm + uvIndex + HeatIndexC + WindChillC +
                                      WindGustKmph + cloudcover + humidity + precipMM + maxtempC + visibility + windspeedKmph +
                                      timeofday + holiday))

augment(dr.agg) %>% 
  ggplot(aes(x = date)) +
  geom_line(aes(y = Total, colour = "Data"), size = 1) +
  geom_line(aes(y = .fitted, colour = "Fitted"), size = 1) + xlab("Year") + ylab(NULL) +
  guides(colour = guide_legend(title=NULL))  

#horrible overfit

#So, either the aggregated TSLM model or the regular TSLM model can be used for forecasting. The next step is to just forecast

#separate models for bikes and e-bikes: --APPENDIX
bikes.ts.lm <- bikes.ts %>% model(TSLM(Bike ~ weekday + totalSnow_cm + uvIndex + HeatIndexC + WindChillC +
                                              WindGustKmph + cloudcover + humidity + precipMM + maxtempC + visibility + windspeedKmph +
                                              timeofday + holiday))

#fitting the values
augment(bikes.ts.lm) %>% filter(name == 'Nyon, Gare Sud') %>% 
  ggplot(aes(x = date)) +
  geom_line(aes(y = Bike, colour = "Data"), size = 1) +
  geom_line(aes(y = .fitted, colour = "Fitted"), size = 1) + xlab("Year") + ylab(NULL) +
  guides(colour = guide_legend(title=NULL))

#checking the report
bikes.ts.lm %>% filter(name == 'Nyon, Gare Sud') %>% report()

#creating a model for e-bikes
ebikes.ts.lm <- bikes.ts %>% model(TSLM(`E-Bike` ~ weekday + totalSnow_cm + uvIndex + HeatIndexC + WindChillC +
                                         WindGustKmph + cloudcover + humidity + precipMM + maxtempC + visibility + windspeedKmph +
                                         timeofday + holiday))
#fitting the values to existing values
augment(ebikes.ts.lm) %>% filter(name == 'Nyon, Gare Sud') %>% 
  ggplot(aes(x = date)) +
  geom_line(aes(y = `E-Bike`, colour = "Data"), size = 1) +
  geom_line(aes(y = .fitted, colour = "Fitted"), size = 1) + xlab("Year") + ylab(NULL) +
  guides(colour = guide_legend(title=NULL))

#checking the report
ebikes.ts.lm %>% filter(name == 'Nyon, Gare Sud') %>% report()

#R-Squared for each of the models --APPENDIX
reportbikes <- report(bikes.ts.lm)
reportebikes <- report(ebikes.ts.lm)
reporttotal <- report(bikes.lm)
reportagg <- report(bikes.lm.agg)

#let's compare the Adjusted-Rsquared of all the main models under consideration
mean(reportbikes$adj_r_squared)
mean(reportebikes$adj_r_squared)
mean(reporttotal$adj_r_squared)
mean(reportagg$adj_r_squared)


#All right, now that we've done all of that nonsense, let's forecast!
newdata <- read.csv("newdata.csv")
newdata <- newdata %>% select(-c('tempC', 'mintempC', 'sunHour', 'moon_illumination', 'moonrise', 'moonset', 'sunrise', 'sunset',
                                             'DewPointC', 'pressure', 'FeelsLikeC', 'winddirDegree', 'location'))

#adding the relevant columns
newdata$date <- as.POSIXct(newdata$date_time, format = "%d/%m/%Y %H:%M", tz = "UTC")
newdata$timeofday <- as.factor(nightday(newdata$date))
newdata$weekday <- as.factor(ifelse(isWeekday(newdata$date), "Weekday", "Weekend"))
newdata$holiday <- 0

#turning it into a tsibble and getting rid of date_time since we have date
newdata.ts <- as_tsibble(newdata, index = date, key = name)
newdata.ts <- newdata.ts %>% select(-date_time)

fcast.lm <- forecast(bikes.lm, new_data = newdata.ts)
fcast.lm

nyon <- fcast.lm %>% filter(name == 'Nyon, Changins')
bikes.simplified %>% filter(name == 'Nyon, Changins') %>%
  autoplot(TotalBikes) +
  autolayer(nyon)

#we have high multi-collinearity, therefore, we need to check if the variables we have are perfectly correlated and remove sum:
#the problem is with timeofday, let's try two models and see if they're worth removing

bikes.simplified %>% ggplot() + geom_jitter(aes(x = maxtempC, y = totalSnow_cm))
#snow is mostly zero, worth removing

bikes.simplified %>% ggplot() + geom_jitter(aes(x = maxtempC, y = uvIndex))
#not perfectly correlated but close

bikes.simplified %>% ggplot() + geom_jitter(aes(x = maxtempC, y = HeatIndexC))
#very linear trend. worth removing

