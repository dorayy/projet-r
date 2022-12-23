#  PREVION DU NOMBRE DE dose(total) EN %  ET PREVISION VACCIN ADMINISTRER POUR 2023

# set working directory
setwd("C:/Users/lahou/Documents/IPSSI/Maths-R/projet-r/")

data <- read.csv("data.csv", header = TRUE, sep = ";")

# remove all lines equal 0 to each column
data <- data[!apply(data[-1:-3], 1, function(x) all(x == 0)), ]

# convert to data frame
data <- as.data.frame(data)

# convert to date and order by date
data$jour <- as.Date(data$jour)
data <- data[order(data$jour), ]

# library
library(dplyr)
library(foreach)
library(lubridate)
library(ggplot2)

dfBiv = dataSumDose <- data %>% group_by(month=floor_date(jour, "month")) %>% summarise(bivHomme = sum(couv_complet_h) , bivFemme = sum(couv_complet_f))

dataH.ts = ts(dfBiv$bivHomme, start=c(2020,12), frequency = 12)
seasonal_arima_modelH = auto.arima(dataH.ts)
season_arima_forecastH = forecast(seasonal_arima_modelH, h=3, level=c(80,90))

plot(
  season_arima_forecastH,
  main="Prévision du nombre de vaccination après variant sur Hommes",
  ylim=c(0,max(dfBiv$bivHomme)),
  xlab="Mois",
  ylab="Nombre de vaccinés",
  col="blue",
  pch=19)

dataF.ts = ts(dfBiv$bivFemme, start=c(2020,12), frequency = 12)
seasonal_arima_modelF = auto.arima(dataF.ts)
season_arima_forecastF = forecast(seasonal_arima_modelF, h=3, level=c(80,90))

plot(
  season_arima_forecastF,
  main="Prévision du nombre de vaccination après variant sur Femmes",
  ylim=c(0,max(dfBiv$bivFemme)),
  xlab="Mois",
  ylab="Nombre de vaccinés",
  col="blue",
  pch=19)
