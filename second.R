# set working directory
setwd("D:/IPPSI/M2/R/projet-r")

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

# nolint start
listReg <- sort(data$reg[!duplicated(data$reg)])
nomReg <- c("Guadeloupe","Martinique","Guyane","La Réunion","Saint-Pierre-et-Miquelon","Mayotte","Saint-Barthélemy","Saint-Martin","Ile-de-France","Centre-Val de Loire","Bourgogne-Franche-Comté","Normandie","Hauts-de-France","Grand Est","Pays de la Loire","Bretagne","Nouvelle-Aquitaine","Occitanie","Auvergne-Rhône-Alpes","Provence-Alpes-Côte d’Azur","Corse")
dataByRegionByMonth <- foreach(i=listReg) %do% (data[data$reg == i, ] %>% 
                        group_by(month=floor_date(jour, "month"),reg=reg) %>% 
                        summarize(n_2_rappel_h=sum(n_2_rappel_h),n_2_rappel_f=sum(n_2_rappel_f)))

statsDataByRegionByMonth <- list()
find_mode <- function(x) {
  u <- unique(x)
  tab <- tabulate(match(x, u))
  u[tab == max(tab)]
}

statsDataByRegionByMonth <- 
    foreach(i=1:length(dataByRegionByMonth)) %do% (list(
            region=dataByRegionByMonth[[i]]$reg[1],
            name=nomReg[i],
            summaryHomme=summary(dataByRegionByMonth[[i]]$n_2_rappel_h),
            summaryFemme=summary(dataByRegionByMonth[[i]]$n_2_rappel_f),
            modeHomme=find_mode(dataByRegionByMonth[[i]]$n_2_rappel_h)[1],
            modeFemme=find_mode(dataByRegionByMonth[[i]]$n_2_rappel_f)[1],
            interQuantileHomme=IQR(dataByRegionByMonth[[i]]$n_2_rappel_h),
            interQuantileFemme=IQR(dataByRegionByMonth[[i]]$n_2_rappel_f),
            firstDecileHomme=quantile(dataByRegionByMonth[[i]]$n_2_rappel_h,0.1),
            firstDecileFemme=quantile(dataByRegionByMonth[[i]]$n_2_rappel_f,0.1),  
            lastDecileHomme=quantile(dataByRegionByMonth[[i]]$n_2_rappel_h,0.9),
            lastDecileFemme=quantile(dataByRegionByMonth[[i]]$n_2_rappel_f,0.9),
            varianceHomme=var(dataByRegionByMonth[[i]]$n_2_rappel_h),
            varianceFemme=var(dataByRegionByMonth[[i]]$n_2_rappel_f),
            ecarTypeHomme=sd(dataByRegionByMonth[[i]]$n_2_rappel_h),
            ecarTypeFemme=sd(dataByRegionByMonth[[i]]$n_2_rappel_f),
            covariance=cov(dataByRegionByMonth[[i]]$n_2_rappel_h,dataByRegionByMonth[[i]]$n_2_rappel_f),
            corelation=cor(dataByRegionByMonth[[i]]$n_2_rappel_h,dataByRegionByMonth[[i]]$n_2_rappel_f)
        ))
View(statsDataByRegionByMonth)

foreach(i=1:length(statsDataByRegionByMonth)) %do% {
  
  dataH.ts = ts(dataByRegionByMonth[[i]]$n_2_rappel_h, start=c(2020,12), frequency = 12)
  seasonal_arima_modelH = auto.arima(dataH.ts)
  season_arima_forecastH = forecast(seasonal_arima_modelH, h=3, level=c(80,99))
  
  plot(
    season_arima_forecastH,
    main=statsDataByRegionByMonth[[i]]$name,
    ylim=c(0,max(dataByRegionByMonth[[i]]$n_2_rappel_h)),
    xlab="Mois",
    ylab="Nombre de vaccinés H",
    col="blue",
    pch=19)
  
  dataF.ts = ts(dataByRegionByMonth[[i]]$n_2_rappel_f, start=c(2020,12), frequency = 12)
  seasonal_arima_modelF = auto.arima(dataF.ts)
  season_arima_forecastF = forecast(seasonal_arima_modelF, h=3, level=c(80,99))
  
  plot(
    season_arima_forecastF,
    main=statsDataByRegionByMonth[[i]]$name,
    ylim=c(0,max(dataByRegionByMonth[[i]]$n_2_rappel_f)),
    xlab="Mois",
    ylab="Nombre de vaccinés F",
    col="blue",
    pch=19)
  
}
# nolint end