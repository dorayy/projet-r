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

# nolint start
listAge <- sort(data$clage_vacsi[!duplicated(data$clage_vacsi)])
nameAge <- c("Tous âges","0-4","5-9","10-11","12-17","18-24","25-29","30-39","40-49","50-59","60-64","65-69","70-74","75-79","80 et +")
dataByAgeByMonth <- foreach(i=listAge) %do% (data[data$clage_vacsi == i, ] %>% 
                        group_by(month=floor_date(jour, "month"),age=clage_vacsi) %>% 
                        summarize(count_all=sum(n_dose1_h,n_rappel_h,n_2_rappel_h,n_3_rappel_h,n_dose1_f,n_rappel_f,n_2_rappel_f,n_3_rappel_f)))

statsDataByAgeByMonth <- list()
find_mode <- function(x) {
  u <- unique(x)
  tab <- tabulate(match(x, u))
  u[tab == max(tab)]
}

statsDataByAgeByMonth <- 
    foreach(i=1:length(dataByAgeByMonth)) %do% (list(
            age=dataByAgeByMonth[[i]]$age[1],
            range=nameAge[i],
            summaryAll=summary(dataByAgeByMonth[[i]]$count_all),
            modeAll=find_mode(dataByAgeByMonth[[i]]$count_all)[1],
            interQuantileAll=IQR(dataByAgeByMonth[[i]]$count_all),
            firstDecileAll=quantile(dataByAgeByMonth[[i]]$count_all,0.1),  
            lastDecileAll=quantile(dataByAgeByMonth[[i]]$count_all,0.9),
            varianceAll=var(dataByAgeByMonth[[i]]$count_all),
            ecarTypeAll=sd(dataByAgeByMonth[[i]]$count_all)
            ))
View(statsDataByRegionByMonth)

foreach(i=1:length(statsDataByRegionByMonth)) %do% {

  data.ts = ts(dataByRegionByMonth[[i]]$count_all, start=c(2020,12), frequency = 12)
  seasonal_arima_model = auto.arima(data.ts)
  season_arima_forecast = forecast(seasonal_arima_model, h=3, level=c(80,99))
  
  plot(
    season_arima_forecast,
    main=statsDataByAgeByMonth[[i]]$range,
    ylim=c(0,max(dataByRegionByMonth[[i]]$count_all)),
    xlab="Mois",
    ylab="Nombre de vaccinés",
    col="blue",
    pch=19)

}
# nolint end

