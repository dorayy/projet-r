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
listReg <- sort(data$reg[!duplicated(data$reg)])
nomReg <- c("Guadeloupe","Martinique","Guyane","La Réunion","Saint-Pierre-et-Miquelon","Mayotte","Saint-Barthélemy","Saint-Martin","Ile-de-France","Centre-Val de Loire","Bourgogne-Franche-Comté","Normandie","Hauts-de-France","Grand Est","Pays de la Loire","Bretagne","Nouvelle-Aquitaine","Occitanie","Auvergne-Rhône-Alpes","Provence-Alpes-Côte d’Azur","Corse")
dataByRegionByMonth <- foreach(i=listReg) %do% (data[data$reg == i, ] %>% 
                        group_by(month=floor_date(jour, "month"),reg=reg) %>% 
                        summarize(count_all=sum(n_dose1_h,n_rappel_h,n_2_rappel_h,n_3_rappel_h,n_dose1_f,n_rappel_f,n_2_rappel_f,n_3_rappel_f)))

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
            summaryAll=summary(dataByRegionByMonth[[i]]$count_all),
            modeAll=find_mode(dataByRegionByMonth[[i]]$count_all)[1],
            interQuantileAll=IQR(dataByRegionByMonth[[i]]$count_all),
            firstDecileAll=quantile(dataByRegionByMonth[[i]]$count_all,0.1),  
            lastDecileAll=quantile(dataByRegionByMonth[[i]]$count_all,0.9),
            varianceAll=var(dataByRegionByMonth[[i]]$count_all),
            ecarTypeAll=sd(dataByRegionByMonth[[i]]$count_all)
            ))
View(statsDataByRegionByMonth)

foreach(i=1:length(statsDataByRegionByMonth)) %do% {

  data.ts = ts(dataByRegionByMonth[[i]]$count_all, start=c(2020,12), frequency = 12)
  seasonal_arima_model = auto.arima(data.ts)
  season_arima_forecast = forecast(seasonal_arima_model, h=3, level=c(80,99))

  plot(
    season_arima_forecast,
    main=statsDataByRegionByMonth[[i]]$name,
    ylim=c(0,max(dataByRegionByMonth[[i]]$count_all)),
    xlab="Mois",
    ylab="Nombre de vaccinés",
    col="blue",
    pch=19)

}
# nolint end

