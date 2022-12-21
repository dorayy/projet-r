#  PREVION DU NOMBRE DE VACCINE(1 er dose) EN % PAR REGION PROPORTION 2020,2021,2022 
#  ET PREVOIR POUR 2023

# set working directory
setwd("C:/ipssi/projet-r")

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

listReg <- sort(data$reg[!duplicated(data$reg)])
dataByRegionByMonth <- foreach(i=listReg) %do% (data[data$reg == i, ] %>% group_by(month=floor_date(jour, "month"),reg=reg) %>% summarize(n_dose1_h=sum(n_dose1_h),n_dose1_f=sum(n_dose1_f)))

statsDataByRegionByMonth <- list()
find_mode <- function(x) {
  u <- unique(x)
  tab <- tabulate(match(x, u))
  u[tab == max(tab)]
}

statsDataByRegionByMonth <- 
    foreach(i=1:length(dataByRegionByMonth)) %do% (list(
            region=dataByRegionByMonth[[i]]$reg[1],
            summaryHomme=summary(dataByRegionByMonth[[i]]$n_dose1_h),
            summaryFemme=summary(dataByRegionByMonth[[i]]$n_dose1_f),
            modeHomme=find_mode(dataByRegionByMonth[[i]]$n_dose1_h)[1],
            modeFemme=find_mode(dataByRegionByMonth[[i]]$n_dose1_f)[1],
            interQuantileHomme=IQR(dataByRegionByMonth[[i]]$n_dose1_h),
            interQuantileFemme=IQR(dataByRegionByMonth[[i]]$n_dose1_f),
            firstDecileHomme=quantile(dataByRegionByMonth[[i]]$n_dose1_h,0.1),
            firstDecileFemme=quantile(dataByRegionByMonth[[i]]$n_dose1_f,0.1),
            lastDecileHomme=quantile(dataByRegionByMonth[[i]]$n_dose1_h,0.9),
            lastDecileFemme=quantile(dataByRegionByMonth[[i]]$n_dose1_f,0.9),
            varianceHomme=var(dataByRegionByMonth[[i]]$n_dose1_h),
            varianceFemme=var(dataByRegionByMonth[[i]]$n_dose1_f),
            ecarTypeHomme=sd(dataByRegionByMonth[[i]]$n_dose1_h),
            ecarTypeFemme=sd(dataByRegionByMonth[[i]]$n_dose1_f),
            covariance=cov(dataByRegionByMonth[[i]]$n_dose1_h,dataByRegionByMonth[[i]]$n_dose1_f),
            corelation=cor(dataByRegionByMonth[[i]]$n_dose1_h,dataByRegionByMonth[[i]]$n_dose1_f)
        ))
View(statsDataByRegionByMonth)

foreach(i=1:length(statsDataByRegionByMonth)) %do% {
  # plot y = n_dose1_h en fonction de x = month
  plot(
    dataByRegionByMonth[[i]]$month,
    dataByRegionByMonth[[i]]$n_dose1_h,
    main=statsDataByRegionByMonth[[i]]$region,
    xlab="Mois",
    ylab="Nombre de vaccinés",
    col="blue",
    pch=19)
  
  points(
    dataByRegionByMonth[[i]]$month,
    dataByRegionByMonth[[i]]$n_dose1_f,
    col="red",
    pch=19)
  
  modele <- lm(n_dose1_f ~ poly(month,1), data=dataByRegionByMonth[[i]])
  nextDate <- data.frame(month = as.Date(c("2023-01-01","2023-02-01")))
  predict <- predict(modele, nextDate , interval = "confidence")

  print(predict)
}

