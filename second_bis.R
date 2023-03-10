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
  # plot y = n_2_rappel_h en fonction de x = month
  plot(
    dataByRegionByMonth[[i]]$month,
    dataByRegionByMonth[[i]]$n_2_rappel_h,
    main=statsDataByRegionByMonth[[i]]$region,
    xlim=c(as.Date("2020-01-01"), as.Date("2023-03-01")),
    xlab="Mois",
    ylab="Nombre de vaccin??s",
    col="blue",
    pch=19)

  points(
    dataByRegionByMonth[[i]]$month,
    dataByRegionByMonth[[i]]$n_2_rappel_f,
    col="red",
    pch=19)

  # regression lineaire
  abline(lm(dataByRegionByMonth[[i]]$n_2_rappel_h ~ dataByRegionByMonth[[i]]$month), col="blue")

  # predict for 2023 each month
  modeleHomme <- lm(n_2_rappel_h ~ poly(month,1) , data=dataByRegionByMonth[[i]])
  new <- data.frame(month=as.Date(c("2023-01-01", "2023-02-01")))
  predict <- predict(modeleHomme, new , interval="confidence")
  predict <- data.frame(predict)

  modeleFemme <- lm(n_2_rappel_f ~ poly(month,1) , data=dataByRegionByMonth[[i]])
  new1 <- data.frame(month=as.Date(c("2023-01-01", "2023-02-01")))
  predict1 <- predict(modeleFemme, new1 , interval="confidence")
  predict1 <- data.frame(predict1)

  # plot predict homme
  points(new$month, predict$fit, col="#459afc")
  points(new$month, predict$lwr, col="cyan")
  points(new$month, predict$upr, col="#1656cc")

  # plot predict femme
  points(new1$month, predict1$fit, col="#ff9900")
  points(new1$month, predict1$lwr, col="#ce5221")
  points(new1$month, predict1$upr, col="#ff0000")

  # legend
  legend("topleft", legend=c("Homme", "Femme"), col=c("blue", "red"), pch=19)
}
# nolint end