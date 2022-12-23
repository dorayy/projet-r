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

dataSumDose <- data %>% group_by(month=floor_date(jour, "month")) %>% 
                        summarize(count=sum(n_dose1_h , n_rappel_h , n_2_rappel_h , n_3_rappel_h , n_dose1_f , n_rappel_f , n_2_rappel_f , n_3_rappel_f),)

dataSumDose$sumCount <- cumsum(dataSumDose$count)

View(dataSumDose)

data.ts = ts(dataSumDose$sumCount, start=c(2020,12), frequency = 12)
seasonal_arima_model = auto.arima(data.ts)
season_arima_forecast = forecast(seasonal_arima_model, h=12, level=c(80,99))
df_forecast <- as.data.frame(season_arima_forecast)

plot(
  season_arima_forecast,
  main="Prévision pour le nombre de vaccination",
  ylim=c(0,max(df_forecast[[1]])),
  xlab="Mois",
  ylab="Nombre de vaccinés",
  col="blue",
  pch=19)

# plot
ggplot(dataSumDose, aes(x=month, y=sumCount)) + geom_line() + geom_point() + 
        labs(title="Nombre de dose total administré en France", x="Mois", y="Nombre de dose total administré cumulé") + geom_smooth(method=lm, se=FALSE)
