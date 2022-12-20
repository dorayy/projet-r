# read csv from same directory
#data <- read.csv("D:/IPPSI/M2/R/projet-r/data.csv", header = TRUE, sep = ";")
data <- read.csv("C:/Users/lahou/Documents/IPSSI/Maths-R/projet-r/data.csv", header = TRUE, sep = ";")

# remove all lines equal 0 to each column
data <- data[!apply(data[-1:-3], 1, function(x) all(x == 0)), ]

# convert to data frame
data <- as.data.frame(data)

# convert to date and order by date
data$jour <- as.Date(data$jour)
data <- data[order(data$jour), ]

# library
library(dplyr)

# get all men data
dataH <- data %>% select(contains(c("reg","clage","jour","_h")))

# get all women data
dataF <- data %>% select(contains(c("reg","clage","jour","_f")))

# get all non identified data
dataE <- data %>% select(contains(c("reg","clage","jour","_e")))

# get data of 2020 and order by date
data2020 <- data[(data$jour < as.Date("2021-01-01")), ]
data2020 <- data2020[order(data2020$jour), ]

# get all men data
dataH2020 <- data2020 %>% select(contains(c("reg","clage","jour","_h")))
View(dataH2020)

# get all women data
dataF2020 <- data2020 %>% select(contains(c("reg","clage","jour","_f")))
View(dataF2020)

# get all non identified data
dataE2020 <- data2020 %>% select(contains(c("reg","clage","jour","_e")))
View(dataE2020)

# get data of 2021 and order by date
data2021 <- data[(data$jour > as.Date("2020-12-31") & data$jour < as.Date("2022-01-01")), ]
data2021 <- data2021[order(data2021$jour), ]

# get all men data
dataH2021 <- data2021 %>% select(contains(c("reg","clage","jour","_h")))
View(dataH2021)

# get all women data
dataF2021 <- data2021 %>% select(contains(c("reg","clage","jour","_f")))
View(dataF2021)

# get all non identified data
dataE2021 <- data2021 %>% select(contains(c("reg","clage","jour","_e")))
View(dataE2021)

# get data of 2021 and order by date
data2022 <- data[(data$jour > as.Date("2021-12-31") & data$jour < as.Date("2023-01-01")), ]
data2022 <- data2022[order(data2022$jour), ]

# get all men data
dataH2022 <- data2022 %>% select(contains(c("reg","clage","jour","_h")))
View(dataH2022)

# get all women data
dataF2022 <- data2022 %>% select(contains(c("reg","clage","jour","_f")))
View(dataF2022)

# get all non identified data
dataE2022 <- data2022 %>% select(contains(c("reg","clage","jour","_e")))
View(dataE2022)
