# read csv from same directory
#data <- read.csv("D:/IPPSI/M2/R/projet-r/data.csv", header = TRUE, sep = ";")
#data <- read.csv("C:/Users/lahou/Documents/IPSSI/Maths-R/projet-r/data.csv", header = TRUE, sep = ";")

# remove all lines equal 0 to each column
data <- data[!apply(data[-1:-3], 1, function(x) all(x == 0)), ]

# convert to data frame
data <- as.data.frame(data)

View(data)

# convert to date
data$jour <- as.Date(data$jour)

# get data of 2020 and order by date
data2020 <- data[(data$jour < as.Date("2021-01-01")), ]
data2020 <- data2020[order(data2020$jour), ]
View(data2020)

# get data of 2021 and order by date
data2021 <- data[(data$jour > as.Date("2020-12-31") & data$jour < as.Date("2022-01-01")), ]
data2021 <- data2021[order(data2021$jour), ]
View(data2021)

# get data of 2021 and order by date
data2022 <- data[(data$jour > as.Date("2021-12-31") & data$jour < as.Date("2023-01-01")), ]
data2022 <- data2022[order(data2022$jour), ]
View(data2022)
