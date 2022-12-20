# read csv from same directory
data <- read.csv("D:/IPPSI/M2/R/projet-r/data.csv", header = TRUE, sep = ";")

# remove all lines equal 0 to each column
data <- data[!apply(data[-1:-3], 1, function(x) all(x == 0)), ]

# convert to data frame
data <- as.data.frame(data)

View(data)