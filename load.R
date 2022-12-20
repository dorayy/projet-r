# import csv file
data <- read.csv("data.csv", header = TRUE, sep = ";")
# convert to data frame
data <- as.data.frame(data)

# show first 6 rows
head(data)
