# This script is just to call the data
setwd('C:/Users/nb/Desktop/shinycusum')

setwd('C:/Users/nb/Desktop/thesis')

data <- read.csv("C:/Users/nb/Desktop/thesis/OutbreakDetector_sampledata.csv")
data <- data[,-4]
#######
colnames(data) <- c("Date","Count","Holiday")

