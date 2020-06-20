library(forecast)
library(MASS)
library(nlme)
library(lubridate)
library(car)

digits=2
# setwd('/Users/ZFED//Users/ZFED/Dropbox/Outbreak Detector/R')
#setwd('/Users/ZFED/Dropbox/RESIDUAL CUSUM for RESPIRATORY DATA/R')

#--------------------------------------------------------------------
#                     1- DATA PREPERATION
# -------------------------------------------------------------------

# Date:


Sys.setlocale("LC_TIME", "English")


data[,"Date"] <- as.Date(parse_date_time(x = data[,"Date"], orders = c("%d-%b-%y", "%d %b %Y", "%d-%m-%Y", "%m/%d/%y", "%d.%m.%y", "%d.%m.%Y")))
# Define train and test data:
numyears_train <- 1

train_startdate <- as.Date(data[1,"Date"])
train_enddate <- as.Date(train_startdate %m+% years(numyears_train)-days(1))

test_startdate <- train_startdate %m+% years(numyears_train)
test_enddate <- data[dim(data)[1],"Date"]

b <- as.numeric(which(data[,"Date"] == train_startdate))
T <- as.numeric(which(data[,"Date"] == train_enddate))

# For including the "day of year" effect in estimation model, we need day variables:

data[,"Dayofyear"]  <- yday(data[,"Date"])

# For including the "day of week" effect in estimation model, we need day variables:

Sys.setlocale("LC_TIME", "English")


data[,"Mon_dual"] <- ifelse( weekdays(data[,"Date"]) == "Monday", 1,
                             ifelse( weekdays(data[,"Date"]) != "Monday", 0,
                                     NA)) 

data[,"Tues_dual"] <- ifelse( weekdays(data[,"Date"]) == "Tuesday", 1,
                              ifelse( weekdays(data[,"Date"]) != "Tuesday", 0,
                                      NA)) 

data[,"Wed_dual"] <- ifelse( weekdays(data[,"Date"]) == "Wednesday", 1,
                             ifelse( weekdays(data[,"Date"]) != "Wednesday", 0,
                                     NA)) 

data[,"Thu_dual"] <- ifelse( weekdays(data[,"Date"]) == "Thursday", 1,
                             ifelse( weekdays(data[,"Date"]) != "Thursday", 0,
                                     NA)) 

data[,"Fri_dual"] <- ifelse( weekdays(data[,"Date"]) == "Friday", 1,
                             ifelse( weekdays(data[,"Date"]) != "Friday", 0,
                                     NA)) 

data[,"Sat_dual"] <- ifelse( weekdays(data[,"Date"]) == "Saturday", 1,
                             ifelse( weekdays(data[,"Date"]) != "Saturday", 0,
                                     NA)) 

data[,"Sun_dual"] <- ifelse( weekdays(data[,"Date"]) == "Sunday", 1,
                             ifelse( weekdays(data[,"Date"]) != "Sunday", 0,
                                     NA)) 

# data[,c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")] <- 
#   data[,"Count"]*data[,c("Mon_dual", "Tues_dual", "Wed_dual", "Thu_dual", "Fri_dual", "Sat_dual", "Sun_dual")] 

# For including the "day of week" effect in estimation model, we need day variables:

# data[,"Month"] <- month(data[,"Date"])

data[,"Jan_dual"] <- ifelse( month(data[,"Date"]) == 1, 1,
                             ifelse( month(data[,"Date"]) != 1, 0,
                                     NA)) 

data[,"Feb_dual"] <- ifelse( month(data[,"Date"]) == 2, 1,
                             ifelse( month(data[,"Date"]) != 2, 0,
                                     NA)) 

data[,"Mar_dual"] <- ifelse( month(data[,"Date"]) == 3, 1,
                             ifelse( month(data[,"Date"]) != 3, 0,
                                     NA)) 

data[,"Apr_dual"] <- ifelse( month(data[,"Date"]) == 4, 1,
                             ifelse( month(data[,"Date"]) != 4, 0,
                                     NA)) 

data[,"May_dual"] <- ifelse( month(data[,"Date"]) == 5, 1,
                             ifelse( month(data[,"Date"]) != 5, 0,
                                     NA)) 

data[,"Jun_dual"] <- ifelse( month(data[,"Date"]) == 6, 1,
                             ifelse( month(data[,"Date"]) != 6, 0,
                                     NA)) 

data[,"Jul_dual"] <- ifelse( month(data[,"Date"]) == 7, 1,
                             ifelse( month(data[,"Date"]) != 7, 0,
                                     NA)) 

data[,"Aug_dual"] <- ifelse( month(data[,"Date"]) == 8, 1,
                             ifelse( month(data[,"Date"]) != 8, 0,
                                     NA)) 
data[,"Sep_dual"] <- ifelse( month(data[,"Date"]) == 9, 1,
                             ifelse( month(data[,"Date"]) != 9, 0,
                                     NA)) 
data[,"Oct_dual"] <- ifelse( month(data[,"Date"]) == 10, 1,
                             ifelse( month(data[,"Date"]) != 10, 0,
                                     NA)) 
data[,"Nov_dual"] <- ifelse( month(data[,"Date"]) == 11, 1,
                             ifelse( month(data[,"Date"]) != 11, 0,
                                     NA)) 
data[,"Dec_dual"] <- ifelse( month(data[,"Date"]) == 12, 1,
                             ifelse( month(data[,"Date"]) != 12, 0,
                                     NA)) 
# We need to add sin and cos variables for seasonalitys:

colnames <- c("Date","Count","Holiday", "Dayofyear","Mon_dual", "Tues_dual", "Thu_dual","Fri_dual", "Sat_dual", "Sun_dual",
              "Jan_dual", "Feb_dual", "Mar_dual", "Apr_dual", "May_dual", "Jul_dual","Aug_dual", "Sep_dual", 
              "Oct_dual", "Nov_dual", "Dec_dual")
data <- cbind( data[ ,names(data) %in% colnames],
               sin((2*pi*data[,"Dayofyear"])/365.25),cos((2*pi*data[,"Dayofyear"])/365.25),
               sin((4*pi*data[,"Dayofyear"])/365.25),cos((4*pi*data[,"Dayofyear"])/365.25) )
colnames(data) <- c(colnames, c("sin2","cos2","sin4","cos4"))

# VERININ KENDI GRAFIGI
# GUN ETKISI BOXPLOT
# HOLIDAY GRAPH
# AY ETKISI

#--------------------------------------------------------------------
#                     2- OLS MODEL --  BOX-COX
# -------------------------------------------------------------------

traindata <- data[b:T,-which(names(data)=="Date")]
data_ts <- ts (traindata, frequency = (T-b+1), start = c (lubridate::year(train_startdate),month(train_startdate)) )
model_ts<-lm( Count ~., data=data_ts ) 
plot(residuals(model_ts))   # ERRORLAR STATIONARY DEGIL

# Implement the Box-Cox transformation

bc <- boxcox(Count ~., data=data_ts, plotit = FALSE) 
lambda <- bc$x[which.max(bc$y)]
data[ ,"TransformedCount"] <-  c()
data[ ,"TransformedCount"] <-((data[ ,"Count"] ^lambda)-1)/lambda
#plot(data[ ,"TransformedCount"])


# We can clear the Count column now and create new traindata and new data_ts with transformed values
rm(traindata)

traindata <- data[b:T,-which(names(data) %in% c("Date","Count"))]

data_ts <- ts (traindata, frequency = (T-b+1), start = c (lubridate::year(train_startdate),month(train_startdate)) )

model_ts<-lm(TransformedCount~., data=data_ts )

summary(model_ts) 
#plot(residuals(model_ts))   # ERRORLAR STATIONARY DEGIL


# Extract AR and MA coefficient outputs from the auto.arima function and call them as ar_coef and ma_coef
# ar=? ma=? 

tsdisplay(residuals(model_ts), main="ARIMA errors")
auto.arima(residuals(model_ts))$coef 

coefficients_arima <- as.data.frame(strsplit(rownames( data.frame(auto.arima(residuals(model_ts))$coef)), 
                                             "(?=[A-Za-z])(?<=[0-9])|(?=[0-9])(?<=[A-Za-z])", perl=TRUE))

if ( length (coefficients_arima[2,][coefficients_arima[1,]=="ar"] ) == 0 ) {
  ar_coef <- 0
} else {
  ar_coef <-as.numeric(max(coefficients_arima[2,][coefficients_arima[1,]=="ar"]))
}

if (length(coefficients_arima[2,][coefficients_arima[1,]=="ma"] ) == 0 ) {
  ma_coef <- 0
} else {
  ma_coef <-as.numeric(max(coefficients_arima[2,][coefficients_arima[1,]=="ma"]))
}
ar_coef 
ma_coef
#--------------------------------------------------------------------
#                     3- GLS FIT WITH ARMA ERRORS (use nmle)  -- BOX-COX
# -------------------------------------------------------------------

gls_fit_ts <- gls(TransformedCount ~ ., data=data_ts, correlation = corARMA(p=ar_coef,q=ma_coef),
                  method="ML")

tsdisplay(residuals(gls_fit_ts, type="normalized"), main="ARIMA errors")

#--------------------------------------------------------------------
#                     4- CUSUM AND SIGNAL DETECTION
# -------------------------------------------------------------------

gls_predict <-  c()

#cusum parameters:
kk <- 1    # agressive:0.5  ---  moderate:1     ----  routine:1
H <- 0.695 # agressive:0.365 --- moderate:0.695 ----  routine:1.2
win <- 7 #sliding window size
cusum_result <- data.frame()
maxlimit <- max(data[,"Count"])

while (test_startdate < test_enddate) {
  
  # elimizdeki test data sini yillik yillik cekiyoruz, taa ki test_startdate >= test_enddate olana kadar
  # test data her sene icin yeniden olusturulacak
  
  # test data list oldugu icin onu dataNum diye bir numeric'e donusturup islemlere devam ettim
  
  testdata <-data[which(data[,"Date"] >= test_startdate & data[,"Date"]< test_startdate %m+%years(1)),
                  -which(names(data)==c("Date","Count"))]
  
  # prediction icin gls model:
      
  # EGER DATA FRAME OLARAK KABUL ETMEZSE BUNU KULLAN:
  # dataNum <- matrix(data = NA, nrow = dim(testdata)[1], ncol = dim(testdata)[2])
      # colnames(dataNum) <- colnames(testdata)
      # 
      # for (i in 1:dim(testdata)[2]) {
      #   dataNum[,i] <- as.numeric(testdata[[i]])
      # }
      # gls_predict <-  predict(gls_fit_ts,as.data.frame(dataNum))[1:dim(dataNum)[1]]
      # 
  gls_predict <-  predict(gls_fit_ts,as.data.frame(testdata))[1:dim(testdata)[1]]
  
  
  #pb ve pT bu testdata'ya aldigimiz verilerin normal data'da kacinci veriye denk geldigini soyluyor
  
  pb <- as.numeric(which(data[,"Date"]==test_startdate))
  
  if (test_enddate >= test_startdate %m+% years(1)-days(1)) {
    pT <- as.numeric(which(data[,"Date"]==test_startdate %m+% years(1)-days(1)))
  } else {
    pT <- pb + as.numeric(length( which(data[,"Date"]>=test_startdate & data[,"Date"]<=test_enddate )))-1
  }
  
  # SON SENE TAM YIL DEGILSE PT DEGISECEK
  
  testperiod <- paste(lubridate::year(data[which(data[,"Date"]==test_startdate),"Date"]),"-",lubridate::year(test_startdate %m+% years(1)-days(1)))
  testdata[,"Year"] <- c(rep(testperiod,dim(testdata)[1]))
  testdata[,"Day"] <- c(1:dim(testdata)[1])
  testdata[,"CumDay"] <- c(pb:pT)
  testdata[,"s"] <- c(rep(win,dim(testdata)[1]))
  
  # inverse box-cox to transform predicted gls_predict values
 
  testdata[ ,"Predicted"] <- (lambda*gls_predict + 1)^(1/lambda) 
  testdata[ ,"Observed"] <- (lambda*testdata[ ,"TransformedCount"] + 1)^(1/lambda) 

  
  # Find error terms
  testdata[,"Residuals"] <- scale(testdata[ ,"Observed"]-testdata[ ,"Predicted"])
  
  ll <- pT-pb+1
  #resid <- testdata$Residuals
  
  # CUSUM CHARTS 
  testdata[,"Cp"]=NULL
  testdata[1,"Cp"]=0
  
  # -------------------------------------------------------------------
  #for(i in 2:ll){Cp[i]=max(0,resid[i]-kk+Cp[i-1])
  #                 if(Cp[i]<Cp[i-1]){Cp[i]=0}}
  # -------------------------------------------------------------------
  for(i in 2:ll){testdata[i,"Cp"]=max(0,testdata[i,"Residuals"]-kk+testdata[i-1,"Cp"])}
  
  for(i in (win+1):ll){
    mn=(i-win):i
    if(testdata[i,"Cp"]>H){mod=lm(testdata[mn,"Cp"]~mn)
    if(mod$coef[2]<0){testdata[i,"Cp"]=0}}}
  
  
  testdata[,"signal"]<- ifelse( testdata[,"Cp"]>H, 1,
                                   ifelse( testdata[,"Cp"]<=H, 0,
                                           NA))
  testdata[,"Cp2"]<- ifelse( testdata[,"Cp"]>H, 1.2*maxlimit,
                                   ifelse( testdata[,"Cp"]<=H, NA,
                                           NA)) 
  
  tmp <- testdata[,c("Observed", "Predicted","Year","CumDay","Day","s","Residuals","Cp","Cp2","signal")]
  
  cusum_result <- rbind(cusum_result,tmp)
  
  test_startdate <- test_startdate %m+% years(1)
  rm(testdata)
  rm(tmp)
  #rm(dataNum)
  
}
#return(cusum_result)

