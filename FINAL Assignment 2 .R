#Clears the environment
rm(list=ls()) 
#Control + L --> clears the console
# Closes figures
graphics.off()

library (quantmod)
library (xts)
library (forecast)
library (fGarch)
library (fBasics)
#setwd("~/Desktop")
setwd("C:/Users/Arda/Downloads") 

#1data = read.csv ("Bloom_miss.csv")
#or 
data = read.csv ("Bloom_miss-1.csv", row.names = 1)

#Use if the first one does not work
#data1 = read.csv2 ("Bloom_miss.csv")

#Save the progress frequently!

#checking the data for the class
class (data)
class (data$AUDUSD)

#We need to convert the data to the time series 
#HAVING THE DATES!
#2rownames (data) = data [,1]

#Getting the read of the first column
#3data = data [,-1]

#Converting data to Time series
#First we need to convert rows from characters to DATES
#class (rownames (data))
Dates = as.Date(rownames(data), format = "%d/%m/%Y")
class (Dates)
head (Dates)

#Now we can convert our data set to time series
data.ts = as.xts(data, order.by = Dates)

#Cross-check:
class (data.ts)
#Every date is the date now! 
#data is the Bloomberg formatted
#data.ts is the proper format with the year upfront.

#1st currency - Base currency, #2nd currency Quoted currency

#If we are in the Euro zone, how much EURO we pay for 1 DOLLAR?
#Inverse exchange rate:
USDEUR = 1 / data$EURUSD

#Limiting the dates for the last year:
EX = window (data.ts, start = '2019-09-30', end = '2020-09-30')

#Question b: How many valid (non-missing and positive) observations areat NOK?
length (EX$USDNOK)
#However, some of them are omitted
#Is there any NA in USD/NOK?
sum(is.na(EX$USDNOK))
#How many positive values? ==> #Answering b
sum (EX$USDNOK>0, na.rm = T)

#Question c What are the dates of all missing data on SEK in the subsample?Omit them properly.
Dates = index (EX)
#Gives the position of missing observation:
which (is.na(EX$USDSEK))
#Finding the date with a missing observation:
Dates [which (is.na(EX$USDSEK))]
#Cross-check
sum(is.na(EX$USDSEK))
#WORKS!

#Omitting properly
SEK = na.omit (EX$USDSEK)
sum(is.na(SEK))



#TASK 2: For your subsample calculate 100*log-returns on 500 tho EUR long position in USD. 
#Fill in the sentences below (put a number). Round the results to 1 bp (base point= 0.01 pp) if necessary:

#Firstly, omitting the NA dates
EUR = na.omit (EX$EURUSD)
sum (is.na(EUR))
#Works, we can proceed.

#When long EUR/USD if we account in USD
#IF LONG VaR USD:
r = 100 * diff (log(EUR))

#If SHORT VaR in USD:
#r = - 100 * diff (log(EUR)) #(the returns will be negative)


#IF LONG VaR EUR:
#r = 100 * diff (log(1/EUR))
#If SHORT VaR in EUR:
#r = - 100 * diff (log(1/EUR))

r = r[-1]
sum (is.na (r))

#Point A: Maximum of daily squared log-returns is on .............(date) and is equal to ......pp.
max (r^2)
Dates [which.max(r)]

#alternative way
r = r[-1]
rsq = r^2
rsq [rsq==max(rsq)]

#or looking manually through the data set of rsq know the max (r^2)

#Point B: Log-returns on long EUR are normally 
#distributed at 1% significance level, report p-value.
normalTest (r, method = "jb")
#Comparing the p-value with the significance level


#Point C: Produce ACF correlogram plot for those returns(graphical file)and test if there is an autocorrelation.
Acf (r)
pacf (r)
Box.test (r, type = "Ljung-Box") 
#at 5% there is an autocorrelation
#at 1% there is no autocorrelation

#Point D:The one-day-ahead value at risk 95% 
#for a long position 100 thousands EUR on 30 SEP 2020 is according to standard (normal) method is ...... pp., 
#and according to historical simulation......pp.

#Standard (Normal) method ===>
VAR = mean (r) + qnorm (0.05) * sd(r)
#Risk metric:
#VAR = mean (r[(length(r)-249):length(r)]) + qnorm (0.05) * sd(r[(length(r)-249):length(r)])

#Historical VAR
VAR_hs = quantile (r, 0.05)
VAR_hs


#Point E: Interpret VaR 99%in dollar terms according to standard (normal) method.
#VaR in thou USD is ............, which means that

#Changing VaR from 0.05 to 0.01
# returns are in percentage points
#Results in thousands:
VAR_USD = 500 * (mean (r) + qnorm (0.01) * sd(r))/100 #we devide because we have r in pp
VAR_USD
#Result will occur in thousands

#Results in Full nominal:
#VAR_USD = 500000 * (mean (r) + qnorm (0.01) * sd(r))/100 #we devide because we have r in pp
#VAR_USD


#MATEHMATICALLY TRUE RESULTS:
#Exact because we use logarithmic returns
VAR_USD = 500 * (exp((mean (r) + qnorm (0.01) * sd(r))/100)-1)
VAR_USD

#Interpretation: e
#With 99% confidence level, we will make a lower loss than USD 5.018
#In 1% of cases, our loss will be higher than USD 5.018


#TASK 3
MA = arima (r, c(0,0,1))
AR = arima (r,c(1,0,0))
AR_ARCH = garchFit(r~arma(1,0)+garch(1,0), trace = F)
GARCH = garchFit(r~garch(1,1), trace = F)

#Looking for significance:
MA
AR
#Dividing the first row of the results, by the second row (s.e.)
#or alternative way:
params = AR$coef
stde = sqrt (diag(MA$var.coef))
tratio = params/stde
tratio

#Already gives the t-values
AR_ARCH
#omega is an intercept in the Variance equation (h_t)
#alpha1 = ARCH 1

#Already gives the t-values
GARCH
#mu - intercept not significant
#omega 
#alpha (ARCH) - how fast it reacts to news --> short term persistence
#beta1 ==> GARCH --> how much the returns are persistent ==> long term persistence

#Point 2 in the table
#Is there autocorrelation of order 10 in residuals
#Report the p-value

Box.test (MA$residuals, lag = 10, type = "Ljung-Box")
#We have autocorrelation

Box.test (AR$residuals, lag = 10, type = "Ljung-Box")
#We have autocorrelation

summary (AR_ARCH)
#Look for the LJUNG BOX TEST (Q10)
#No autocorrelation
summary (GARCH)
#Look for the LJUNG BOX TEST (Q10)
#No autocorrelation

#===> Insert p-values in the table


#Point 3 in the table
Box.test (MA$residuals^2, lag = 10, type = "Ljung-Box")
#0, there is

Box.test (AR$residuals^2, lag = 10, type = "Ljung-Box")
#0, there is

#There are ARCH effects in AR and MA, because squared residuals are autocorrelaterd ==> Box Test = LJUNG BOX

summary (AR_ARCH)
#Look for the LJUNG BOX TEST (Q10)
#0, there is
summary (GARCH)
#Look for the LJUNG BOX TEST (Q10)
#0.5261, there is no ARCH EFFECT ==> 

#Point 4 ===> Report AIC ==> We have them in Summaries
AR$aic
#AIC = 317.7988

MA$aic
#AIC = 318.9512

summary (AR_ARCH)
#AIC = 1.226476
summary (GARCH)
#AIC = 1.125236


#TASK 4
#Which of these models is
#the most parsimonious specification for returns dynamics and why?

#a. The AIC is lowest in the case of GARCH, so the GARCH model should be the most parsimonious. Because it captures volatilities and there are no ARCH effects in residuals
#Parsimonious = lowest AIC; All of the important data features are included (e.g if there are GARCH effects some of the features might not be included properly)

#b. Are exchange rate EUR/USD returns persistent?
#Explain your answer.
#The model we chosen as parsimonious (the best one) "GARCH (1,1" 
#==> there is no persistence in the returns (no correlation in residuals), only in VARIANCE

#For other models, the persistence of returns exist only due to the ARCH effects.


#c.
summary (GARCH)
#Jarque-Bera p-value is equal to 0
#Thus, our residuals are not normally distributed.

#d: What is the forecasted standard deviation of returns for 10 days ahead 
#using EWMA method.
EWMA = 1
T = length (r)
for (i in 1:T){
  EWMA [i+1] = 0.95 * EWMA [i] + 0.05 * r[i]^2
}

#We are interested in the last one
EWMA [T]

#We are interested in the next 10 days
sqrt (10*EWMA[T])
#1.205643





