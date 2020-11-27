# To clear the workspace
rm(list=ls())
# To download libraries
library(quantmod)
#library(moments)
library(fBasics)

# list of valid tickers
tickers = c("AAL","BA","MA","SA","^DJI")
# downloading on-line data with quantmod API library
getSymbols(tickers,from = "2019-01-31", to = "2020-09-30")

Market_close = DJI$DJI.Close

# Calculate simple returns on market
Rm =  Market_close/lag(Market_close,1)-1
data.frame(cbind(Market_close, lag(Market_close,1), Rm))
# Calculate log returns on market
rm =  diff(log(Market_close))
data.frame(cbind(Market_close, Rm, rm))

# display length
print(paste("DJIA has:", length(rm), " observations"))
# Skewness of market log-returns
skewness(rm,na.rm = T)
# Excess kurtosis of market log-returns
kurtosis(rm,na.rm = T)
# Normality test (Shapiro-Wilk)
normalTest(rm,method = "jb",na.rm = T)
# Histogram of returns with 36 bins
hist(rm, breaks = 35)

results = array(NaN,c(4,4))
colnames(results) = tickers[1:4]
rownames(results) = c("positive ret", "sd","skewness", "excess kurtosis")

# results for bullet 9
results_tab = array(NaN,c(4,4))
colnames(results_tab) = tickers[1:4]
rownames(results_tab) = c("beta", "tratio","pvalue>0.05", "if significant")

for (i in 1:4){
  # get() use character as variable name
  price = get(tickers[i])
  # choose close prices only
  close_price = price[,4]
  #close_price = price[,sprintf(paste(i,".Close",sep = ""))]
  # display
  print(paste(tickers[i],":" ,length(close_price)))
  R = close_price/lag(close_price,1)-1
  r = diff(log(close_price))
  print("Positive returns:")
  print(sum(r>0,na.rm = T))
  #TOTAL DAYS - POSITIVE DAYS =  NEGATIVE DAYS
  results[1,i] = sum(r>0,na.rm = T)
  print("sd:")
  print(100*sd(r,na.rm = T))
  results[2,i] = 100*sd(r,na.rm = T)
  print("skew:")
  print(skewness(r,na.rm=T))
  results[3,i] = skewness(r,na.rm=T)
  print("kurt:")
  print(kurtosis(r,na.rm=T))
  results[4,i] = kurtosis(r,na.rm=T)
  # Point 9 in class assignment
  OLS = lm(r~rm)
  beta = OLS$coefficients[2]
  results_tab[1,i]=beta 
  tvalue = summary(OLS)$coeff[6]
  results_tab[2,i]=tvalue 
  pvalue = summary(OLS)$coeff[8]
  if (pvalue>0.01){
    results_tab[3,i] = pvalue
    results_tab[4,i] = F}
  else{
    results_tab[4,i] = T}
  
  print("OLS results:")
  sprintf(c(beta, tvalue, pvalue),fmt='%.2f')
  if (tickers[i] == "AAL"){
    n = length(close_price)
    print("annualized sd of returns on AAL is")
    print(100*sqrt(250)*sd(r,na.rm = T))
    print("average simple return on AAL is")
    n = length(close_price)
    print(100*((t(close_price[n])/t(close_price[1]))^(1/(n-1))-1))
    print("which in annual terms is:")
    print(100*((t(close_price[n])/t(close_price[1]))^(250/(n-1))-1))
  }
  if (tickers[i] == "BA"){
    n = length(close_price)
    print("annualized sd of returns on AAL is")
    print(100*sqrt(250)*sd(r,na.rm = T))
    print("average simple return on AAL is")
    n = length(close_price)
    print(100*((t(close_price[n])/t(close_price[1]))^(1/(n-1))-1))
    print("which in annual terms is:")
    print(100*((t(close_price[n])/t(close_price[1]))^(250/(n-1))-1))
  }
  if (tickers[i] == "MA"){
    n = length(close_price)
    print("annualized sd of returns on AAL is")
    print(100*sqrt(250)*sd(r,na.rm = T))
    print("average simple return on AAL is")
    n = length(close_price)
    print(100*((t(close_price[n])/t(close_price[1]))^(1/(n-1))-1))
    print("which in annual terms is:")
    print(100*((t(close_price[n])/t(close_price[1]))^(250/(n-1))-1))
  }
  if (tickers[i] == "SA"){
    n = length(close_price)
    print("annualized sd of returns on AAL is")
    print(100*sqrt(250)*sd(r,na.rm = T))
    print("average simple return on AAL is")
    n = length(close_price)
    print(100*((t(close_price[n])/t(close_price[1]))^(1/(n-1))-1))
    print("which in annual terms is:")
    print(100*((t(close_price[n])/t(close_price[1]))^(250/(n-1))-1))
  }
  
  if (tickers[i] == "BA"){
    print("R-squared of BA is")
    print(100*summary(OLS)$r.squared)
  }
  if (tickers[i] == "MA"){
    print("R-squared of MA is")
    print(100*summary(OLS)$sigma)
  }
  if (tickers[i] == "TA"){
    print("Beta of TA in base-point units is")
    print(100*OLS$coefficients[2])
  }
  print("----------------------------------")
}


for (i in 1:4){
  # get() use character as variable name
  price = get(tickers[i])
  # choose close prices only
  close_price = price[,4]
  #close_price = price[,sprintf(paste(i,".Close",sep = ""))]
  # display
  print(paste(tickers[i],
              " has length:" ,
              length(close_price)))
  r = diff(log(close_price))
  print("Positive returns:")
  print(sum(r>0,na.rm = T))
  print("sd:")
  print(sd(r,na.rm = T))
  print("skew:")
  print(skewness(r,na.rm=T))
  print("kurt:")
  print(kurtosis(r,na.rm=T))
  print("----------------------------------")
}


for (i in tickers[-length(tickers)]){
  pvalue = summary(OLS)$coeff[8]
  if (pvalue>0.01){
    results_tab[3,i] = pvalue
    results_tab[4,i] = F}
  else{
    results_tab[4,i] = T}
  # get() use character as variable name
  price = get(i)
  # choose close prices only
  close_price = price[,4]
  r = diff(log(close_price))
  # Point 9 in class assignment
  OLS = lm(r~rm)
  beta = OLS$coefficients[2]
  tvalue = summary(OLS)$coeff[6]
  pvalue = summary(OLS)$coeff[8]
  print("OLS results:")
  print(sprintf(c(beta, tvalue, pvalue),fmt='%.2f'))
  print("----------------------------------")
}

for (i in tickers[-length(tickers)]){
  # get() use character as variable name
  price = get(i)
  # choose close prices only
  close_price = price[,4]
  #close_price = price[,sprintf(paste(i,".Close",sep = ""))]
  r = diff(log(close_price))
  # Point 9 in class assignment
  OLS = lm(r~rm)
  print(i)
  print("OLS results:")
  print(summary(OLS))
  print("----------------------------------")
}