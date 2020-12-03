rm(list=ls())
# To download libraries
library(quantmod)
#library(moments)
library(fBasics)
library(tidyquant)
library(corrgram)

options("getSymbols.warning4.0"=FALSE)
options("getSymbols.yahoo.warning"=FALSE)
# Downloading Apple price using quantmod

getSymbols("YKBNK.IS", from = '2019-01-01',
           to = "2020-12-01",warnings = FALSE,
           auto.assign = TRUE)
head(YKBNK.IS)

market_close = YKBNK.IS$YKBNK.IS.Close
simple_RM = market_close/lag(market_close,1)-1
data.frame(cbind(market_close, lag(market_close,1), simple_RM))

log_RM = diff(log(market_close))
total_df = data.frame(cbind(market_close, simple_RM, log_RM))
log_df = data.frame(cbind(log_RM))

# 1. sorunun cevabý 
print(paste("YKBNK has:", length(log_RM), " observations"))

# 2. soru ilk grafik - time series
chart_Series(YKBNK.IS$YKBNK.IS.Close)

market_df = data.frame((cbind(market_close)))
final_df <- as.data.frame(t(market_df))

# 2. soru ikinci grafik - acf grafiði
corrgram(total_df, order=TRUE,  lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt, main="YKBNK CLOSE / Simple_RM / Log_ RM")

# 3. soru acf grafiði ÇALIÞMADI !!!! 
corrgram(x, lag.max = 12, type = "correlation", mode = "simple",
         ci = 0.95, style = "plotly", knit = F)

# 3. soru pacf grafiði ÇALIÞMADI !!!! 
corrgram(x, lag.max = 12, type = "partial", mode = "simple",
         ci = 0.95, style = "plotly", knit = F)

# 3. soru time series grafiði
plot(timeSeries(log_RM))

# 3. soru historgram
hist(timeSeries(log_RM))
