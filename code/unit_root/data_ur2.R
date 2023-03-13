
# testing stationarity

rm(list = ls())
require(zoo)
require(ggfortify)
require(lubridate)
require(urca)
require(knitr)
require(stringr)
require(data.table)
require(dynlm)
require(lmtest)
# load data

input_dir  = "C:/users/Piotr/Dropbox/WNE/ASC_2023/data/"
agg_data     = fread(paste0(input_dir,"/agg_data.csv"),header = TRUE)



forward_rate_list = c(paste0("TIPSF0",2:9),paste0("TIPSF",10:20))
hqm_TD_spread_list = paste0("hqm_TD_spread",c(2,5,10,20))


agg_data[, leverage := liquid_effective_leverage_agg]
agg_data = subset(agg_data,select = c("date","real_rate_1year",forward_rate_list,"CP_TB_spread",hqm_TD_spread_list,"leverage"))


agg_data      = na.omit(agg_data)
agg_data      = agg_data[order(date)]


# some plots
temp = melt(agg_data[,c("date","leverage")],id.vars="date")
ggplot()+
  geom_point(data = temp, aes(x=date, y= value, color=variable))+
  geom_line(data = temp, aes(x=date, y= value, color=variable))


y = agg_data$leverage
dy   = diff(y)

# plot acf and pacf
acf(coredata(y), lag.max = 12, plot = TRUE)
pacf(coredata(y), lag.max = 12, plot = TRUE)



# Test ADF
########################################
summary(ur.df(y, type="none", lags=4))
summary(ur.df(y, type="drift", lags=4))
summary(ur.df(y, type="trend", lags=4))
summary(ur.df(dy, type="drift", lags=4))

# BG test - ADF
reg_df = dynlm(zoo(dy) ~ L(zoo(y),1) - 1) 
reg_df = dynlm(zoo(dy) ~ lag(zoo(y),-1) - 1) 

summary(reg_df)
resid  = zoo(resid(reg_df), agg_data$date)

acf(coredata(resid), lag.max = 12, plot = TRUE)

acf(coredata(resid), lag.max = 12, plot = TRUE)
pacf(coredata(resid), lag.max = 12, plot = TRUE)

bgtest(reg_df,order = 1)
bgtest(reg_df,order = 2)
bgtest(reg_df,order = 3)
bgtest(reg_df,order = 4)

# Test KPSS
########################################

summary(ur.kpss(y, type = "tau",lags = "short"))
summary(ur.kpss(dy, type = "mu",lags = "short"))


