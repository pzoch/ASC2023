require(zoo)
require(eurostat)
require(ggfortify)
require(forecast)
require(smooth)
require(pracma)
require(TSstudio)

tempY_SA <- get_eurostat("namq_10_gdp",
                         filters = list(geo="PL", 
                                        na_item = "B1G",
                                        unit    = "CP_MNAC",
                                        s_adj   = "SCA"))

tempY_NS <- get_eurostat("namq_10_gdp",
                         filters = list(geo="PL", 
                                        na_item = "B1G",
                                        unit    = "CP_MNAC",
                                        s_adj   = "NSA"))

logY_SA  <- na.omit(ts(log(tempY_SA$values)))
logY_NS  <- na.omit(ts(log(tempY_NS$values)))

logY_SA <- ts(logY_SA,start = c(1995, 1), frequency =4)
logY_NS <- ts(logY_NS,start = c(1995, 1), frequency =4)

dlogY_SA <- (diff(logY_SA, lag = 1) * 100) # growth rate
dlogY_NS <- (diff(logY_NS, lag = 1) * 100) # growth rate



# SES - do it on differences first
data_validation.dSA <- window(dlogY_SA, start = c(2015, 1))
data_training.dSA   <- window(dlogY_SA, end = c(2014, 4))

data_validation.dNS <- window(dlogY_NS, start = c(2015, 1))
data_training.dNS   <- window(dlogY_NS, end = c(2014, 4))

GDP_forecast.dSA <- ses((data_training.dSA), h=length(data_validation.dSA),alpha=0.3)
GDP_forecast.dNS <- ses((data_training.dNS), h=length(data_validation.dNS),alpha=0.3)

autoplot(GDP_forecast.dSA) +
  autolayer(fitted(GDP_forecast.dSA), series="Fitted - SA") +
  autolayer(data_validation.dSA, series="Data - validation") +
  ylab("dlog GDP") + xlab("Year")

autoplot(GDP_forecast.dNS) +
  autolayer(fitted(GDP_forecast.dNS), series="Fitted - NS") +
  autolayer(data_validation.dNS, series="Data - validation") +
  ylab("log GDP") + xlab("Year")


# SES - do it on levels now
data_validation.lSA <- window(logY_SA, start = c(2015, 1))
data_training.lSA   <- window(logY_SA, end = c(2014, 4))

data_validation.lNS <- window(logY_NS, start = c(2015, 1))
data_training.lNS   <- window(logY_NS, end = c(2014, 4))

GDP_forecast.lSA <- ses((data_training.lSA), h=length(data_validation.lSA),alpha=0.3)
GDP_forecast.lNS <- ses((data_training.lNS), h=length(data_validation.lNS),alpha=0.3)

autoplot(GDP_forecast.lSA) +
  autolayer(fitted(GDP_forecast.lSA), series="Fitted - SA") +
  autolayer(data_validation.lSA, series="Data - validation") +
  ylab("log GDP") + xlab("Year")

autoplot(GDP_forecast.lNS) +
  autolayer(fitted(GDP_forecast.lNS), series="Fitted - NS") +
  autolayer(data_validation.lNS, series="Data - validation") +
  ylab("log GDP") + xlab("Year")

# Holt - do it on levels
data_validation.hlSA <- window(logY_SA, start = c(2015, 1))
data_training.hlSA   <- window(logY_SA, end = c(2014, 4))

data_validation.hlNS <- window(logY_NS, start = c(2015, 1))
data_training.hlNS   <- window(logY_NS, end = c(2014, 4))

GDP_forecast.hlSA <- holt((data_training.hlSA), h=length(data_validation.hlSA),alpha=0.3,beta=0.3)
GDP_forecast.hlNS <- holt((data_training.hlNS), h=length(data_validation.hlNS),alpha=0.3,beta=0.3)

autoplot(GDP_forecast.hlSA) +
  autolayer(fitted(GDP_forecast.hlSA), series="Fitted - SA") +
  autolayer(data_validation.hlSA, series="Data - validation") +
  ylab("log GDP") + xlab("Year")

autoplot(GDP_forecast.hlNS) +
  autolayer(fitted(GDP_forecast.hlNS), series="Fitted - NS") +
  autolayer(data_validation.hlNS, series="Data - validation") +
  ylab("log GDP") + xlab("Year")

## play a bit with beta to see what happens!

# Holt-Winters - do it on levels
data_validation.hlSA <- window(logY_SA, start = c(2015, 1))
data_training.hlSA   <- window(logY_SA, end = c(2014, 4))

data_validation.hlNS <- window(logY_NS, start = c(2015, 1))
data_training.hlNS   <- window(logY_NS, end = c(2014, 4))

GDP_forecast.hlSA <- hw((data_training.hlSA), h=length(data_validation.hlSA),alpha=0.3,beta=0.3,gamma=0.1,seasonal="additive")
GDP_forecast.hlNS <- hw((data_training.hlNS), h=length(data_validation.hlNS),alpha=0.3,beta=0.3,gamma=0.1,seasonal="additive")


##nb: use HW for decomposition
plot(GDP_forecast.hlNS$model$states)
###


autoplot(GDP_forecast.hlSA) +
  autolayer(fitted(GDP_forecast.hlSA), series="Fitted - SA") +
  autolayer(data_validation.hlSA, series="Data - validation") +
  ylab("log GDP") + xlab("Year")

autoplot(GDP_forecast.hlNS) +
  autolayer(fitted(GDP_forecast.hlNS), series="Fitted - NS") +
  autolayer(data_validation.hlNS, series="Data - validation") +
  ylab("log GDP") + xlab("Year")

## play a bit with parameters to see what happens!

GDP_forecast.a01b01g05 <- hw((data_training.hlNS), h=length(data_validation.hlNS),alpha=0.1,beta=0.1,gamma=0.5,seasonal="additive")
GDP_forecast.a01b01g01 <- hw((data_training.hlNS), h=length(data_validation.hlNS),alpha=0.1,beta=0.1,gamma=0.1,seasonal="additive")
GDP_forecast.a02b02g02 <- hw((data_training.hlNS), h=length(data_validation.hlNS),alpha=0.2,beta=0.2,gamma=0.5,seasonal="additive")
GDP_forecast.opt <- hw((data_training.hlNS), h=length(data_validation.hlNS),seasonal="additive")


autoplot(GDP_forecast.a01b01g05) +
  autolayer(fitted(GDP_forecast.a01b01g05), series="Fitted - NS") +
  autolayer(data_validation.hlNS, series="Data - validation") +
  ylab("log GDP") + xlab("Year")

autoplot(GDP_forecast.a01b01g01) +
  autolayer(fitted(GDP_forecast.a01b01g01), series="Fitted - NS") +
  autolayer(data_validation.hlNS, series="Data - validation") +
  ylab("log GDP") + xlab("Year")

autoplot(GDP_forecast.a02b02g02) +
  autolayer(fitted(GDP_forecast.a02b02g02), series="Fitted - NS") +
  autolayer(data_validation.hlNS, series="Data - validation") +
  ylab("log GDP") + xlab("Year")

autoplot(GDP_forecast.opt) +
  autolayer(fitted(GDP_forecast.opt), series="Fitted - NS") +
  autolayer(data_validation.hlNS, series="Data - validation") +
  ylab("log GDP") + xlab("Year")
