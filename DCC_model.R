

# DCC-GARCH ---------------------------------------------------------------


library(rmgarch)

m.dcc.spec<- multispec(c(spec7, spec2, spec7 , spec1, spec3))
dccspec <- dccspec(m.dcc.spec, VAR = FALSE, robust = FALSE, lag = 1, lag.max = NULL, 
                   lag.criterion = "BIC", external.regressors = NULL, 
                   robust.control = list("gamma" = 0.25, "delta" = 0.01, "nc" = 10, "ns" = 500), 
                   dccOrder = c(1,1), model = "DCC", groups = rep(1, length(uspec@spec)), 
                   distribution = "mvt", start.pars = list(), fixed.pars = list()) 

dccdata <- cbind(ret_ibov, ret_comm, ret_xrate, ret_btc, ret_gold)
dccfit <- dccfit(dccspec, dccdata, out.sample = 0, solver = "solnp", solver.control = list(), 
                 fit.control = list(eval.se = TRUE, stationarity = TRUE, scale = FALSE), 
                 cluster = NULL, fit = NULL, VAR.fit = NULL, realizedVol = NULL)
show(dccfit)



#par(mfrow=c(3,1))
plot(dccfit, which=2)

str(dccfit)
slotNames(dccfit)
class(dccfit@mfit$R)

#Correlations DCC
dfr <- data.frame(matrix(unlist(dccfit@mfit$R), nrow=length(ret_ibov),byrow=T))
head(dfr)
summary(dfr)
View(dfr)
class(dfr)

Ibov_SPGSCI <- dfr[,2]
Ibov_SPGSCI
Ibov_XRate <- dfr[,3]
Ibov_BTC <- dfr[,4] 
Ibov_BTC
Ibov_gold <- dfr[,5] 
Ibov_gold

par(mfrow=c(3,1))
plot(Ibov_SPGSCI, type="l")
plot(Ibov_XRate, type="l")
plot(Ibov_BTC, type="l")
plot(Ibov_gold, type="l")

mean(Ibov_XRate)
sd(Ibov_XRate)
mean(Ibov_SPGSCI)
sd(Ibov_SPGSCI)
mean(Ibov_BTC)
sd(Ibov_BTC)
mean(Ibov_gold)


