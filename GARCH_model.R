
# Modelos GARCH univariados -----------------------------------------------

####### eGARCH(1,1) dist t
spec1 <- ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(1,1)), 
                    mean.model = list(armaOrder = c(0,0), include.mean = FALSE), distribution.model = "std") 
fit1a <- ugarchfit(spec1, ret_ibov) 
fit1b <- ugarchfit(spec1, ret_comm)
fit1c <- ugarchfit(spec1, ret_xrate)
fit1d <- ugarchfit(spec1, ret_btc)
fit1e <- ugarchfit(spec1, ret_gold)

show(fit1a)
show(fit1b)
show(fit1c)
show(fit1d)
show(fit1e)

xtable(fit1a)
stargazer(fit1a) 

####### eGARCH(1,1) dist skew t
spec2 <- ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(1,1)), 
                    mean.model = list(armaOrder = c(0,0), include.mean = FALSE), distribution.model = "sstd")
fit2a <- ugarchfit(spec2, ret_ibov) 
fit2b <- ugarchfit(spec2, ret_comm)
fit2c <- ugarchfit(spec2, ret_xrate)
fit2d <- ugarchfit(spec2, ret_btc)

show(fit2a)
show(fit2b)
show(fit2c)
show(fit2d)
show(fit2e)

####### GARCH(1,1) dist t
spec3 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)), 
                    mean.model = list(armaOrder = c(0,0), include.mean = FALSE), distribution.model = "std")

fit3a <- ugarchfit(spec3, ret_ibov) 
fit3b <- ugarchfit(spec3, ret_comm)
fit3c <- ugarchfit(spec3, ret_xrate)
fit3d <- ugarchfit(spec3, ret_btc)
fit3e <- ugarchfit(spec3, ret_gold)

show(fit3a)
show(fit3b)
show(fit3c)
show(fit3d)
show(fit3e)

####### GARCH(1,1) dist skew t
spec4 <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1)), 
                    mean.model = list(armaOrder = c(0,0), include.mean = FALSE), distribution.model = "sstd")

fit4a <- ugarchfit(spec4, ret_ibov) 
fit4b <- ugarchfit(spec4, ret_comm)
fit4c <- ugarchfit(spec4, ret_xrate)
fit4d <- ugarchfit(spec4, ret_btc)
fit4e <- ugarchfit(spec4, ret_gold)

show(fit4a)
show(fit4b)
show(fit4c)
show(fit4d)
show(fit4e)

####### gjr-GARCH(1,1) dist t
spec5 <- ugarchspec(variance.model = list(model = "gjrGARCH", garchOrder = c(1,1)), 
                    mean.model = list(armaOrder = c(0,0), include.mean = FALSE), distribution.model = "std")

fit5a <- ugarchfit(spec5, ret_ibov) 
fit5b <- ugarchfit(spec5, ret_comm)
fit5c <- ugarchfit(spec5, ret_xrate)
fit5d <- ugarchfit(spec5, ret_btc)
fit5e <- ugarchfit(spec5, ret_gold)


show(fit5a)
show(fit5b)
show(fit5c)
show(fit5d)
show(fit5e)


####### gjr-GARCH(1,1) dist skew t
spec6 <- ugarchspec(variance.model = list(model = "gjrGARCH", garchOrder = c(1,1)), 
                    mean.model = list(armaOrder = c(0,0), include.mean = FALSE), distribution.model = "sstd")

fit6a <- ugarchfit(spec6, ret_ibov) 
fit6b <- ugarchfit(spec6, ret_comm)
fit6c <- ugarchfit(spec6, ret_xrate)
fit6d <- ugarchfit(spec6, ret_btc)
fit6e <- ugarchfit(spec6, ret_gold)

show(fit6a)
show(fit6b)
show(fit6c)
show(fit6d)
show(fit6e)


####### apARCH(1,1) dist t
spec7 <- ugarchspec(variance.model = list(model = "apARCH", garchOrder = c(1,1)), 
                    mean.model = list(armaOrder = c(0,0), include.mean = FALSE), distribution.model = "std")

fit7a <- ugarchfit(spec7, ret_ibov) 
fit7b <- ugarchfit(spec7, ret_comm)
fit7c <- ugarchfit(spec7, ret_xrate)
fit7d <- ugarchfit(spec7, ret_btc)
fit7e <- ugarchfit(spec7, ret_gold)

show(fit7a)
show(fit7b)
show(fit7c)
show(fit7d)
show(fit7e)

####### apARCH(1,1) dist skew t
spec8 <- ugarchspec(variance.model = list(model = "apARCH", garchOrder = c(1,1)), 
                    mean.model = list(armaOrder = c(0,0), include.mean = FALSE), distribution.model = "sstd")

fit8a <- ugarchfit(spec8, ret_ibov) 
fit8b <- ugarchfit(spec8, ret_comm)
fit8c <- ugarchfit(spec8, ret_xrate)
fit8d <- ugarchfit(spec8, ret_btc)
fit8e <- ugarchfit(spec8, ret_gold)


show(fit8a)
show(fit8b)
show(fit8c)
show(fit8d)
show(fit8e)


