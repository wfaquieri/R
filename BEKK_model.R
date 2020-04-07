
# BEKK model --------------------------------------------------------------

#data1 <- data.frame(ibovindex, spgsciindex, xrate, BTC, gold, oil ) #df
data2 <- data.frame(ibovindex, BTC) #Dataset2
head(data2)

#Returns
data.ret <- returns(as.ts(data2))
dataret1 <- data.frame(data.ret)
dataret1 <- na.omit(data.ret)
head(dataret1)
ret_ibov <- dataret1[,1]
ret_comm <- dataret1[,2]
ret_xrate <- dataret1[,2]
ret_btc <- dataret1[,2]
ret_gold <- dataret1[,2]

install.packages("mgarchBEKK")
install.packages("devtools")

## Load the library:
library(mgarchBEKK)
spec.bekk.1 <- BEKK(dataret1, order = c(1, 1), params = NULL, fixed = NULL, method = "BFGS",
                    verbose = F)
diagnoseBEKK(spec.bekk.1)
str(spec.bekk.1)
class(spec.bekk.1$cor)
View(spec.bekk.1$cor)
dfr2 <- data.frame(matrix(unlist(spec.bekk.1$cor), ncol=length(ret_ibov),byrow=T))
head(dfr2)
View(dfr2)
summary(dfr2)

#correla??o entre a s?rie do Ibovespa e o ind?cie de commodities S&P GSCI
Ibov_SPGSCI_bekk <- dfr2[2,2:2083]
mean_corr_Ibov_SPGSCI <- sum(Ibov_SPGSCI_bekk)/length(Ibov_SPGSCI_bekk)
mean_corr_Ibov_SPGSCI
# mean_corr_Ibov_SPGSCI  #0.3961547


#correla??o entre a s?rie do Ibovespa e a taxa de c?mbio
Ibov_XRate_bekk <- dfr2[2,2:2083]
mean_corr_ibov_XRate <- sum(Ibov_XRate_bekk)/length(Ibov_XRate_bekk)
#mean_corr_ibov_XRate -0.6975717

#correla??o entre a s?rie do Ibovespa e Bitcoin
Ibov_BTC_bekk <- dfr2[2,2:2083]
mean_corr_ibov_BTC <- sum(Ibov_BTC_bekk)/length(Ibov_BTC_bekk)
#mean_corr_ibov_BTC 0.04058474

#correla??o entre a s?rie do Ibovespa e gOLD
Ibov_gold_bekk <- dfr2[2,2:2083]
mean_corr_ibov_gold <- sum(Ibov_gold_bekk)/length(Ibov_gold_bekk)
#mean_corr_ibov_gold 0.1497346

Ibov_SPGSCI <- bekkcorr[,2]
Ibov_XRate <- bekkcorr[,3]
Ibov_BTC <- bekkcorr[4] 

plot(spec.bekk.1$cor[[1:2]], type="l")
plot(spec.bekk.1$cor[[1:3]], type="l")
plot(spec.bekk.1$cor[[1:4]], type="l")
mean(na.omit(spec.bekk.1$cor[[,2]]))
mean(na.omit(spec.bekk.1$cor[[1:3]]))
mean(na.omit(spec.bekk.1$cor[[1:4]]))





















######residuos     ######Nao necessita
residuals(fit1,standardize=TRUE) 
resdi<-as.numeric(residuals(fit1,standardize=TRUE)) 

acf(resdi, lag(30))
pacf(resdi, lag=30) 
par(mfrow=c(2,1));
qqnorm(resdi, main = "Q-Q Plot", xlab = "Quantis dos res?duos", 
       ylab = "Quantis da GED", plot.it = TRUE, datax = FALSE); qqline(resdi, col="red")

residuals(fit2,standardize=TRUE) 
resdi2<-as.numeric(residuals(fit2,standardize=TRUE))

residuals(fit3,standardize=TRUE) 
resdi3<-as.numeric(residuals(fit3,standardize=TRUE))

library(forecast) 

Box.test(resdi^2, lag = 9, type = "Ljung-Box", fitdf = 2)
Box.test(resdi^2, lag = 15, type = "Ljung-Box", fitdf = 2)
Box.test(resdi^2, lag = 20, type = "Ljung-Box", fitdf = 2)

Box.test(resdi2^2, lag = 9, type = "Ljung-Box", fitdf = 2)
Box.test(resdi2^2, lag = 15, type = "Ljung-Box", fitdf = 2)
Box.test(resdi2^2, lag = 20, type = "Ljung-Box", fitdf = 2)

Box.test(resdi3^2, lag = 9, type = "Ljung-Box", fitdf = 2)
Box.test(resdi3^2, lag = 15, type = "Ljung-Box", fitdf = 2)
Box.test(resdi3^2, lag = 20, type = "Ljung-Box", fitdf = 2)

# ARCH-LM teste
library(FinTS)
ArchTest(resdi^2, lags = 7, demean = FALSE)
ArchTest(resdi2^2, lags = 7, demean = FALSE)
ArchTest(resdi3^2, lags = 7, demean = FALSE)


#plotting the results
library(timeSeries)
