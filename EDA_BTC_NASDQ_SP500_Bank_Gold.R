########################################
# This script produces the plots about the EDA of returns SP500, NASDQ, BTC, Gold, BANKS
#######################################



#########
# Plot BTC EDA
#########
setEPS()
postscript("EDA_BTC.eps")

par(mfrow=c(2,2))
qqnorm(SK_Market_returns$log_adjclose_BTC_USD,
       main='QQplot BTC returns')
qqline(SK_Market_returns$log_adjclose_BTC_USD)
plot(density(SK_Market_returns$log_adjclose_BTC_USD[!is.na(SK_Market_returns$log_adjclose_BTC_USD)]),
     main='Kernel Density BTC returns')
acf(SK_Market_returns$log_adjclose_BTC_USD[!is.na(SK_Market_returns$log_adjclose_BTC_USD)], main=' Autocorrelation function BTC')
pacf(SK_Market_returns$log_adjclose_BTC_USD[!is.na(SK_Market_returns$log_adjclose_BTC_USD)], main='Partial Autocorr. function BTC')


dev.off()


#############
# Plot SP500 EDA
############
setEPS()
postscript("EDA_SP500.eps")
par(mfrow=c(2,2))
qqnorm(SK_Market_returns$log_adjclose__GSPC,
       main='QQplot SP500 returns')
qqline(SK_Market_returns$log_adjclose__GSPC)
plot(density(SK_Market_returns$log_adjclose__GSPC[!is.na(SK_Market_returns$log_adjclose__GSPC)]),
     main='Kernel Density SP500 returns')
acf(SK_Market_returns$log_adjclose__GSPC[!is.na(SK_Market_returns$log_adjclose__GSPC)], main=' Autocorrelation function SP500')
pacf(SK_Market_returns$log_adjclose__GSPC[!is.na(SK_Market_returns$log_adjclose__GSPC)], main='Partial Autocorr.  function SP500')

dev.off()


############
# Plot NASDAQ
############
setEPS()
postscript("EDA_NASDAQ.eps")
par(mfrow=c(2,2))

qqnorm(SK_Market_returns$log_adjclose__IXIC,
       main='QQplot NASDAQ returns')
qqline(SK_Market_returns$log_adjclose__IXIC)
plot(density(SK_Market_returns$log_adjclose__IXIC[!is.na(SK_Market_returns$log_adjclose__IXIC)]),
     main='Kernel Density NASDAQ returns')
acf(SK_Market_returns$log_adjclose__IXIC[!is.na(SK_Market_returns$log_adjclose__IXIC)], main=' Autocorrelation function NASDAQ')
pacf(SK_Market_returns$log_adjclose__IXIC[!is.na(SK_Market_returns$log_adjclose__IXIC)], main='Partial Autocorr.  function NASDAQ')
dev.off()



############
# Plot GOLD
############
setEPS()
postscript("EDA_GOLD.eps")
par(mfrow=c(2,2))
qqnorm(SK_Market_returns$log_adjclose_PAXG_USD,
       main='QQplot GOLD returns')
qqline(SK_Market_returns$log_adjclose_PAXG_USD)
plot(density(SK_Market_returns$log_adjclose_PAXG_USD[!is.na(SK_Market_returns$log_adjclose_PAXG_USD)]),
     main='Kernel Density GOLD returns')
acf(SK_Market_returns$log_adjclose_PAXG_USD[!is.na(SK_Market_returns$log_adjclose_PAXG_USD)], main=' Autocorrelation function GOLD')
pacf(SK_Market_returns$log_adjclose_PAXG_USD[!is.na(SK_Market_returns$log_adjclose_PAXG_USD)], main='Partial Autocorr.  function GOLD')
dev.off()

############
# Plot BANKS
############
setEPS()
postscript("EDA_BANKS.eps")
par(mfrow=c(2,2))
qqnorm(SK_Market_returns$log_av_banks,
       main='QQplot Banks index returns')
qqline(SK_Market_returns$log_av_banks)
plot(density(SK_Market_returns$log_av_banks[!is.na(SK_Market_returns$log_av_banks)]),
     main='Kernel Density Banks index returns')
acf(SK_Market_returns$log_av_banks[!is.na(SK_Market_returns$log_av_banks)], main=' Autocorrelation function Banks')
pacf(SK_Market_returns$log_av_banks[!is.na(SK_Market_returns$log_av_banks)], main='Partial Autocorr.  function Banks')
dev.off()




####################
#Plot volatility comparison 
####################
setEPS()
postscript("VolatilityComparison.eps")
periodOfInterest <- 1:378
plot(Variance_BTC[1:378], type='l', x=SK_Market_returns$period[periodOfInterest] ,ylim=plotLimit, xlab='t', ylab=yl, main='Volatility comparison')
points(Variance_SP500, type='l', x=SK_Market_returns$period[periodOfInterest] ,col='purple')
points(Variance_NADQ, type='l', x=SK_Market_returns$period[periodOfInterest] ,col='blue')
points(Variance_Gold, type='l', x=SK_Market_returns$period[periodOfInterest] ,col='green')
points(Variance_Banks, type='l', x=SK_Market_returns$period[periodOfInterest] ,col='orange')
#Variance_Banks
abline(v=SK_Market_returns$period[periodOfInterest][Bank_crisis_Start], col='red', lty=4)
#legend(x=SK_Market_returns$period[periodOfInterest][4] , y=9.6, legend=c("BTC", "SP500", 'NASDQ', 'Gold', 'Banks'), # legenda non funziona
#      col=c('black',"red", "blue", 'green','orange'), lty=rep(1,4), cex=1)
legend('topleft', legend=c("BTC", "SP500", 'NASDQ', 'Gold', 'Banks'), # legenda non funziona
       col=c('black',"purple", "blue", 'green','orange'), lty=rep(1,4), cex=0.60)
text(x=SK_Market_returns$period[Bank_crisis_Start-35], 
     y=10, "09March2023", col='red', cex = 0.68)
dev.off()





#####################
# Plot adjusted price comparison
#####################
setEPS()
postscript("Adjustedclose_Comparison.eps")

rt_BTC  <-   scale(SK_Market_returns$adjclose_BTC_USD)
rt_SP500<- scale(SK_Market_returns$adjclose__GSPC )
rt_NSDQ <-  scale(SK_Market_returns$adjclose__IXIC)
rt_Gold <- scale(SK_Market_returns$adjclose_PAXG_USD)
rt_back <-scale(SK_Market_returns$av_banks)

maxlimy <- max(rt_BTC,rt_SP500,rt_NSDQ, rt_Gold, rt_back)
minlimy <- min(rt_BTC,rt_SP500,rt_NSDQ, rt_Gold, rt_back)


yl <- expression(r[t])
plot(rt_BTC[1:378], type='l', x=SK_Market_returns$period[periodOfInterest], ylim=c(minlimy, maxlimy ), ylab=yl, xlab='t' , main='Ajusted price comparison')
points(rt_SP500[1:378], type='l', x=SK_Market_returns$period[periodOfInterest] ,col='purple')
points(rt_NSDQ[1:378], type='l', x=SK_Market_returns$period[periodOfInterest] ,col='blue')
points(rt_Gold[1:378], type='l', x=SK_Market_returns$period[periodOfInterest] ,col='green')
points(rt_back[1:378], type='l', x=SK_Market_returns$period[periodOfInterest] ,col='orange')
abline(v=SK_Market_returns$period[periodOfInterest][Bank_crisis_Start], col='red', , lty=4)
legend('topright', legend=c("BTC", "SP500", 'NASDQ', 'Gold', 'Banks'), # legenda non funziona
       col=c('black',"purple", "blue", 'green','orange'), lty=rep(1,4), cex=0.50)
text(x=SK_Market_returns$period[Bank_crisis_Start-50], 
     y=3, "09March2023", col='red', cex = 0.68)
dev.off()


#####################
#Plot Returns comparison 
####################
setEPS()
postscript("returns_Comparison.eps")

rt_BTC  <-   SK_Market_returns$log_adjclose_BTC_USD[-1]
rt_SP500<-   SK_Market_returns$log_adjclose__GSPC[-1]
rt_NSDQ <-   SK_Market_returns$log_adjclose__IXIC[-1]
rt_Gold <-   SK_Market_returns$log_adjclose_PAXG_USD[-1]
rt_back <-   SK_Market_returns$log_av_banks[-1]

maxlimy <- max(rt_BTC,rt_SP500,rt_NSDQ, rt_Gold, rt_back)
minlimy <- min(rt_BTC,rt_SP500,rt_NSDQ, rt_Gold, rt_back)

periodOfInterest <- 1:378
yl <- expression(r[t])
plot(rt_BTC[periodOfInterest], type='l', x=SK_Market_returns$period[periodOfInterest], ylim=c(minlimy, maxlimy ), ylab=yl, xlab='t', main=' Log returns comparison' )
points(rt_SP500[periodOfInterest], type='l', x=SK_Market_returns$period[periodOfInterest] ,col='purple')
points(rt_NSDQ[periodOfInterest], type='l', x=SK_Market_returns$period[periodOfInterest] ,col='blue')
points(rt_Gold[periodOfInterest], type='l', x=SK_Market_returns$period[periodOfInterest] ,col='green')
points(rt_back[periodOfInterest], type='l', x=SK_Market_returns$period[periodOfInterest] ,col='orange')
#Variance_Banks
abline(v=SK_Market_returns$period[periodOfInterest][297], col='red', lty=4)
legend('topleft', legend=c("BTC", "SP500", 'NASDQ', 'Gold', 'Banks'), # legenda non funziona
       col=c('black',"purple", "blue", 'green','orange'), lty=rep(1,4), cex=0.7)
text(x=SK_Market_returns$period[Bank_crisis_Start-35], 
     y=maxlimy, "09March2023", col='red', cex = 0.68)
dev.off()


#####################
#Plot returns squared
####################
setEPS()
postscript("proxyvolatility.eps")
rt_BTC  <-   SK_Market_returns$log_adjclose_BTC_USD[-1]^2
rt_SP500<-   SK_Market_returns$log_adjclose__GSPC[-1]^2
rt_NSDQ <-   SK_Market_returns$log_adjclose__IXIC[-1]^2
rt_Gold <-   SK_Market_returns$log_adjclose_PAXG_USD[-1]^2
rt_back <-   SK_Market_returns$log_av_banks[-1]^2


periodOfInterest <- 1:378

par(mfrow=c(2,3))

plot(rt_BTC[periodOfInterest], type='l', x=SK_Market_returns$period[periodOfInterest], ylab=yl, xlab='t', main='BTC' )
plot(rt_SP500[periodOfInterest], type='l', x=SK_Market_returns$period[periodOfInterest],  ylab=yl, xlab='t', main=' SP500' )
plot(rt_NSDQ[periodOfInterest], type='l', x=SK_Market_returns$period[periodOfInterest],  ylab=yl, xlab='t', main=' NASDAQ' )
plot(rt_Gold[periodOfInterest], type='l', x=SK_Market_returns$period[periodOfInterest],  ylab=yl, xlab='t', main=' GOLD' )
plot(rt_back[periodOfInterest], type='l', x=SK_Market_returns$period[periodOfInterest],  ylab=yl, xlab='t', main=' BankIndex' )

dev.off()

#####################
#Plot Returns comparison Focus
####################
setEPS()
postscript("returns_Comparison_focus.eps")

rt_BTC  <-   SK_Market_returns$log_adjclose_BTC_USD[-1]
rt_SP500<-   SK_Market_returns$log_adjclose__GSPC[-1]
rt_NSDQ <-   SK_Market_returns$log_adjclose__IXIC[-1]
rt_Gold <-   SK_Market_returns$log_adjclose_PAXG_USD[-1]
rt_back <-   SK_Market_returns$log_av_banks[-1]

periodOfInterest <- (297-30):(30+297)
maxlimy <- max(rt_BTC,rt_SP500[periodOfInterest],rt_NSDQ[periodOfInterest], rt_Gold[periodOfInterest], rt_back[periodOfInterest])
minlimy <- min(rt_BTC[periodOfInterest],rt_SP500[periodOfInterest],rt_NSDQ[periodOfInterest], rt_Gold[periodOfInterest], rt_back[periodOfInterest])



yl <- expression(r[t])
plot(rt_BTC[periodOfInterest], type='l', x=SK_Market_returns$period[periodOfInterest], ylim=c(minlimy, maxlimy ), ylab=yl, xlab='t', main=' log returns over 25/01/2023 to 21/04/2023' )
points(rt_SP500[periodOfInterest], type='l', x=SK_Market_returns$period[periodOfInterest] ,col='purple')
points(rt_NSDQ[periodOfInterest], type='l', x=SK_Market_returns$period[periodOfInterest] ,col='blue')
points(rt_Gold[periodOfInterest], type='l', x=SK_Market_returns$period[periodOfInterest] ,col='green')
points(rt_back[periodOfInterest], type='l', x=SK_Market_returns$period[periodOfInterest] ,col='orange')
#Variance_Banks
abline(v=SK_Market_returns$period[periodOfInterest][30], col='red', lty=4)
legend('topleft', legend=c("BTC", "SP500", 'NASDQ', 'Gold', 'Banks'), # legenda non funziona
       col=c('black',"purple", "blue", 'green','orange'), lty=rep(1,4), cex=0.7)
text(x=SK_Market_returns$period[297-9], 
     y=maxlimy, "09March2023", col='red', cex = 0.68)
dev.off()
