##Carico in memoria librerie e funzioni utili per l'analisi
#install.packages('tseries')
#install.packages('rugarch')
#install.packages('rmgarch')

library(tseries)
library(rugarch)
library(rmgarch)



## Dati
price1=get.hist.quote(instrument = "msft", start = "2005-01-03", end="2018-11-30",  quote = "AdjClose",provider = c("yahoo"),  compression = "d",retclass = c("zoo"))
price2=get.hist.quote(instrument = "ba", start = "2005-01-03", end="2018-11-30",  quote = "AdjClose",provider = c("yahoo"),  compression = "d",retclass = c("zoo"))



## Calcolo rendimenti
ret1=diff(log(price1))
plot(ret1,type='l',main=('MSFT returns'))
T_1=length(ret1)
ret2=diff(log(price2))
plot(ret2,type='l',main=('ba returns'))
T_2=length(ret1)



############################################################################
# Datasets multivariati									   #
############################################################################

data=cbind(ret1,ret2) #bv
data=as.data.frame(data)
names(data) <- c("MSFT","BA")

dim(data)
data(dji30retw) #dj
dim(dji30retw)

############################################################################
# Stima modelli DCC					 				  #
############################################################################

#Specificazioni univariate
garch_spec_t1=ugarchspec(variance.model = list(model = "gjrGARCH", garchOrder = c(1, 1)),
mean.model = list(armaOrder = c(0, 0), include.mean = TRUE), fixed.pars=list(),
distribution.model = "std")

garch_spec_t2=ugarchspec(variance.model = list(model = "eGARCH", garchOrder = c(1, 1)),
mean.model = list(armaOrder = c(0, 0), include.mean = TRUE), fixed.pars=list(),
distribution.model = "std")

#spec = ugarchspec()
mspec = multispec( replicate(2, garch_spec_t1) )
mspec12 = multispec( list(garch_spec_t1,garch_spec_t2) )

#test DCC
DCCtest(data, garchOrder = c(1,1), n.lags = 1)

#specificazione e stima dcc
dcc_spec=dccspec(mspec, external.regressors = NULL,
dccOrder = c(1,1), model = c("DCC"), 
distribution = c("mvt"), start.pars = list(), fixed.pars = list())

dcc_spec12=dccspec(mspec12, external.regressors = NULL,
dccOrder = c(1,1), model = c("DCC"), 
distribution = c("mvt"), start.pars = list(), fixed.pars = list())



dcc_esti=dccfit(dcc_spec, data, out.sample = 100, solver = "solnp", solver.control = list(),
fit.control = list(eval.se = TRUE, stationarity = TRUE, scale = FALSE))


dcc_esti12=dccfit(dcc_spec12, data, out.sample = 100, solver = "solnp", solver.control = list(),
fit.control = list(eval.se = TRUE, stationarity = TRUE, scale = FALSE))


#Stima rolling
dcc_esti_roll=dccroll(dcc_spec, data, n.ahead = 1, forecast.length = 100, refit.every = 25,
n.start = NULL, refit.window = c("moving"), window.size = 1000,
solver = "solnp")


#Grafico correlazioni
#opzione 1
plot(dcc_esti)

#opzione 2
slotNames(dcc_esti)
names(dcc_esti@mfit)
R=dcc_esti@mfit$R
T=dim(data)[1]
rhot=rep(0,(T-100))
for (i in 1:(T-100))
{rhot[i]=R[[i]][1,2]}
plot(rhot,type='l')


#ADCC

adcc_spec=dccspec(mspec, external.regressors = NULL,
dccOrder = c(1,1), model = c("aDCC"), 
distribution = c("mvt"), start.pars = list(), fixed.pars = list())

adcc_esti=dccfit(adcc_spec, data, out.sample = 100, solver = "solnp", solver.control = list(),
fit.control = list(eval.se = TRUE, stationarity = TRUE, scale = FALSE))


#DJDATA
mspec30=multispec(replicate(30,garch_spec_t1))

dcc_spec30=dccspec(mspec30, external.regressors = NULL,
dccOrder = c(1,1), model = c("aDCC"), 
distribution = c("mvt"), start.pars = list(), fixed.pars = list())

dcc_esti30=dccfit(dcc_spec30, dji30retw, out.sample = 100, solver = "solnp", solver.control = list(),
fit.control = list(eval.se = TRUE, stationarity = TRUE, scale = FALSE))



