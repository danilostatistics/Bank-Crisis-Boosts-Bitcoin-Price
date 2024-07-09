#################################################
#
################################################


library(ggforce)
library(tidyverse)
theme_set(theme_bw(16))
astronauts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-14/astronauts.csv')

astronauts %>% 
  ggplot(aes(x=year_of_mission,hours_mission, color=sex))+
  geom_point()+
  scale_y_log10()

astronauts %>% 
  ggplot(aes(x=year_of_mission,hours_mission, color=sex))+
  geom_point()+
  scale_y_log10()+
  facet_zoom(xlim = c(1965, 1985))+
  

############BUONO VOLATILITA'########################################

TimeFrame<- 1:378



Variance_BTC<- scale(BTC.AR1_EGarch11_GE@fit$var)
Variance_SP500<- scale(SP500.AR1_EGarch11_Norm@fit$var)
Variance_NADQ<- scale(NASDQ.AR1_EGarch11_GE@fit$var)
Variance_Gold<- scale(GOLD.AR1_EGarch11_GE@fit$var)
Variance_Banks<- scale(BANKS.AR1_EGarch11_Norm@fit$var)

VariancesEstimated<- as_tibble(data.frame(V_BTC=Variance_BTC[TimeFrame],
           V_SP500=Variance_SP500[TimeFrame],
           V_NASDQ=Variance_NADQ[TimeFrame],
           V_Gold=Variance_Gold[TimeFrame],
           V_Banks=Variance_Banks[TimeFrame],
           timespan=SK_Market_returns$period[periodOfInterest]))

save(VariancesEstimated, file='VariancePlot.RData')
load( file='VariancePlot.RData')


VariancesEstimated %>% 
  ggplot(aes(x=timespan))+
  geom_line(aes(y=V_BTC, color='BTC'))+
  geom_line(aes(y=V_SP500, color='SP500'))+
  geom_line(aes(y=V_NASDQ, color='NASDQ'))+
  geom_line(aes(y=V_Gold, color='Gold'))+
  geom_line(aes(y=V_Banks, color='Banks'))+
  facet_zoom(xlim = as.POSIXct(c("2023-03-01 00:00:00", "2023-04-15 00:00:00")))+
  labs(x = "Time",
       y = "Volatility")+
  geom_vline(xintercept=as.POSIXct(c("2023-03-09 00:00:00")), linetype='dotted', color='grey30', linewidth=0.5)+
  geom_text(aes(x=as.POSIXct(c("2023-03-09 00:00:00")), label="(a)", y=9.5), colour="grey20")+
  theme(legend.justification = "top")+
  coord_cartesian()+
  scale_color_manual(name='',
                     breaks=c('BTC', 'SP500', 'NASDQ', 'Gold', 'Banks'),
                     values=c('BTC'='black', 'SP500'='red', 'NASDQ'='blue','Gold'='green', 'Banks'='orange'))+
  theme_bw()+
  theme(legend.justification = "top",
        legend.position = c(0.05, 0.99),
        legend.key.size = unit(0.2, 'cm'),
        legend.key.height = unit(0.1, 'cm'),
        legend.key.width = unit(0.1, 'cm'),
        legend.text = element_text(size=9),
        legend.title = element_text(size=1))
    ggsave('Volatility.eps')    
####################################################





############ Plot on prices with zoom#############################
TimeFrame<- 1:378

Pt_BTC  <-   scale(SK_Market_returns$adjclose_BTC_USD)
Pt_SP500<- scale(SK_Market_returns$adjclose__GSPC )
Pt_NSDQ <-  scale(SK_Market_returns$adjclose__IXIC)
Pt_Gold <- scale(SK_Market_returns$adjclose_PAXG_USD)
Pt_back <-scale(SK_Market_returns$av_banks)


Prices<- as_tibble(data.frame(P_BTC=Pt_BTC[TimeFrame],
                                          P_SP500=Pt_SP500[TimeFrame],
                                          P_NASDQ=Pt_NSDQ[TimeFrame],
                                          P_Gold=Pt_Gold[TimeFrame],
                                          P_Banks=Pt_back[TimeFrame],
                                          timespan=SK_Market_returns$period[periodOfInterest]))
save(Prices, file='Price.RData')
load( file='Price.RData')

Prices %>% 
  ggplot(aes(x=timespan))+
  geom_line(aes(y=P_BTC, color='BTC'))+
  geom_line(aes(y=P_SP500, color='SP500'))+
  geom_line(aes(y=P_NASDQ, color='NASDQ'))+
  geom_line(aes(y=P_Gold, color='Gold'))+
  geom_line(aes(y=P_Banks, color='Banks'))+
  facet_zoom(xlim = as.POSIXct(c("2023-03-01 00:00:00", "2023-04-15 00:00:00")))+
  labs(x = "Time",
       y = "Price")+
  geom_vline(xintercept=as.POSIXct(c("2023-03-09 00:00:00")), linetype='dotted', color='grey30', linewidth=0.5)+
  geom_text(aes(x=as.POSIXct(c("2023-03-09 00:00:00")), label="(a)", y=2), colour="grey20")+
  theme(legend.justification = "top")+
  coord_cartesian()+
  scale_color_manual(name='',
                     breaks=c('BTC', 'SP500', 'NASDQ', 'Gold', 'Banks'),
                     values=c('BTC'='black', 'SP500'='red', 'NASDQ'='blue','Gold'='green', 'Banks'='orange'))+
  theme_bw()+
  theme(legend.justification = "top",
        legend.position = c(0.05, 0.84),
        legend.key.size = unit(0.2, 'cm'),
        legend.key.height = unit(0.1, 'cm'),
        legend.key.width = unit(0.1, 'cm'),
        legend.text = element_text(size=9),
        legend.title = element_text(size=1))


ggsave('Price.eps')
######################### Returns ###########################################


TimeFrame<- 1:378

rt_BTC  <-   SK_Market_returns$log_adjclose_BTC_USD
rt_SP500<- SK_Market_returns$log_adjclose__GSPC
rt_NSDQ <-  SK_Market_returns$log_adjclose__IXIC
rt_Gold <- SK_Market_returns$log_adjclose_PAXG_USD
rt_back <-SK_Market_returns$log_av_banks


Returns<- as_tibble(data.frame(R_BTC=rt_BTC[TimeFrame],
                              R_SP500=rt_SP500[TimeFrame],
                              R_NASDQ=rt_NSDQ[TimeFrame],
                              R_Gold=rt_Gold[TimeFrame],
                              R_Banks=rt_back[TimeFrame],
                              timespan=SK_Market_returns$period[periodOfInterest]))
save(Returns, file='Returns.RData')
load( file='Returns.RData')

Returns %>% 
  ggplot(aes(x=timespan))+
  geom_line(aes(y=R_BTC, color='BTC'))+
  geom_line(aes(y=R_SP500, color='SP500'))+
  geom_line(aes(y=R_NASDQ, color='NASDQ'))+
  geom_line(aes(y=R_Gold, color='Gold'))+
  geom_line(aes(y=R_Banks, color='Banks'))+
  facet_zoom(xlim = as.POSIXct(c("2023-03-01 00:00:00", "2023-04-15 00:00:00")))+
  coord_cartesian()+
  labs(x = "Time",
       y = "Price")+
  geom_vline(xintercept=as.POSIXct(c("2023-03-09 00:00:00")), linetype='dotted', color='grey30', linewidth=0.5)+
  geom_text(aes(x=as.POSIXct(c("2023-03-09 00:00:00")), label="(a)", y=0.1), colour="grey20")+
  theme(legend.justification = "top")+
  scale_color_manual(name='',
                     breaks=c('BTC', 'SP500', 'NASDQ', 'Gold', 'Banks'),
                     values=c('BTC'='black', 'SP500'='red', 'NASDQ'='blue','Gold'='green', 'Banks'='orange'))+
  theme_bw()+
  theme(legend.justification = "top",
        legend.position = c(0.05, 0.84),
        legend.key.size = unit(0.2, 'cm'),
        legend.key.height = unit(0.1, 'cm'),
        legend.key.width = unit(0.1, 'cm'),
        legend.text = element_text(size=9),
        legend.title = element_text(size=1))

ggsave('Returns.eps')
