library(readr)
bitcoin = read_csv("C:/Users/DEVI PRADEEP/Downloads/Bitcoin Historical Data.csv")
xrp=read_csv("C:/Users/DEVI PRADEEP/Downloads/XRP Historical Data.csv")
bitcoin
xrp
tail(xrp)
tail(bitcoin)

#ensure date column are in data type
bitcoin$Date = as.Date(bitcoin$Date,format = "%d-%m-%Y")
xrp$Date = as.Date(xrp$Date,format="%d-%m-%Y")
bitcoin
xrp

#returns
library(dplyr)
bitcoin = bitcoin %>%
  mutate(daily_return = (Price - lag(Price))/lag(Price))
xrp = xrp %>%
  mutate(daily_return = ( Price -lag( Price ))/lag( Price ))
xrp

sd_bitcoin=sd(bitcoin$daily_return,na.rm = TRUE)
sd_xrp = sd(xrp$daily_return,na.rm = T)
sd_bitcoin
sd_xrp

bitcoin = bitcoin %>%
  mutate(daily_return = ifelse(is.na(daily_return),0,daily_return),
         cumulative_return =cumprod(1+daily_return))
bitcoin
xrp = xrp %>%
  mutate(daily_return = ifelse(is.na(daily_return),0,daily_return),
         cumulative_return = cumprod(1+daily_return))
xrp


library(ggplot2)
ggplot()+
  geom_line(data = bitcoin,aes(x=Date,y=Price,color="Bitcoin"))+
  geom_line(data = xrp,aes(x=Date,y=Price,color="XRP"))+
  labs(title="Closing price for Bitcoin and XRP",
       x="Date",
       y="Closing Price")

ggplot()+
  geom_line(data=bitcoin,aes(x=Date,y=daily_return,color="Bitcoin"))+
  geom_line(data=xrp,aes(x=Date,y=daily_return,color="XRP"))+
  labs(title="Daily return for Bitcoin and XRP",
       x="Date",
       y="Daily Return")

ggplot()+
  geom_line(data= bitcoin,aes(x=Date,y=cumulative_return,color="Bitcoin"))+
  geom_line(data=xrp,aes(x=Date,y=cumulative_return,color="XRP"))+
  labs(title="cumulative return for Bitcoin and XRP",
       x="Date",
       y="cumulative Return")

risk_free_rate =0.06/365

sharpe_ratio_bitcoin = (mean(bitcoin$daily_return)-risk_free_rate)/sd_bitcoin
sharpe_xrp = (mean(xrp$daily_return)-risk_free_rate)/sd_xrp
sharpe_ratio_bitcoin
sharpe_xrp

