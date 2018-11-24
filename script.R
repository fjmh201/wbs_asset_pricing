library(tidyverse)
library(lubridate)


threeFactorModel<-read_csv("F-F_Research_Data_Factors_daily.CSV",skip = 4) %>% 
  rename(Date = X1) %>% 
  mutate(Date = ymd(Date)) %>% 
  filter(!is.na(Date))

linear1<-lm(formula = `Mkt-RF`~SMB+HML,data = threeFactorModel)
linear1

threeFactorModel$pred<-predict(linear1,threeFactorModel)

cor(threeFactorModel$`Mkt-RF`,threeFactorModel$pred)

fiveFactorModel<-read_csv("F-F_Research_Data_5_Factors_2x3_daily.CSV",skip=3) %>% 
  rename(Date = X1) %>% 
  mutate(Date = ymd(Date))

linear2<-lm(formula = `Mkt-RF`~SMB+HML+RMW+CMA,data=fiveFactorModel)

linear2

fiveFactorModel$pred<-predict(linear2,fiveFactorModel)

cor(fiveFactorModel$`Mkt-RF`,fiveFactorModel$pred)


factors25<-read_csv("25_Portfolios_5x5.CSV",skip=15) %>% 
  rename(Date = X1) %>% 
  gather(key = "Portfolio",value = "Value",-Date) %>% 
  mutate("Size" = str_split_fixed(Portfolio," ",2)[,1],"B/M" = str_split_fixed(Portfolio," ",2)[,2])

firstCorrelations<-factors25 %>% 
  filter(Date>=196401&Date<=199301) %>% 
  group_by()
