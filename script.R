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



#getting market performance and risk free rate

marketAndRiskFree<-read_csv("F-F_Research_Data_5_Factors_2x3.CSV",skip=3) %>% 
  rename(Date = X1,MKT = "Mkt-RF") %>% select(Date,MKT,RF) %>% 
  mutate(Date = as.numeric(Date)) %>% 
  mutate(MKT = as.numeric(MKT),RF = as.numeric(RF))

firstCorrelations<-factors25 %>% 
  filter(Date>=196401&Date<=199301) %>% 
  left_join(marketAndRiskFree) %>% 
  mutate("excessReturn" = Value - RF)

groupings<-unique(factors25$Portfolio)


for(i in 1:length(groupings)){
  dataNeeded<-filter(firstCorrelations,Portfolio == groupings[i])
  
  model<-lm(formula = Value~MKT,data=dataNeeded)
  dataNeeded$prediction<-predict(model,dataNeeded)
  correl<-cor(dataNeeded$Value,dataNeeded$prediction)
  modelSummary<-summary(model)
  
  beta<-modelSummary$coefficients["MKT","Estimate"]
  alpha<-modelSummary$coefficients["(Intercept)","Estimate"]
  alphaSignificance<-modelSummary$coefficients["(Intercept)","Pr(>|t|)"]
  meanReturn<-mean(dataNeeded$excessReturn)
  tempDF<-data.frame("Size" = unique(dataNeeded$Size),"B/M" = unique(dataNeeded$`B/M`),"meanReturn" = meanReturn,"alpha" = alpha,"alphaSignificance" = alphaSignificance,"beta" = beta,stringsAsFactors = F)
  
  if(exists("permDF")){
    permDF<-full_join(permDF,tempDF,by=names(tempDF))
  }else{
    permDF<-tempDF
  }
}


meanReturnTable<-permDF %>% 
  mutate(Size = factor(Size,levels = c("SMALL","ME1","ME2","ME3","ME4","ME5","BIG")),
         B.M = factor(B.M,levels = c("LoBM","BM1","BM2","BM3","BM4","BM5","HiBM"))) %>% 
  select(Size,B.M,meanReturn) %>% 
  spread(key = "B.M",value = "meanReturn")
