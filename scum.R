##### Simple combination of univariate models #####

#### libraries ####
library(tidyverse)
library(data.table)
library(caret)
library(lubridate)
library(forecast)
library(xgboost)
library(doParallel)
library(smooth)
library(forecTheta)

#### load ####
train_month <- fread("train_month.csv")

##
ID <- unique(train_month$ID)

### scum  
t1 <- Sys.time()
cl <- makeCluster(11, outfile="")
registerDoParallel(cl)
scum.fcs <- foreach(i = 15000:16000,.combine = 'rbind',.inorder=FALSE,
                    .packages=c('data.table','forecast','forecTheta', 'smooth'))%dopar%{
                      
                      y <- ts(as.numeric(train_month[ID==ID[i],-c("ID")]),frequency=12)
                      
                      if(sum(y)==0){
                        c(ID[i],rep(0,6))
                      }else{
                        # ETS
                        ets<- as.vector(forecast(ets(y), h=1, level=95)$mean)
                        # CES
                        ces <- as.vector(auto.ces(as.vector(y),h=1,interval="p",level=.95)$forecast)
                        #SES
                        ses <- as.vector(forecast(ses(y),h=1)$mean)
                        # AutoArima
                        arima <- as.vector(forecast(auto.arima(y),h=1,level=95)$mean)
                        # Dotm
                        dotm <- as.vector(dotm(y,h=1,level=95,)$mean)
                        #Comb
                        fcs.comb <- median(ets,ces,ses,arima,dotm)
                        c(ID[i],ets,ces,ses,arima,dotm,fcs.comb)
                      }
                    }
stopCluster(cl)
Sys.time()-t1 # 

scum <- data.frame(scum.fcs)
colnames(scum) <- c("ID","ets","ces","ses","arima","dotm","scrum.comb")

scum <- scum%>%
  remove_rownames()%>%
  mutate(ID=as.integer(ID))%>%
  mutate_at(vars(-ID),~ifelse((.<0),0,ifelse(.>20,20,.))) #clip at (0,20)

for(i in c("ets","ces","ses","arima","dotm","scrum.comb")){
  scum%>%
    select(ID,item_cnt_month=i)%>%
    fwrite(file=paste0("submit/",i,".csv"))
}
















































