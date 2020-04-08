#### libraries ####
library(tidyverse)
library(data.table)
library(caret)
library(lubridate)
library(forecast)
library(xgboost)
library(doParallel)

#### load ####
sales_train <- fread("data/sales_train.csv")
items <- fread("data/items.csv") 
test <- fread("data/test.csv")
shops <- fread("data/shops.csv")
submit <- fread("data/sample_submission.csv")
item_cat <- fread("data/item_categories.csv")

### Functions
countunique <- function(x){length(unique(x))}

#### Pre-processing ####
str(sales_train)
str(test)
str(submit)

# Create training data which contains ID-shopid-itemid from test for all month blocks
# and available training data from sales-train

train <- test%>%
  mutate(tmp_id = 1) %>%
  left_join(data.frame(tmp_id = 1,
                       date_block_num = seq(0, 33, by = 1)), by = "tmp_id") %>%
  left_join(sales_train, by = c("shop_id", "item_id", "date_block_num")) %>%
  arrange(ID, date_block_num)

# monthly data
train_month <- train%>%
  group_by(ID,date_block_num)%>%
  summarise(item_cnt_month=sum(item_cnt_day,na.rm=T))%>%
  arrange(ID, date_block_num)

# simple naive
submit_naive_1 <- filter(train_month,date_block_num==33)%>%
  select(ID,item_cnt_month)%>%
  mutate(item_cnt_month = ifelse((item_cnt_month<0),0,ifelse(item_cnt_month>20,20,item_cnt_month)))
fwrite(submit_naive_1,file="submit/naive_1.csv")

# naive using forecast package
# This is done to test against the simple filtering method

# change train to wide with each row as product and 2..k column as date-block
train_month <- setDT(train_month)
setkey(train_month,ID,date_block_num)

train_month <- dcast(train_month,ID~date_block_num,value.var = "item_cnt_month")

# each row is now a numerical vector of 33 monthly sales for each ID

# IDs in test set
ID <- unique(train_month$ID)

set.seed(123)
t1 <- Sys.time()
cl <- makeCluster(11, outfile="")
registerDoParallel(cl)
benchmark <- foreach(i = 1:length(ID),.combine = 'rbind',.inorder=FALSE,
                         .packages=c('data.table','forecast'))%dopar%{
                           
                           # subset train_month for each ID
                           sample <- ts(as.numeric(train_month[ID==ID[i],-c("ID")]),frequency=12)
                           
                           # naive <- naive(sample,1)$mean #naive
                           # ses <- ses(sample, 1)$mean #ses
                           # holt <- holt(sample, h=1, damped=F)$mean #Holt
                           # holt_d <- holt(sample, h=1, damped=T)$mean #Damped
                           # comb <- (ses+holt+holt_d)/3 #Combination
                           
                           # hw <- hw(sample,seasonal="additive",damped=T,h=1)$mean #holt-winters (takes long)
                           # nn <- forecast(nnetar(sample),h=1)$mean
                           cros <- croston(ifelse(sample < 0,0,sample),h=1)$mean
                           
                           # c(ID[i],naive,ses,holt,holt_d,comb)
                           
                           # c(ID[i],naive,ses,holt,holt_d,comb,hw)
                           c(ID[i],cros)
                         }
stopCluster(cl)
Sys.time()-t1 # 28.19 mins


benchmark.test <- data.frame(benchmark)
# colnames(benchmark.test) <- c("ID","naive","ses","holt","holt_d","comb","hw")
colnames(benchmark.test) <- c("ID","naive","ses","holt","holt_d","comb")
# colnames(benchmark.test) <- c("ID","croston")

benchmark.test <- benchmark.test%>%
  remove_rownames()%>%
  mutate(ID=as.integer(ID))%>%
  mutate_at(vars(-ID),~ifelse((.<0),0,ifelse(.>20,20,.)))


for(i in c("naive","ses","holt","holt_d","comb")){
  benchmark.test%>%
    select(ID,item_cnt_month=i)%>%
    fwrite(file=paste0("submit/",i,".csv"))
}

fwrite(train_month,file="train_month.csv")





















