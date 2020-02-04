library(data.table)
library(mlr)
library(caret)
library(xgboost)
library(readr)
library(dplyr)
library(tidyr)
library(mltools) ##Cal rmsle
df <- read_csv("C:/Users/terte/Desktop/R/Project02/trainSmp.csv") 
str(df)

## check weird supplier 
df[which(df$Producto_ID==4118),]   ##  1KG not in train
df[which(df$Producto_ID==0),]  ## 0 not in train


temp <- df
temp$Venta_uni_hoy <- NULL
temp$Venta_hoy <- NULL
temp$Dev_uni_proxima <- NULL
temp$Dev_proxima  <- NULL
#temp

#df_test <- df_train[Semana>8,]
df_train <- temp[temp$Semana <= 7,]
df_test <- temp[temp$Semana >= 8,]
head(df_train)
temp <- df_train

# https://statisticsglobe.com/mean-by-group-in-r
#aggregate(temp[,7], list(temp$Producto_ID), mean)

demand_mean_client <- aggregate(temp[,7], list(temp$Producto_ID ,temp$Cliente_ID), mean)
names(demand_mean_client)[names(demand_mean_client) == "Group.1"] <- "Producto_ID"
names(demand_mean_client)[names(demand_mean_client) == "Group.2"] <- "Cliente_ID"
names(demand_mean_client)[names(demand_mean_client) == "Demanda_uni_equil"] <- "Mean_byPC"
colnames(demand_mean_client)

##----- merge in training set
temp1 <- merge(x=temp,y=demand_mean_client,by=c("Producto_ID","Cliente_ID"), all.x = TRUE)
dim(temp1)
head(temp1,10)
df_train2 <- temp1
##----- merge in testset
temp <- df_test
dim(temp)
temp2 <- merge(x=temp,y=demand_mean_client,by=c("Producto_ID","Cliente_ID"), all.x = TRUE)
dim(temp2)
head(temp2,10)
df_test2 <- temp2
##--------
#temp$mean_client_prod <- m
#df_train[Semana <= 8][, .(mean_client_prod = mean(Demanda_uni_equil), 
#                       count_client_prod = .N),
#                   by = .(Producto_ID, Cliente_ID)] %>% 
#  merge(rec_train, all.y = TRUE, by = c("Producto_ID", "Cliente_ID")) -> rec_train
df_test2[is.na(df_test2)] <- 0
dim(df_train2)
dim(df_test2)
#dim(df)

#df_train$Demanda_uni_equil = log1p(df_train$Demanda_uni_equil)

#convert data frame to data table
setDT(df_train2) 
setDT(df_test2)

#check missing values 
#table(is.na(df_train))
#sapply(df_train, function(x) sum(is.na(x))/length(x))*100

#table(is.na(df_test))
#sapply(df_test, function(x) sum(is.na(x))/length(x))*100
##------

## Convert the categorical variables into numeric using one hot encoding
#using one hot encoding 
#labels <- df_train$Cliente_ID 
#ts_label <- df_test$Cliente_ID 
#new_tr <- model.matrix(~.+0,data = df_train[,-c("Cliente_ID"),with=F]) 
#new_ts <- model.matrix(~.+0,data = test[,-c("target"),with=F])
#str(df_train)

labels <- df_train2$Demanda_uni_equil 
#ts_label <-  as.matrix(df_test2$Demanda_uni_equil) 
ts_label <-  df_test2$Demanda_uni_equil 
new_tr <- model.matrix(~.+0,data = df_train2[,-c("Demanda_uni_equil"),with=F]) 
new_ts <- model.matrix(~.+0,data = df_test2[,-c("Demanda_uni_equil"),with=F])
#str(labels)
#str(ts_label)
#dim(new_ts)
#preparing matrix 
dtrain2 <- xgb.DMatrix(data = new_tr,label = labels) 
dtest2 <- xgb.DMatrix(data = new_ts,label = ts_label)

#default parameters
params <- list(booster = "gbtree", objective = "reg:linear", eta=0.1, gamma=0, max_depth=10, min_child_weight=1, subsample=0.85, colsample_bytree=0.7)

xgbcv <- xgb.cv( params = params, data = dtrain, nrounds = 100, nfold = 5, showsd = T, stratified = T, print_every_n = 10, early_stop_round = 20, maximize = F)
xgbcv$evaluation_log

#first default - model training
set.seed(20)
xgb1 <- xgb.train(params = params, data = dtrain2, nrounds = 46, watchlist = list(val=dtest2,train=dtrain2), print_every_n = 5, early_stop_round = 20, maximize = F)
#model prediction
xgbpred <- predict(xgb1,dtest2)

#view variable importance plot
mat <- xgb.importance(feature_names = colnames(new_tr),model = xgb1)
xgb.plot.importance(importance_matrix = mat[1:20]) 

#xgbpred <- expm1(xgbpred)
xgbpred <- as.integer(xgbpred)  ## convert to int
xgbpred[xgbpred<=1] <- 2

#xgbpredV2 <- xgbpred
dim(ts_label)
dim(xgbpred)
## cal rmsle
rmsle(ts_label, xgbpred)
## 0.54

##0.5346 at nrounds = 46