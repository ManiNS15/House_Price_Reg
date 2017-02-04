library(xgboost)
library(mice)

######Build Model XGB
df_train<-read.csv2("./Perso/House_Price_reg/data/train.csv",sep=",")

#Visualisation valeur manquante
head(as.data.frame(sort(colSums(sapply(df_train, is.na))/dim(df_train)[1],decreasing =  TRUE)),10)

#Suppression colonnes "creuses"
for (i in c('Utilities','PoolQC','MiscFeature','Alley','Fence')){
  df_train[i]<-NULL
}

#Get column with NA
col_with_na<-colnames(df_train)[colSums(sapply(df_train, is.na))>0]

df_train1<-df_train

#convert all columns value in numeric (xgb doesn't accept integer)
df_train1[] <- lapply(df_train1, as.numeric)

#Replace Na with mice method
df_train1[col_with_na]<-complete(mice(df_train1[col_with_na],m=5,printFlag = F))

#Check there's no Na left
#as.data.frame(sort(colSums(sapply(df_train1, is.na))/nrow(df_train1),decreasing =  TRUE))

#Conversion des dataframe en xgbMatrix
xgb_train_set<-xgb.DMatrix(as.matrix(df_train1[1:ncol(df_train1)-1]),label=df_train1[["SalePrice"]])

#Configuration
xgb_params = list(
  seed = 144,
  colsample_bytree = 0.5,
  subsample = 0.8,
  eta = 0.02, 
  objective = 'reg:linear',
  max_depth = 12,
  alpha = 1,
  gamma = 2,
  min_child_weight = 1,
  base_score = 7.76
)

xg_eval_mae <- function (yhat, dtrain) {
  y = getinfo(dtrain, "label")
  err= mae(exp(y),exp(yhat) )
  return (list(metric = "error", value = err))
}

best_n_rounds=400 # try more rounds

#Modelisation
gb_dt=xgb.train(xgb_params,xgb_train_set,nrounds = as.integer(best_n_rounds))

######Apply Model XGB to test set

df_test<-read.csv2("./Perso/House_Price_reg/data/test.csv",sep=",")

#Suppression colonnes "creuses"
for (i in c('Utilities','PoolQC','MiscFeature','Alley','Fence')){
  df_test[i]<-NULL
}

head(as.data.frame(sort(colSums(sapply(df_test, is.na))/dim(df_test)[1],decreasing =  TRUE)))

#convert columns value into numeric
df_test[] <- lapply(df_test, as.numeric)

#Replace Na
col_with_na<-colnames(df_test)[colSums(sapply(df_test, is.na))>0]
df_test[col_with_na]<-complete(mice(df_test[col_with_na],m=5,printFlag = F))

#Convert
xgb_test<-xgb.DMatrix(as.matrix(df_test))

#Predict
res_xgb1<-data.frame(ID=df_test$Id)
res_xgb1$SalePrice<-predict(gb_dt,xgb_test)
write.table(res_xgb1,"./Perso/House_Price_reg/submissions//test_submission5.csv",sep=",",row.names = FALSE)

