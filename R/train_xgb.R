library(mice)
library(xgboost)
source("./Perso/House_Price_reg/R/functions.R")
nrounds<-c()

score1<-c()
t<-Sys.time()
for (j in 1:10){
  
df_train<-read.csv2("./Perso/House_Price_reg/data/train.csv",sep=",")

#Visualisation valeur manquante
head(as.data.frame(sort(colSums(sapply(df_train, is.na))/dim(df_train)[1],decreasing =  TRUE)),10)

#Suppression colonnes "creuses"
for (i in c('Utilities','PoolQC','MiscFeature','Alley','Fence')){
  df_train[i]<-NULL
}

# features<-names(df_train)
# cat_var<-features[sapply(features,function(x) is.factor(df_train[,x]))]
# num_var<-features[sapply(features,function(x) !is.factor(df_train[,x]))]

col_with_na<-colnames(df_train)[colSums(sapply(df_train, is.na))>0]
# cat_var_na<-col_with_na[sapply(col_with_na,function(x) is.factor(df_train[,x]))]
# num_var_na<-col_with_na[sapply(col_with_na,function(x) !is.factor(df_train[,x]))]

df_train1<-df_train

#convert all columns value in numeric (xgb doesn't accept integer)
df_train1[] <- lapply(df_train1, as.numeric)

#Replace Na with mice method
df_train1[col_with_na]<-complete(mice(df_train1[col_with_na],m=5,printFlag = FALSE))

#as.data.frame(sort(colSums(sapply(df_train1, is.na))/nrow(df_train1),decreasing =  TRUE))

#Split leaning/test sets
df_learn<-split_dataset(df_train1,split_pct = 0.5)$df_learn
df_test_set<-split_dataset(df_train1)$df_test_set
# df_learn[] <- lapply(df_learn, as.numeric)
# df_test_set[] <- lapply(df_test_set, as.numeric)


#Conversion des dataframe en xgbMatrix
xgb_learn<-xgb.DMatrix(data.matrix(df_learn[1:ncol(df_learn)-1]),label=df_learn[["SalePrice"]])
xgb_test<-xgb.DMatrix(as.matrix(df_test_set[1:ncol(df_test_set)-1]))

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

#n_rounds=25*j # try more rounds
best_n_rounds<-400

#Modelisation
gb_dt=xgb.train(xgb_params,xgb_learn,nrounds = as.integer(best_n_rounds))

#Predict
res_xgb<-data.frame(SalePrice=df_test_set$SalePrice)
res_xgb$SalePrice_pred<-predict(gb_dt,xgb_test)
res_xgb<-res_xgb[order(res_xgb$SalePrice),]

#Plot Obs/Pred
plot(res_xgb$SalePrice)
points(res_xgb$SalePrice_pred,col=rgb(1,0,0,.5))

#Compute and save score on test set
res_xgb$score<-(log(res_xgb$SalePrice_pred)-log(res_xgb$SalePrice))^2
score_mice_xgb<-sqrt((sum(res_xgb$score)/dim(res_xgb)[1]))
score1<-c(score1,score_mice_xgb)
#nrounds<-c(nrounds,n_rounds)
#score<-c(score,score_mice_xgb)
#print(paste0("n_rounds : " ,n_rounds," - ","score : ",score_mice_xgb))
print(j)
}
t1<-Sys.time()-t
print(t1)
plot(cbind(nrounds,score),type = 'o')
