library(xgboost)
bst <- xgboost(data = train$data, label = train$label, max.depth = 2, eta = 1, nround = 2,
               nthread = 2, objective = "binary:logistic")

data(agaricus.train, package='xgboost')
data(agaricus.test, package='xgboost')
train <- agaricus.train
as.data.frame(train$data)
#############################################################
res<-c()
for (j in 1:10){
  
df_train<-read.csv2("./Perso/House_Price_reg/train.csv",sep=",")
#df_sample<-read.csv2("./Perso/House_Price_reg/sample_submission.csv",sep=",")

#Visualisation valeur manquante
head(as.data.frame(sort(colSums(sapply(df_train, is.na))/dim(df_train)[1],decreasing =  TRUE)),10)

#Suppression colonnes "creuses"
for (i in c('Utilities','PoolQC','MiscFeature','Alley','Fence')){
  df_train[i]<-NULL
}

as.data.frame(sort(colSums(sapply(df_train, is.na))/dim(df_train)[1],decreasing =  TRUE))
features<-names(df_train)
cat_var<-features[sapply(features,function(x) is.factor(df_train[,x]))]
num_var<-features[sapply(features,function(x) !is.factor(df_train[,x]))]

col_with_na<-colnames(df_train)[colSums(sapply(df_train, is.na))>0]
cat_var_na<-col_with_na[sapply(col_with_na,function(x) is.factor(df_train[,x]))]
num_var_na<-col_with_na[sapply(col_with_na,function(x) !is.factor(df_train[,x]))]


df_train1<-df_train

#convert character into integer
for(f in cat_var){
  df_train1[[f]]=as.integer(df_train1[[f]])
}

library(mice)
df_train1[col_with_na]<-complete(mice(df_train1[col_with_na],m=5))
as.data.frame(sort(colSums(sapply(df_train1, is.na))/nrow(df_train1),decreasing =  TRUE))

features[sapply(features,function(x) is.integer(df_train1[,x]))]
df_train1[] <- lapply(df_train1, as.numeric)

learn_set<-sample(nrow(df_train1), round(nrow(df_train1)*1))
test_set<- c(1:nrow(df_train1))[!1:nrow(df_train1) %in% learn_set]
df_learn<-df_train1[learn_set,]
df_test_set<-df_train1[test_set,]


#Error in xgb.DMatrix [...]can only be applied to a 'numeric', not a 'integer'
xgb_learn<-xgb.DMatrix(as.matrix(df_learn[1:ncol(df_learn)-1]),label=df_learn[["SalePrice"]])
xgb_test<-xgb.DMatrix(as.matrix(df_test_set[1:ncol(df_test_set)-1]))

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

best_n_rounds=150 # try more rounds
gb_dt=xgb.train(xgb_params,xgb_learn,nrounds = as.integer(best_n_rounds))

###########################################
df_test<-read.csv2("./Perso/House_Price_reg/test.csv",sep=",")

#Suppression colonnes "creuses"
for (i in c('Utilities','PoolQC','MiscFeature','Alley','Fence')){
  df_test[i]<-NULL
}

features<-names(df_test)
cat_var<-features[sapply(features,function(x) !is.numeric(df_test[,x]))]
#convert character into numeric

head(as.data.frame(sort(colSums(sapply(df_test, is.na))/dim(df_test)[1],decreasing =  TRUE)))

df_test[] <- lapply(df_test, as.numeric)

library(mice)
col_with_na<-colnames(df_test)[colSums(sapply(df_test, is.na))>0]
df_test[col_with_na]<-complete(mice(df_test[col_with_na],m=2))

xgb_test<-xgb.DMatrix(as.matrix(df_test))

res_xgb1<-data.frame(ID=df_test$Id)
res_xgb1$SalePrice<-predict(gb_dt,xgb_test)
write.table(res_xgb1,"./Perso/House_Price_reg/test_submission3.csv",sep=",",row.names = FALSE)

###########################################

res_xgb<-data.frame(SalePrice=df_test_set$SalePrice)
res_xgb$SalePrice_pred<-predict(gb_dt,xgb_test)
res_xgb<-res_xgb[order(res_xgb$SalePrice),]
plot(res_xgb$SalePrice)
points(res_xgb$SalePrice_pred,col=rgb(1,0,0,.5))
res_xgb$score<-(log(res_xgb$SalePrice_pred)-log(res_xgb$SalePrice))^2

score_mice_xgb<-sqrt((sum(res_xgb$score)/dim(res_xgb)[1]))
res<-c(res,sqrt((sum(res_xgb$score)/dim(res_xgb)[1])))


}




as.matrix(df_train)

nrow(df_train)
names(df_train)

class(df_train[["HeatingQC"]])
class(df_train$HeatingQC)

f<-"HeatingQC"

levels=sort(unique(df_train[[f]]))
train_test[[f]]=as.integer(factor(df_train[[f]],levels = levels))


factor(df_train[[f]],levels = levels)
as.integer(df_train[[f]])
as.integer(df_train[,f])

#convert character into integer
for(f in features){
  if(class(train_test[[f]])=="character"){
    levels=sort(unique(as.character(df_train[[f]])))
    train_test[[f]]=as.integer(factor(train_test[[f]],levels = levels))
  }
}

levels[1]<-"An"
factor(df_train[[f]],levels = levels)
table(df_train[[f]])

factor(as.character(df_train[,f]))

