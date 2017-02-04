library(randomForest)

#################### Build model with training data
df_train<-read.csv2("./Perso/House_Price_reg/train.csv",sep=",")

#Suppression colonnes "creuses"
for (i in c('ID','PoolQC','MiscFeature','Alley','Fence','Utilities')){
  df_train[i]<-NULL
}
#Remplace les Valeurs manquantes
df_train<-replace_NA(df_train)

#Conversion variable cat en num
df_train<-convert_cat_num(df_train)

#Modélisation
model<-randomForest(x=df_train,y=df_train)


#################### Apply model to submitted data

#####Shapping Data
#Lecture
df_test<-read.csv2("./Perso/House_Price_reg/test.csv",sep=",")

#Visualisation valeur manquante
head(as.data.frame(sort(colSums(sapply(df_test, is.na))/dim(df_test)[1],decreasing =  TRUE)))

#Suppression colonnes "creuses"
for (i in c('ID','PoolQC','MiscFeature','Alley','Fence')){
  df_test[i]<-NULL
}
#Visualisation valeur manquante
as.data.frame(sort(colSums(sapply(df_test, is.na))/dim(df_test)[1],decreasing =  TRUE))

df_test<-replace_NA(df_test)

#convert categorical into numeric
df_test[]<-lapply(df_test,as.numeric)


#####Predict
test_res<-data.frame(Id=df_test$Id)
test_res$SalePrice<-predict( model, newdata = df_test)
test_rf$SalePrice_pred<-predict( model, newdata = df_test_set[,1:ncol(df_learn)-1])
write.table(test_res,"./Perso/House_Price_reg/test_submission2.csv",sep=",",row.names = FALSE)