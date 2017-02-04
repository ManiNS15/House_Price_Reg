library(randomForest)

res1<-c()
for (k in 1:10){

  df_train<-read.csv2("./Perso/House_Price_reg/train.csv",sep=",")
  #df_sample<-read.csv2("./Perso/House_Price_reg/sample_submission.csv",sep=",")
  
  #Visualisation valeur manquante
  head(as.data.frame(sort(colSums(sapply(df_train, is.na))/dim(df_train)[1],decreasing =  TRUE)),10)
  
  #Suppression colonnes "creuses"
  for (i in c('ID','PoolQC','MiscFeature','Alley','Fence','Utilities')){
    df_train[i]<-NULL
  }
  
  drop_NA<-F
  
  if (isTRUE(drop_NA))
  {
    #On ne prend que les lignes sans NA
    df_train<-df_train[complete.cases(df_train),]
  } else 
  {
    #On remplace par NA
    df_train<-replace_NA(df_train)
  }
  
  #Conversion variable cat en num
  df_train<-convert_cat_num(df_train)
  
  #Ensemble apprentissage/test
  df_learn<-split_dataset(df_train)$df_learn
  df_test_set<-split_dataset(df_train)$df_test_set
  
  
  #Modelisation
  model<-randomForest(x=df_learn[,1:ncol(df_learn)-1],y=df_learn[,ncol(df_learn)])
  
  #Resultat Prédiction
  test_rf<-data.frame(SalePrice=df_test_set$SalePrice)
  test_rf$SalePrice_pred<-predict( model, newdata = df_test_set[,1:ncol(df_learn)-1])
  test_rf<-test_rf[order(test_rf$SalePrice),]
  
  #Plot Comparaison Obs/Pred
  plot(test_rf$SalePrice)
  points(test_rf$SalePrice_pred,col=rgb(1,0,0,.5))
  
  #calcul du score
  test_rf$score<-(log(test_rf$SalePrice_pred)-log(test_rf$SalePrice))^2
  print(sqrt((sum(test_rf$score)/dim(test_rf)[1])))
  
  if (isTRUE(drop_NA))
  {
    score_drop_na<-sqrt((sum(test_rf$score)/dim(test_rf)[1]))  
  } else
  {
    score_replace_na<-sqrt((sum(test_rf$score)/dim(test_rf)[1]))
  }
  
  res1<-c(res1,sqrt((sum(test_rf$score)/dim(test_rf)[1])))
}