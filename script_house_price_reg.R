df_train<-read.csv2("./Perso/House Prices Advanced Regression Tech/train.csv",sep=",")
df_sample<-read.csv2("./Perso/House Prices Advanced Regression Tech/sample_submission.csv",sep=",")
install.packages("xgboost")
library(xgboost)
colSums(sapply(df_train, is.na))

df_train2<-df_train[complete.cases(df_train),]
learn_set<-sample(nrow(df_train), round(nrow(df_train)*0.6))
test_set<- c(1:nrow(df_train))[!1:nrow(df_train) %in% learn_set]

df_learn<-df_train[learn_set,]
df_learn<-df_learn[complete.cases(df_learn),]

df_test_set<-df_train[test_set,]


library(randomForest)
ncol(df_learn[,81])
model<-randomForest(x=df_learn[,1:80],y=df_learn[,81])

k <- which(is.na(m), arr.ind=TRUE)
m[k] <- rowMeans(m, na.rm=TRUE)[k[,1]]