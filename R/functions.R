replace_NA<-function(df){

	#Recupere colonne contennant des NA
	col_with_na<-colnames(df)[colSums(sapply(df, is.na))>0]
	
	#Check variable nature
	#sapply(col_with_na,function(x) summary(df[x]))

	#Identification variables catégorielles et continues
	cat_var_na<-col_with_na[sapply(col_with_na,function(x) is.factor(df[,x]))]
	con_var_na<-col_with_na[sapply(col_with_na,function(x) !is.factor(df[,x]))]
	
	#Remplace les Na des variables catégorielles par la valeur la plus fréquente
	for (i in cat_var_na){
	  tt<-table(df[,i])
	  df[which(is.na(df[,i])),i]<-names(tt[which.max(tt)])
	}

	#Remplace les Na des variables continues par la moyenne de la série
	for (i in con_var_na){
	  df[which(is.na(df[,i])),i]<-mean(df[,i],na.rm = T)
	}

	return(df)
}

convert_cat_num<- function(df){

	features<-names(df)
	cat_var<-features[sapply(features,function(x) is.factor(df[,x]))]
	
	for(f in cat_var){
	  df[[f]]<-as.numeric(df[[f]])
	}
	return(df)
}

split_dataset<-function(df,split_pct=0.6){

	learn_set<-sample(nrow(df), round(nrow(df)*split_pct))
	test_set<- c(1:nrow(df))[!1:nrow(df) %in% learn_set]
	
	df_learn<-df[learn_set,]
	df_test_set<-df[test_set,]
	return(list("df_learn"=df_learn,"df_test_set"=df_test_set))
}

# features<-names(df_train)
# cat_var<-features[sapply(features,function(x) is.factor(df_train[,x]))]
# num_var<-features[sapply(features,function(x) !is.factor(df_train[,x]))]
# 
# cat_var_na<-col_with_na[sapply(col_with_na,function(x) is.factor(df_train[,x]))]
# num_var_na<-col_with_na[sapply(col_with_na,function(x) !is.factor(df_train[,x]))]
