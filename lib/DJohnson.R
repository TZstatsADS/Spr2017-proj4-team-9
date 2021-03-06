
DJohnson<-read.csv("/Users/xuehan/Desktop/Spr2017-proj4-team-9/data/nameset/D Johnson.csv")

DJohnson$author.ID<-as.character(DJohnson$author.ID)
DJohnson$paper.ID<-as.character(DJohnson$paper.ID)
DJohnson$paper.title<-as.character(DJohnson$paper.title)
DJohnson$coauthor.names<-as.character(DJohnson$coauthor.names)
it_train <- itoken(DJohnson$paper.title, 
                   preprocessor = tolower, 
                   tokenizer = word_tokenizer,
                   ids = DJohnson$paper.ID,
                   # turn off progressbar because it won't look nice in rmd
                   progressbar = FALSE)
vocab <- create_vocabulary(it_train, stopwords = c("a", "an", "the", "in", "on",
                                                   "at", "of", "above", "under"))

#vocab

vectorizer <- vocab_vectorizer(vocab)
dtm_train <- create_dtm(it_train, vectorizer)
dim(dtm_train)

tfidf <- TfIdf$new()
dtm_train_tfidf <- fit_transform(dtm_train, tfidf)



####Initialize Parameter lambda
lambda<-rep(1,nrow(dtm_train_tfidf))

#Add the Author's ID as the label column to the feature matrix for future use
dtm_train_tfidf<-cbind(dtm_train_tfidf,as.numeric(DJohnson$author.ID))


#Given the training set, we are able to generate the true clusters. 
#Based on the paper, we define true score S_star as the distance of the sum of clusterwise distance. We also define our own score function as the sum of distance of clusters that is clustered by hclust() function.


#Compute the true score S_star for the giving training data
element<-list()
S_star<-vector(length=length(unique(DJohnson$author.ID)))
for (i in 1:length(unique(DJohnson$author.ID))){
  element[[i]]<-dtm_train_tfidf[dtm_train_tfidf[,ncol(dtm_train_tfidf)]==i,]
  S_star[i]<-sum(dist(element[[i]]))/2
}
S_star<-mean(S_star)
T_star<-dtm_train_tfidf[,ncol(dtm_train_tfidf)]

K=14
k=1
lambda1 <- matrix(NA, nrow = nrow(DJohnson), ncol = K)
S1 <- numeric(K)
acc <- numeric(K)
while (k<(K+1)){
  
  #Implement Hierirchical Clustering 
  h<-hclust(dist(dtm_train_tfidf*lambda))
  #Check the result for the number of cluster equals to the number of unique authors in the DJohnson. 
  h_result<-cutree(h,k=length(unique(DJohnson$author.ID)))
  
  #Compute the our own score function S
  S<-vector(length=length(unique(DJohnson$author.ID)))
  element_s<-list()
  for (i in 1:length(unique(DJohnson$author.ID))){
    element_s[[i]]<-dtm_train_tfidf[which(h_result==i),]
    S[i]<-sum(dist(element_s[[i]]))/2
  }
  S<-mean(S)
  
  
  
  #Identify true author for each cluster generated by hclust() function, and assign it to each element of the cluster.
  
  label<-dtm_train_tfidf[,ncol(dtm_train_tfidf)]
  author.clust <- vector(length=length(unique(DJohnson$author.ID)))
  for (i in 1:length(unique(DJohnson$author.ID))){
    author.clust[i]<-as.numeric(names(which.max(table(label[which(h_result==i)]))))
  }
  
  for (i in 1:unique(DJohnson$author.ID)){
    h_result[h_result==i]<-author.clust[i]
  }
  T_hat<-h_result
  
  #Update lambda
  
  for (i in 1:length(T_star)){
    if (T_hat[i]!=T_star[i]){
      lambda[i]<-lambda[i]-((S-S_star)/S) #!!!
    }
    else {
      lambda[i]<-lambda[i]
    }
  }
  lambda1[,k] <- lambda
  S1[k] <- S
  acc[k] <- mean(label == h_result)    
  k=k+1
}

source('../lib/evaluation_measures.R') 
matching_matrix_hclust <- matching_matrix(DJohnson$author.ID,T_hat_overall) 
performance_hclust.DJ <- performance_statistics(matching_matrix_hclust)

performance_hclust.DJ


