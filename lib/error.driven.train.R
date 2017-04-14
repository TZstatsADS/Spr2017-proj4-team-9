error.driven.train<-function(dataset,lambda=rep(1,nrow(dtm_train_tfidf))){
  ########change all AKumar to dataset
  
  # extract canonical author id befor "_"
  dataset$AuthorID <- sub("_.*","",dataset$Coauthor)
  # extract paper number under same author between "_" and first whitespace
  dataset$PaperNO <- sub(".*_(\\w*)\\s.*", "\\1", dataset$Coauthor)
  # delete "<" in AKumar$Coauthor, you may need to further process the coauthor
  # term depending on the method you are using
  dataset$Coauthor <- gsub("<","",sub("^.*?\\s","", dataset$Coauthor))
  # delete "<" in AKumar$Paper
  dataset$Paper <- gsub("<","",dataset$Paper)
  # add PaperID for furthur use, you may want to combine all the nameset files and 
  # then assign the unique ID for all the citations
  dataset$PaperID <- rownames(dataset)
  

  
  
  ### Step 2: Feature Design
  
  it_train <- itoken(dataset$Paper, 
                     preprocessor = tolower, 
                     tokenizer = word_tokenizer,
                     ids = dataset$PaperID,
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
  dtm_train_tfidf<-cbind(dtm_train_tfidf,as.numeric(dataset$AuthorID))
  
  

  
  #Compute the true score S_star for the giving training data
  element<-list()
  S_star<-vector(length=length(unique(dataset$AuthorID)))
  for (i in 1:length(unique(dataset$AuthorID))){
    element[[i]]<-dtm_train_tfidf[dtm_train_tfidf[,ncol(dtm_train_tfidf)]==i,]
    S_star[i]<-sum(dist(element[[i]]))/2
  }
  S_star<-mean(S_star)
  T_star<-dtm_train_tfidf[,ncol(dtm_train_tfidf)]
  
  
  while (S<0.01){
    
    #Implement Hierirchical Clustering 
    h<-hclust(dist(dtm_train_tfidf*lambda))
    #Check the result for the number of cluster equals to the number of unique authors in the dataset. 
    h_result<-cutree(h,k=length(unique(dataset$AuthorID)))
    
    #Compute the our own score function S
    S<-vector(length=length(unique(dataset$AuthorID)))
    element_s<-list()
    for (i in 1:length(unique(dataset$AuthorID))){
      element_s[[i]]<-dtm_train_tfidf[which(h_result==i),]
      S[i]<-sum(dist(element_s[[i]]))/2
    }
    S<-mean(S)
    
    T_hat<-h_result
    
    
    for (i in 1:length(T_star)){
      if (T_hat[i]!=T_star[i]){
        lambda[i]<-lambda[i]-((S-S_star)/S)
      }
      else {
        lambda[i]<-lambda[i]
      }
    }
    
  }
  
}