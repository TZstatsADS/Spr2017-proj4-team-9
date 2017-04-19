setwd("~/Desktop/Spr2017-proj4-team-9-master/")
dat = read.csv("data/nameset/A Gupta.csv")
test = data.frame()
train = data.frame()

for (i in 1:max(dat$author.ID))
{
  sub = subset(dat, author.ID == i)
  sam_index = sample(nrow(sub), floor(nrow(sub)/2))
  sam = sub[sam_index, ]
  train = rbind(train, sam)
  antisam = sub[-sam_index ,]
  test = rbind(test, antisam)
}


nb.train = function(train)
{
  # number vectors
  
  # calculate the number of authors in the dataset
  numauthor = max(train$author.ID)
  # to count the number of paper for each author
  numpaper = rep(0, numauthor)
  # to count the number of paper authored with coauthors
  numpapercoauthor = rep(0, numauthor)
  # number of paper the author writes alone
  numpapernocoauthor = rep(0, numauthor)
  # number of coauthors each author has
  numcoauthor = rep("", numauthor)
  
  
  
  # probability vectors
  
  # initialize P(xi), prior probabilities vector
  p_xi = rep(0, numauthor)
  # P(seen|Co, Xi)
  p_seen.co = rep(0, numauthor)
  # initialize P(A1k|xi), likelihood vector
  p_A1k.X = data.frame()
  # P(A1k|co, seen, Xi)
  p_A1k.seen = data.frame()
  # P(A1K|co, unseen, Xi)
  p_A1k.unseen = data.frame()
  # P(A1|X)
  p_A1.X = data.frame()
  
  
  # calculate number of paper each author.ID writes
  for (i in 1:numauthor)
  {
    numpaper[i] = sum(train$author.ID==i)
  }
  
  # number of coauthor an author has
  numpapercoauthor = numpaper - numpapernocoauthor
  
  
  # find the number of paper author writes alone
  for (i in 1:numauthor)
  {
    numpapernocoauthor[i] = sum(train$coauthor.names==""&train$author.ID==i)
  }
  
  
  
  # extract all coauthors and put them into a list
  coauthor.count = data.frame(cbind(train$author.ID, as.character(train$coauthor.names)))
  
  colnames (coauthor.count) = c("author.ID", "coauthor.names")
  
  coauthor.count = cSplit(coauthor.count, "coauthor.names", ",", "long")[
    , list(collaboaration.times = .N), .(author.ID, coauthor.names)][]
  
  
  # create a complete coauthor matrix
  coauthor.matrix = dcast(coauthor.count, author.ID~...)
  if("NA" %in% colnames(coauthor.matrix))
  {
    coauthor.matrix$`NA`=NULL
  }
  
  
  coauthor.matrix[is.na(coauthor.matrix)]=0
  
  
  # calculate P(seen|Co, xi) and store in coauthor.matrix$seen
  for (i in 1: numauthor)
  {
    p_seen.co[i] = sum(as.numeric(coauthor.matrix[i,-1])>1) / sum(as.numeric(coauthor.matrix[i,-1])>0)
  }
  
  
  
  
  
  # filter only coauthors with collaboration times >1
  seen <- coauthor.count[coauthor.count$collaboaration.times!=1,]
  # convert this count list to a dataframe
  coauthor.seen.matrix <- dcast(seen, author.ID~...) 
  # change NAs to zero
  coauthor.seen.matrix[is.na(coauthor.seen.matrix)]<- 0
  if("NA" %in% colnames(coauthor.seen.matrix))
  {
    coauthor.seen.matrix$`NA`=NULL
  }
  
  
  
  
  # calculate P(xi)
  p_xi = table(train$author.ID)
  
  # P(N|xi), probability of writing next paper alone
  p_numpapernocoauthor = numpapernocoauthor/numpaper
  
  
  # generate an empty matrix with same dim with coauthor.seen.matrix
  p_A1k.seen <- data.frame(matrix(rep(0,ncol(coauthor.seen.matrix)*numauthor),nrow = numauthor))
  
  ID <- coauthor.matrix$author.ID
  p_A1k.seen <- data.frame(ID, p_A1k.seen)
  colnames(p_A1k.seen) <- colnames(coauthor.seen.matrix)
  row.names(p_A1k.seen) <- coauthor.matrix$author.ID
  
  # calculate P(Aik|Seen,Co, xi) and store in A
  
  
  # remove column titled "NA"
  p_A1k.seen <- p_A1k.seen[!is.na(names(p_A1k.seen))]
  coauthor.seen.matrix = coauthor.seen.matrix[!is.na(names(coauthor.seen.matrix))]
  
  
  p_A1k.seen=as.data.frame(p_A1k.seen, stringsAsFactors = False)
  
  for (i in 1: nrow(coauthor.seen.matrix)) {
    for (j in 2: ncol(coauthor.seen.matrix)) {
      
      if(coauthor.seen.matrix[i,j] !=0)
        p_A1k.seen[i,j] <- as.numeric(coauthor.seen.matrix[i,j])/sum(as.numeric(coauthor.seen.matrix[i,-1]))  
      else p_A1k.seen[i,j] =  (1/(ncol(coauthor.seen.matrix)-1))/(1+sum(as.numeric(coauthor.seen.matrix[i,-1])))
      
      
    }
  }
  
  
  
  
  
  
  # calculate P(A1k|co, unseen, xi)
  
  author.coauthor.total = ncol(coauthor.matrix) + numauthor
  
  for (i in 1: numauthor)
  {
    numcoauthor[i] = sum(coauthor.matrix[i,-1]!=0)
  }
  
  p_A1k.unseen = 1/(author.coauthor.total - as.numeric(numcoauthor))
  
  
  
  
  
  ## Caculate P(A1k|Xi)
  
  # P(A1k|Seen,Co,Xi)
  #dim(p_A1k.seen) #26*246
  p_A1k.seen <- as.matrix(p_A1k.seen)
  # P(Seen|Co,Xi)
  # p_seen.co <- coauthor.matrix$seen
  #length(p_seen.co) #26*1
  # P(Co|Xi)
  p_coauthor <- 1-p_numpapernocoauthor #26*1
  
  # P(A1k|Unseen,Co,Xi)
  #dim(p_A1k.unseen) # 26*1
  p_A1k.unseen <- as.matrix(p_A1k.unseen)
  # P(Unseen|Co,Xi)
  p_unseen.co <- 1-p_seen.co
  #length(p_unseen.co) # 26*1
  # P(Co|Xi) # 26*1
  
  ###
  # Caculate P(A1k|Xi)
  p_A1k.X <- data.frame (matrix(rep(0,(ncol(p_A1k.seen)-1)*numauthor),nrow = numauthor))# 26*245
  p_A1k.seen <- data.frame(p_A1k.seen)
  ID <- coauthor.matrix$ author.ID
  #ID <- p_A1k.seen$author.ID
  p_A1k.X <- data.frame(ID,p_A1k.X)
  colnames(p_A1k.X) <- colnames(p_A1k.seen)
  
  
  
  
  for (i in 2: ncol(p_A1k.X)) 
  {
    p_A1k.X[,i] <-  as.numeric(as.matrix(p_A1k.seen[,i]))*p_seen.co * p_coauthor +
      p_A1k.unseen * p_unseen.co * p_coauthor 
  }
  
  
  
  ## Caculate P(A1|Xi) = P(A11|Xi)...P(A1k|Xi)...P(A1K|Xi)
  
  p_A1k.X.logged = log(p_A1k.X[, -1])
  
  p_A1.X.logged <- rowSums(p_A1k.X.logged)
  
  
  p_A1.X.logged <- data.frame(ID,p_A1.X.logged)
  colnames(p_A1.X.logged) <- c('ID','log(P(A1|Xi))')
  
  
  ##############
  
  ## Trying: vocab matrix for paper
  # extract all paper titles and put them into a list
  paper.count <- data.frame(cbind(train$author.ID, as.character(train$paper.title)))
  colnames (paper.count) = c("author.ID", "Paper.title")
  
  # clean paper title
  library(tm)
  library(tidytext)
  str(paper.count$Paper.title)
  
  corpus <- Corpus(VectorSource(paper.count$Paper.title))
  corpus <- tm_map(corpus, tolower)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, PlainTextDocument)
  corpus <- tm_map(corpus, removeWords, stopwords('english'))
  corpus <- tm_map(corpus, stemDocument)
  
  paper.count[,2] <- data.frame(corpus$content$content)
  
  # caculate number of words in sentence and store in paper.count
  library(stringr)
  
  word.count <- data.frame(matrix(rep(0,nrow(paper.count)),nrow = nrow(paper.count)))
  for(i in 1: nrow(paper.count)){
    word.count[i,] <- str_count(paper.count[i,2], '\\s+')+1
  }
  colnames(word.count) <-'word.count'
  paper.count <- data.frame(paper.count,word.count)
  
  # split sentence into word and store in voc.count
  List <- strsplit(as.character(paper.count$Paper.title), " ")
  voc.count <- data.frame(author.ID=rep(paper.count$author.ID, sapply(List, length)), Words=unlist(List))
  
  # generate a paper matrix
  library(reshape2)
  paper.matrix <-dcast(voc.count, author.ID~Words)
  paper.matrix[is.na(paper.matrix)]=0
  
  ## calculate P(A2k| Xi)
  # generate an empty matrix with same dim with paper.matrix
  
  p_A2k.X <- data.frame(matrix(rep(0,ncol(paper.matrix)*nrow(paper.matrix)),nrow = nrow(paper.matrix)))
  author.ID <- paper.matrix$author.ID
  p_A2k.X <- data.frame(author.ID, p_A2k.X)
  colnames(p_A2k.X) <- colnames(paper.matrix)
  
  p_A2k.X <- p_A2k.X[!is.na(names(p_A2k.X))]
  paper.matrix <- paper.matrix[!is.na(names(paper.matrix))]
  
  
  
  # lol
  for (i in 1: nrow(p_A2k.X)) {
    for (j in 2: ncol(p_A2k.X)) {
      
      if(paper.matrix[i,j] != 0)
        p_A2k.X[i,j] <- as.numeric(paper.matrix[i,j])/sum(as.numeric(paper.matrix[i,]))
      else p_A2k.X[i,j] <- (1/(ncol(paper.matrix)-1))/(1+sum(as.numeric(paper.matrix[i,])))
    }
  }
  # lol
  
  p_A2k.X.logged = log(p_A2k.X[, -1])
  
  p_A2.X.logged <- rowSums(p_A2k.X.logged)
  
  
  p_A2.X.logged <- data.frame(ID,p_A2.X.logged)
  colnames(p_A2.X.logged) <- c('ID','log(P(A2|Xi))')
  
  ##############
  ## Trying: vocab matrix for journal
  # extract all paper titles and put them into a list
  paper.count <- data.frame(cbind(train$author.ID, as.character(train$journal.title)))
  colnames (paper.count) = c("author.ID", "journal.title")
  
  # clean journal title
  str(paper.count$journal.title)
  
  corpus <- Corpus(VectorSource(paper.count$journal.title))
  corpus <- tm_map(corpus, tolower)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, PlainTextDocument)
  corpus <- tm_map(corpus, removeWords, stopwords('english'))
  corpus <- tm_map(corpus, stemDocument)
  
  paper.count[,2] <- data.frame(corpus$content$content)
  
  # caculate number of words in sentence and store in paper.count
  library(stringr)
  
  word.count <- data.frame(matrix(rep(0,nrow(paper.count)),nrow = nrow(paper.count)))
  for(i in 1: nrow(paper.count)){
    word.count[i,] <- str_count(paper.count[i,2], '\\s+')+1
  }
  colnames(word.count) <-'word.count'
  paper.count <- data.frame(paper.count,word.count)
  
  # split sentence into word and store in voc.count
  List <- strsplit(as.character(paper.count$journal.title), " ")
  voc.count <- data.frame(author.ID=rep(paper.count$author.ID, sapply(List, length)), Words=unlist(List))
  
  # generate a paper matrix
  library(reshape2)
  journal.matrix <-dcast(voc.count, author.ID~Words)
  journal.matrix[is.na(journal.matrix)]=0
  
  ## calculate P(A2k| Xi)
  # generate an empty matrix with same dim with journal.matrix
  
  p_A3k.X <- data.frame(matrix(rep(0,ncol(journal.matrix)*nrow(journal.matrix)),nrow = nrow(journal.matrix)))
  author.ID <- journal.matrix$author.ID
  p_A3k.X <- data.frame(author.ID, p_A3k.X)
  colnames(p_A3k.X) <- colnames(journal.matrix)
  
  p_A3k.X <- p_A3k.X[!is.na(names(p_A3k.X))]
  journal.matrix <- journal.matrix[!is.na(names(journal.matrix))]
  
  
  for (i in 1: nrow(p_A3k.X)) {
    for (j in 2: ncol(p_A3k.X)) {
      if(journal.matrix[i,j] != 0)
        p_A3k.X[i,j] <- as.numeric(journal.matrix[i,j])/sum(as.numeric(journal.matrix[i,]))
      else p_A3k.X[i,j] <- (1/(ncol(journal.matrix)-1))/(1+sum(as.numeric(journal.matrix[i,])))
    }
  }
  
  
  p_A3k.X.logged = log(p_A3k.X[, -1])
  
  p_A3.X.logged <- rowSums(p_A3k.X.logged)
  
  
  p_A3.X.logged <- data.frame(ID,p_A3.X.logged)
  colnames(p_A2.X.logged) <- c('ID','log(P(32|Xi))')
  
  
  df = cbind(p_A1k.X.logged, p_A2k.X.logged, p_A3k.X.logged)
  
  return(df)
  
}
