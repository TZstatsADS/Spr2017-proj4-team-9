## check dim ##
dim(coauthor.matrix) # 26*482
# author-ID, seen, NA
# number of coauthor = 482-3=479

nrow(coauthor.count)# 489
nrow(coauthor.count[coauthor.count$collaboaration.times==1,]) # 240
# seen coauthor
nrow(coauthor.count[coauthor.count$collaboaration.times!=1,]) # 249

#--------------------------------------------------------------------#
## Trying: P(Aik|Seen,Co, xi)
# generate a seen coauthor count matrix
seen <- coauthor.count[coauthor.count$collaboaration.times!=1,]
coauthor.seen.matrix <- dcast(seen, author.ID~...) 
coauthor.seen.matrix[is.na(coauthor.seen.matrix)]<- 0
row.names(coauthor.seen.matrix) <- coauthor.seen.matrix$author.ID
coauthor.seen.matrix <- coauthor.seen.matrix[,-1]
dim(coauthor.seen.matrix)# 26*245

# generate an empty matrix with same dim with coauthor.seen.matrix
A <- data.frame(matrix(rep(0,ncol(coauthor.seen.matrix)*26),nrow = 26))
colnames(A) <- colnames(coauthor.seen.matrix)
row.names(A) <- rownames(coauthor.seen.matrix)
dim(A)# 26*245

# calculate P(Aik|Seen,Co, xi) and store in A
for (i in 1: nrow(A)) {
  for (j in 2: ncol(A)) {
    A[i,j] <- as.numeric(coauthor.seen.matrix[i,j])/sum(as.numeric(coauthor.seen.matrix[i,]))
  }
}
A

#-------------------------------------------------------------------------------#
## calculate P(Aik|Seen,Co, xi)

# filter only coauthors with collaboration times >1
seen <- coauthor.count[coauthor.count$collaboaration.times!=1,]
# convert this count list to a dataframe
coauthor.seen.matrix <- dcast(seen, author.ID~...) 
# change NAs to zero
coauthor.seen.matrix[is.na(coauthor.seen.matrix)]<- 0

# generate an empty matrix with same dim with coauthor.seen.matrix
p_A1k.seen <- data.frame(matrix(rep(0,ncol(coauthor.seen.matrix)*numauthor),nrow = numauthor))

ID <- coauthor.seen.matrix$author.ID
p_A1k.seen <- data.frame(ID, p_A1k.seen)
colnames(p_A1k.seen) <- colnames(coauthor.seen.matrix)
row.names(p_A1k.seen) <- coauthor.seen.matrix$author.ID

# calculate P(Aik|Seen,Co, xi) and store in A
# remove column titled "NA"
p_A1k.seen <- p_A1k.seen[!is.na(names(p_A1k.seen))]
coauthor.seen.matrix = coauthor.seen.matrix[!is.na(names(coauthor.seen.matrix))]


for (i in 1: nrow(p_A1k.seen)) {
  for (j in 2: ncol(p_A1k.seen)) {
    p_A1k.seen[i,j] <- as.numeric(coauthor.seen.matrix[i,j])/sum(as.numeric(coauthor.seen.matrix[i,]))
  }
}
p_A1k.seen # 26*246

#-------------------------------------------------------------------------------#
## Trying: P(Aik|unSeen,Co, xi)

# estimate P (A1k |U nseen, Co, Xi) as 
# 1 divided by the total number of author (or coauthor) names in the training citations minus the number of coauthors of Xi .

# total number of author (or coauthor) names in the training citations
# generate an empty matrix for total number of author
T <- data.frame(matrix(rep(0,numauthor),nrow = numauthor))
colnames(T) <- 'total number of author'

for (i in 1: numauthor){
T[i,] <- sum(as.numeric(coauthor.matrix[i,-1])>0)
}
#dim(T) 26*1

# generate an empty matrix with same dim of coauthor.matrix
p_Aik.unseen <- data.frame(matrix(rep(0,(ncol(coauthor.matrix)-1)*numauthor),nrow = numauthor))
ID <- coauthor.matrix$author.ID
p_Aik.unseen <- data.frame(ID, p_Aik.unseen)
#dim(p_Aik.unseen)# 26*482
colnames(p_Aik.unseen) <- colnames(coauthor.matrix)
row.names(p_Aik.unseen) <- coauthor.matrix$author.ID

for(i in 1:numauthor){
  for(j in 2:ncol(p_Aik.unseen)){
    p_Aik.unseen[i,j] <- T[i,]-coauthor.matrix[i,j]
    p_Aik.unseen[i,j]<- 1/(p_Aik.unseen[i,j])
  }
}

#-------------------------------------------------------------------------------#
## Caculate P(A1k|Xi)

# P(A1k|Seen,Co,Xi)
#dim(p_A1k.seen) #26*246
p_A1k.seen <- as.matrix(p_A1k.seen)
# P(Seen|Co,Xi)
p_seen.co <- coauthor.matrix$seen
#length(p_seen.co) #26*1
# P(Co|Xi)
p_coauthor <- 1-p_nocoauthor #26*1

# P(A1k|Unseen,Co,Xi)
#dim(p_A1k.unseen) # 26*1
p_A1k.unseen <- as.matrix(p_A1k.unseen)
# P(Unseen|Co,Xi)
p_unseen.co <- 1-p_seen.co
#length(p_unseen.co) # 26*1
# P(Co|Xi) # 26*1

###
# Caculate P(A1k|Xi)
p_A1k.X <- data.frame(matrix(rep(0,(ncol(p_A1k.seen)-1)*numauthor),nrow = numauthor))# 26*245
p_A1k.seen <- data.frame(p_A1k.seen)
ID <- p_A1k.seen$author.ID
p_A1k.X <- data.frame(ID,p_A1k.X)
colnames(p_A1k.X) <- colnames(p_A1k.seen)

for (i in 2: ncol(p_A1k.X)) {
  p_A1k.X[,i] <- as.numeric(p_A1k.seen[,i])*p_seen.co*p_coauthor+
    p_A1k.unseen*p_unseen.co*p_coauthor 
}

## Caculate P(A1|Xi) = P(A11|Xi)...P(A1k|Xi)...P(A1K|Xi)
#install.packages('matrixStats')
library(matrixStats)
p_A1.X <- rowProds(as.matrix(p_A1k.X[,-1]))
p_A1.X <- data.frame(ID,p_A1.X)
colnames(p_A1.X) <- c('ID','P(A1|Xi)')

#-------------------------------------------------------------------------------#
## Trying: vacob for paper
paper_train <- itoken(as.character(gupta$paper.title), 
                   preprocessor = tolower, 
                   tokenizer = word_tokenizer,
                   ids = gupta$paper.ID,
                   # turn off progressbar because it won't look nice in rmd
                   progressbar = FALSE)
vocab <- create_vocabulary(paper_train, stopwords = c("a", "an", "the", "in", "on",
                                                   "at", "of", "above", "under"))

vocab

vectorizer <- vocab_vectorizer(vocab)
dtm_train <- create_dtm(paper_train, vectorizer)
dtm_train
dim(dtm_train)

#-------------------------------------------------------------------------------#
## Trying: vocab matrix for paper
# extract all paper titles and put them into a list
paper.count <- data.frame(cbind(gupta$paper.ID, as.character(gupta$paper.title)))
colnames (paper.count) = c("Paper.ID", "Paper.title")

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
voc.count <- data.frame(Paper.ID=rep(paper.count$Paper.ID, sapply(List, length)), Words=unlist(List))

# generate a paper matrix
library(reshape2)
paper.matrix <-dcast(voc.count, Paper.ID~Words)
paper.matrix[is.na(paper.matrix)]=0

## calculate P(A2k| Xi)
# generate an empty matrix with same dim with paper.matrix

p_A2k.X <- data.frame(matrix(rep(0,ncol(paper.matrix)*nrow(paper.matrix)),nrow = nrow(paper.matrix)))
Paper.ID <- paper.matrix$Paper.ID
p_A2k.X <- data.frame(Paper.ID, p_A2k.X)
colnames(p_A2k.X) <- colnames(paper.matrix)

p_A2k.X <- p_A2k.X[!is.na(names(p_A2k.X))]
paper.matrix <- paper.matrix[!is.na(names(paper.matrix))]


for (i in 1: nrow(p_A2k.X)) {
  for (j in 2: ncol(p_A2k.X)) {
    p_A2k.X[i,j] <- as.numeric(p_A2k.X[i,j])/sum(as.numeric(p_A2k.X[i,]))
  }
}

#-------------------------------------------------------------------------------#
log.p_A1k.X <- log(as.matrix(p_A1k.X[,-1]))

log.p_A2k.X <- log(as.matrix(p_A2k.X[,-1]))
log.p_A3k.X <- log(as.matrix(p_A3k.X[,-1]))

p_A1.X <- list()
for (i in 1: nrow(log.p_A1k.X)) {
    p_A1.X[[i]] <- sum(log.p_A1k.X[i,1:ncol(log.p_A1k.X)])
}

#####################

for (i in 1: nrow(p_A2k.X)) {
  for (j in 2: ncol(p_A2k.X)) {
    
    if(paper.matrix[i,j] != 0)
    p_A2k.X[i,j] <- as.numeric(paper.matrix[i,j])/sum(as.numeric(paper.matrix[i,]))
    else p_A2k.X[i,j] <- (1/(ncol(paper.matrix)-1))/(1+sum(as.numeric(paper.matrix[i,])))
  }
}

for (i in 1: nrow(p_A3k.X)) {
  for (j in 2: ncol(p_A3k.X)) {
    if(journal.matrix[i,j] != 0)
    p_A3k.X[i,j] <- as.numeric(journal.matrix[i,j])/sum(as.numeric(journal.matrix[i,]))
    else p_A3k.X[i,j] <- (1/(ncol(journal.matrix)-1))/(1+sum(as.numeric(journal.matrix[i,])))
  }
}


# tesing cases

test.coauthor = "B.Kvande"
test.journal = "access"
test.paper = "abstract"

if(test.coauthor %in% colnames(p_A1k.X))
{
  p_A1k = p_A1k.X[, test.coauthor]
}

if(test.journal %in% colnames(p_A2k.X))
{
  p_A2k = p_A2k.X[, test.journal]
}


if(test.paper %in% colnames(p_A3k.X))
{
  p_A3k = p_A3k.X[, test.paper]
}

# P <- log(p_A1k) + log(p_A2k) + log(p_A3k)
P <- p_A1k + p_A2k + p_A3k
P <- data.frame(author.ID,P)
output <- P$author.ID[which(P[,2]==max(P$P))]
output

#-------------------------------------------------------------------------------#
## Naive.Bayes function for gupta

# number vectors
numauthor <- max(gupta$author.ID)#26
numpaper <-  rep(0, numauthor)
numpapercoauthor = rep(0, numauthor)
numpapernocoauthor = rep(0, numauthor)
numcoauthor = rep("", numauthor)

# probability vectors
p_xi = rep(0, numauthor)
p_A1k.X = data.frame()
p_A1k.seen = data.frame()
p_A1k.unseen = data.frame()
p_A1.X = data.frame()

dim(gupta) #577*7
set.seed(1)
#train.index <- sample(1:nrow(gupta),300,replace = F)
#train <- gupta[train.index,]
#test <- gupta[-train.index,]

#length(which(gupta$author.ID==1))
# for ID=1
train <- list()
# all rows with author.ID=1
train.1 <- gupta[as.numeric(rownames(gupta)[which(gupta$author.ID==1)]),]
rownames(train.1) <- 1:nrow(train.1)

train.index <- sample(1:length(which(gupta$author.ID==1)),length(which(gupta$author.ID==1))/2,replace = F)
train <- train.1[train.index,]
test <- train.1[-train.index,]
rownames(train) <- 1: nrow(train)
rownames(test) <- 1: nrow(test)

# generate a list for train and test w.r.t author ID
train.1 <- list()
train <- list()
test <- list()
train.index <- list()
for (i in 1:max(gupta$author.ID)) {
  train.1[[i]] <- gupta[as.numeric(rownames(gupta)[which(gupta$author.ID==i)]),]
  rownames(train.1[[i]]) <- 1:nrow(train.1[[i]])
  train.index[[i]] <- sample(1:length(which(gupta$author.ID==i)),floor(length(which(gupta$author.ID==i))/2),replace = F)
  train[[i]] <- train.1[[i]][rownames(which(train.1[[i]]) == train.index[[i]]),]
  test[[i]] <- train.1[[i]][rownames(which(train.1[[i]]) != train.index[[i]]),]
}

# combine all list together
data.train <- do.call('rbind',train) #281
data.test <- do.call('rbind',test) 

train <- list()
test <- list()
train.index <- list()
for (i in 1:max(gupta$author.ID)){
  train[[i]] <- gupta[as.numeric(rownames(gupta)[which(gupta$author.ID==i)]),]
  rownames(train[[i]]) <- 1:nrow(train[[i]])
  train.index[[i]] <- sample(1:length(which(gupta$author.ID==i)),floor(length(which(gupta$author.ID==i))/2),replace = F)
  train[[i]] <- train[[i]][,]
}


# check
nrow(gupta[as.numeric(rownames(gupta)[which(gupta$author.ID==1)]),])#108
length(train.index[[1]]) # 54
nrow(train[[1]]) #54
nrow(test[[1]]) #25


Naive.Bayes <- function(train,test){
  
  # INPUT: train data frame: coauthor, journal, paper
  # OUTPUT: author ID with biggest probability
  
  for (i in 1:numauthor){numpaper[i] = sum(train$author.ID==i)}
  numpapercoauthor = numpaper - numpapernocoauthor
  for (i in 1:numauthor){numpapernocoauthor[i] = sum(train$coauthor.names==""&train$author.ID==i)}
  
  coauthor.count = data.frame(cbind(train$author.ID, as.character(train$coauthor.names)))
  colnames (coauthor.count) = c("author.ID", "coauthor.names")
  coauthor.count = cSplit(coauthor.count, "coauthor.names", ",", "long")[, list(collaboaration.times = .N), .(author.ID, coauthor.names)][]
  
  coauthor.matrix = dcast(coauthor.count, author.ID~...)
  coauthor.matrix[is.na(coauthor.matrix)]=0
  # calculate P(seen|Co, xi) and store in coauthor.matrix$seen
  #####
  for (i in 1: nrow(coauthor.matrix)){ coauthor.matrix$seen[i] = sum(as.numeric(coauthor.matrix[i,-1])>1) / sum(as.numeric(coauthor.matrix[i,-1])>0)}
  
  seen <- coauthor.count[coauthor.count$collaboaration.times!=1,]
  coauthor.seen.matrix <- dcast(seen, author.ID~...) 
  coauthor.seen.matrix[is.na(coauthor.seen.matrix)]<- 0

  p_xi = table(train$author.ID)
  p_numpapernocoauthor = numpapernocoauthor/numpaper
  
  # generate an empty matrix with same dim with coauthor.seen.matrix
  p_A1k.seen <- data.frame(matrix(rep(0,ncol(coauthor.seen.matrix)*nrow(coauthor.seen.matrix)),nrow = nrow(coauthor.seen.matrix)))
  ID <- coauthor.seen.matrix$author.ID
  p_A1k.seen <- data.frame(ID, p_A1k.seen)
  colnames(p_A1k.seen) <- colnames(coauthor.seen.matrix)
  row.names(p_A1k.seen) <- coauthor.seen.matrix$author.ID
  
  # calculate P(Aik|Seen,Co, xi) and store in A
  p_A1k.seen <- p_A1k.seen[!is.na(names(p_A1k.seen))]
  coauthor.seen.matrix = coauthor.seen.matrix[!is.na(names(coauthor.seen.matrix))]
  for (i in 1: nrow(p_A1k.seen)) {
    for (j in 2: ncol(p_A1k.seen)) {
      p_A1k.seen[i,j] <- as.numeric(coauthor.seen.matrix[i,j])/sum(as.numeric(coauthor.seen.matrix[i,]))}}
  
  # calculate P(A1k|co, unseen, xi)
  author.coauthor.total = ncol(coauthor.matrix) + numauthor
  for (i in 1:nrow(coauthor.matrix)){numcoauthor[i] = sum(coauthor.matrix[i,-1]!=0)}
  p_A1k.unseen = 1/(author.coauthor.total - as.numeric(numcoauthor))
  p_A1k.unseen <- p_A1k.unseen[!is.na(p_A1k.unseen)]
  
  # Caculate P(A1k|Xi)
  p_A1k.seen <- as.matrix(p_A1k.seen) #19*130
  p_seen.co <- coauthor.matrix$seen #25
  p_coauthor <- 1-p_numpapernocoauthor # 25
  p_A1k.unseen <- as.matrix(p_A1k.unseen) # 25*1
  p_unseen.co <- 1-p_seen.co #25
}
