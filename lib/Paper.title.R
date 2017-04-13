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

