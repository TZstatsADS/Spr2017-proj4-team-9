

library(stringr)



data.lib="/Users/xuehan/Desktop/Spr2017-proj4-team-9/data/nameset"
data.files=list.files(path=data.lib, "*.txt")

data.files

## remove "*.txt"
query.list=substring(data.files, 
                     1, nchar(data.files)-4)

query.list

## add a space
query.list=paste(substring(query.list, 1, 1), 
                 " ", 
                 substring(query.list, 
                           2, nchar(query.list)),
                 sep=""
)

query.list




f.line.proc=function(lin, nam.query="."){
  
  # remove unwanted characters
  char_notallowed <- "\\@#$%^&?" # characters to be removed
  lin.str=str_replace(lin, char_notallowed, "")
  
  # get author id
  lin.str=strsplit(lin.str, "_")[[1]]
  author_id=as.numeric(lin.str[1])
  
  # get paper id
  lin.str=lin.str[2]
  paper_id=strsplit(lin.str, " ")[[1]][1]
  lin.str=substring(lin.str, nchar(paper_id)+1, nchar(lin.str))
  paper_id=as.numeric(paper_id)
  
  # get coauthor list
  lin.str=strsplit(lin.str, "<>")[[1]]
  coauthor_list=strsplit(lin.str[1], ";")[[1]]
  
  #print(lin.str)
  for(j in 1:length(coauthor_list)){
    if(nchar(coauthor_list[j])>0){
      nam = strsplit(coauthor_list[j], " ")[[1]]
      if(nchar(nam[1])>0){
        first.ini=substring(nam[1], 1, 1)
      }else{
        first.ini=substring(nam[2], 1, 1)
      }
    }
    last.name=nam[length(nam)]
    nam.str = paste(first.ini, last.name)
    coauthor_list[j]=nam.str
  }
  
  match_ind = charmatch(nam.query, coauthor_list, nomatch=-1)
  
  #print(nam.query)
  #print(coauthor_list)
  #print(match_ind)
  
  if(match_ind>0){
    
    coauthor_list=coauthor_list[-match_ind]
  }
  
  paper_title=lin.str[2]
  journal_name=lin.str[3]
  
  list(author_id, 
       paper_id, 
       coauthor_list, 
       paper_title, 
       journal_name)
}




data_list=list(1:length(data.files))

for(i in 1:length(data.files)){
  
  ## Step 0 scan in one line at a time.
  
  dat=as.list(readLines(paste(data.lib, data.files[i], sep="/")))
  #ASDFASDF
  
  
  data_list[[i]]=lapply(dat, f.line.proc, nam.query=query.list[i])
  
  
}






for (i in 1:length(query.list))
{
  
  mat=data_list[[i]]
  
  ## Turn nested list into one data frame:
  textFileDfList <- lapply(mat,  function(listLevel3){
    ## Paste multiple entries (e.g. vector of co-authors)
    ## together to create a single character entry:
    simplifiedList <- lapply(listLevel3, 
                             function(entries) paste(entries, collapse = ", "))
    ## Create data.frame:
    outDf <- as.data.frame(simplifiedList, 
                           stringsAsFactors = FALSE, 
                           col.names =  c("author ID", "paper ID", "coauthor names", 
                                          "paper title", "journal title")                                                    
    )
    
    ## Combine data frames of the single entries to one data frame,
    ## containing all entries of the text file:
    textFileDf <- do.call('cbind', outDf)           
  })
  ## Combine data frames of the text files to one big data frame:
  bigDataFrame <- as.data.frame(do.call('rbind', textFileDfList))
  bigDataFrame$author.names = query.list[i]
  write.csv(bigDataFrame, file = paste("/Users/xuehan/Desktop/Spr2017-proj4-team-9/data/nameset/", query.list[i], ".csv", sep=""))
}





