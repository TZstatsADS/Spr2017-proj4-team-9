nb.test = function(df, test)
{
  library(tm)
  # get rid of leading and trailing white space
  trim.leading <- function (x)  sub("^\\s+", "", x)
  trim.trailing <- function (x) sub("\\s+$", "", x)
  
  # this vector holds the final result of the test: rows correspond to author ID, columns correspond to a paper
  final.result = data.frame(matrix(ncol = 0, nrow =  max(test$author.ID)))
  
  truth = test$author.ID
  ###################
  # take one paper at a time
    for( i in 1:nrow(test)) 
      {
       # this holds the result from one paper: row corresponds to author ID, columns correspond to logged probability of feature (A1, A2, A3, but so far only A1 is there)
       current.result = data.frame(matrix(ncol = 0, nrow =  max(test$author.ID)))
       # this holds one paper
       current = test[i, ]
       #take coauthor list from the one paper and preprocess: get rid of leading/trailing white space, and replace space with a dot.
       if(current$coauthor.names!="")
       {
         coauthor.list = trim.trailing(trim.leading(unlist(strsplit(as.character(current$coauthor.names), ","))))
         coauthor.list = sub("\\s+", ".", coauthor.list)  
       
         # for each coauthor, get the logged probability
         for (j in 1:length(coauthor.list))
        { # 489 unique coauthors
           not_null = !(coauthor.list[j] !="")
           in_colnames = ifelse(not_null, (coauthor.list[j] %in% colnames(df)), FALSE)
           in_colnames = ifelse(is.na(in_colnames), FALSE, coauthor.list[j] %in% colnames(df))
           
           if(in_colnames)
           {# cbind the result of logged probability of each coauthor.
             
             current.result = cbind(current.result, as.data.frame(df[, coauthor.list[j]]))
           }
         
        }
         
       }
       # rind the transposed column sum of "current.result". This is the probability for A1. 
       final.result = rbind(final.result, t(rowSums(current.result)),t(df$Freq))
        
     }
     
 #################
  for( i in 1:nrow(test)) 
  {
    # this holds the result from one paper: row corresponds to author ID, columns correspond to logged probability of feature (A1, A2, A3, but so far only A1 is there)
    current.result = data.frame(matrix(ncol = 0, nrow =  max(test$author.ID)))
    # this holds one paper
    current = test[i, ]
    
    #take coauthor list from the one paper and preprocess: get rid of leading/trailing white space, and replace space with a dot.
    
    if(current$paper.title!="")
    {
      paper.list = trim.trailing(trim.leading(unlist(strsplit(as.character(current$paper.title), ","))))
      
      
      
      
      # for each keyword, get the logged probability
      for (j in 1:length(paper.list))
      { # 489 unique coauthors
        not_null = !(paper.list[j] !="")
        in_colnames = ifelse(not_null, (paper.list[j] %in% colnames(df)), FALSE)
        in_colnames = ifelse(is.na(in_colnames), FALSE, paper.list[j] %in% colnames(df))
        
        if(in_colnames)
        {# cbind the result of logged probability of each paper title keyword.
          
          current.result = cbind(current.result, as.data.frame(df[, paper.list[j]]), df$Freq)
        }
        
      }
      
    }
    # rind the transposed column sum of "current.result". This is the probability for A1. 
    final.result = rbind(final.result, t(rowSums(current.result)),t(df$Freq))
    
  }
  
  ##########################
  
  for( i in 1:nrow(test)) 
  {
    # this holds the result from one paper: row corresponds to author ID, columns correspond to logged probability of feature (A1, A2, A3, but so far only A1 is there)
    current.result = data.frame(matrix(ncol = 0, nrow =  max(test$journal.title)))
    # this holds one paper
    current = test[i, ]
    
    #take coauthor list from the one paper and preprocess: get rid of leading/trailing white space, and replace space with a dot.
    
    if(current$journal.title!="")
    {
      journal.list = trim.trailing(trim.leading(unlist(strsplit(as.character(current$journal.title), ","))))
      
      
      
      
      # for each coauthor, get the logged probability
      for (j in 1:length(journal.list))
      { # 489 unique coauthors
        not_null = !(journal.list[j] !="")
        in_colnames = ifelse(not_null, (journal.list[j] %in% colnames(df)), FALSE)
        in_colnames = ifelse(is.na(in_colnames), FALSE, journal.list[j] %in% colnames(df))
        
        if(in_colnames)
        {# cbind the result of logged probability of each coauthor.
          
          current.result = cbind(current.result, as.data.frame(df[, journal.list[j]]), df$Freq)
        }
        
      }
      
    }
    # rind the transposed column sum of "current.result". This is the probability for A1. 
    final.result = rbind(final.result, t(rowSums(current.result)),t(df$Freq))
    
  }
  #####################
  
  
   
  
  
  
  
  
  
  answer = matrix()
  for(i in 1:nrow(test))
  {
    answer[i] = which.max(as.numeric(final.result[i, ]))
  }
  
  presentation = cbind(as.data.frame(answer), as.data.frame(truth))
  
  return(presentation)
  
  
}