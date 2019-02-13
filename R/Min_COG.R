# df <- test.2

Unique_COG_cat <- function(df){

  
  # create empty names vector
  nameslist<- sort(unique(unlist(strsplit(df$GroupLetters,""))))
  
  # create matrix of T/F for prooteins vs columns
  TFmatrix<-data.frame(matrix(0,ncol = length(unique(nameslist)),nrow = length(df$GroupLetters)))
  
  
  # set matrix column names to pathways
  colnames(TFmatrix)<- nameslist
  
  
  # add T/F depending of function and protein
  for (fn in colnames(TFmatrix)){
    TFmatrix[,fn]<-stringr::str_detect(df$GroupLetters,
                         fn)
  }
  
  # convert NAs to False
  TFmatrix[is.na(TFmatrix)]<-F
  
  
  # remove functions with no match 
  TFmatrix2<- TFmatrix[apply(TFmatrix,1,sum)!=0,]

#########################################################
##########################################################


# Updated Algorithm

# opt_col <- function(df){
  
  # observe start time
  # t1<- Sys.time()
  
  # calculate the minimum number of pathways
  min_num <- function(df){
    
    # update counter
    i<<-i+1
    
    # if number rows inmatrix is zero, store solution
    if(nrow(df)==0){sol_list[[j]]<<-sol;j<<-j+1;return()} 
    
    # find max number of matching entries for each pathway
    M_all <- apply(df,2,function(x){sum(x)})
    
    # find max of all of these
    M_max <- max(M_all)
    
    
    # If the max is zero, return
    if(M_max==0){return()}
    
    # define the pathways that match the most entries
    M_cols <- seq(ncol(df))[M_all==M_max]
    
    
    # If Max of the columns is one
    # add functions singularly until all proteins have a match
    # Which one function chosen is redundant...
    
    if(M_max == 1){
      df_new <- df
      sol_new<-sol
      
      # add pathways to solution until all matched
      for( col in M_cols){
        if(sum(df_new[,col]) != 0){
          sol_new[i] <- col
          i <<- i+1
          df_new <- df_new[!df_new[,col],]
          
          
          # return when all matched
          if (nrow(df_new)==0){sol_list[[j]]<<-sol_new;j<<-j+1;i<<- i-(length(sol_new)-length(sol));
          return()}
        }
        
      }
      
    }
    
    
    else{
      
      # for each pathway
      for(col in M_cols){
        
        # add pathway to solution
        sol[i]<<-col
        
        # update matrix
        df_new <- df[!df[,col],]
        
        # apply algorithm again 
        min_num(df_new)
        
        # update counter
        i<<- i-1
      }
    }
  }
  
  # initialise counters
  i<-0
  j<-1
  
  # create solution lists
  sol_list<- list()
  sol<-numeric()
  
  # Apply algorithm
  min_num(TFmatrix2)
  
  
  # find lengths for all solutions
  len_all <-numeric()
  for(len in lapply(sol_list,function(x){length(x)})){
    len_all<-c(len_all,len)
  }
  
  # print runtime
  # t2<- Sys.time()
  # print(difftime(t2,t1))
  
  #return the shortest solution
  d <- (sol_list[seq(length(sol_list))[len_all==min(len_all)]])[[1]]
 
  sol_names <- sort(colnames(TFmatrix2)[d])
  
  
  # create column on original dataframe
  df$UniqueCOGcategory <- ""
  
  # assign most likely pathway to each function
  for(func in d){
    tf<- TFmatrix[,func]
    df[tf,"UniqueCOGcategory"]<- ifelse(df[tf,"UniqueCOGcategory"]=="",
                                               colnames(TFmatrix)[func],
                                               df[tf,"UniqueCOGcategory"])
  } 
  return(df)
}

# df <- Unique_COG_cat(df)
# #########################################################
# ##########################################################
# 
# 
# # apply algorithm to TF matrix
# d<- opt_col(TFmatrix2)
# 
# d
# 
# # return solution set (names)
# sort(colnames(TFmatrix2)[d])
# 
# 
# # create column on original dataframe
# TFmatrix2$UniqueCOGcategory <- ""
# 
# # assign most likely pathway to each function
# for(func in d){
#   tf<- TFmatrix[,func]
#   TFmatrix2[tf,"UniqueCOGcategory"]<- ifelse(TFmatrix2[tf,"UniqueCOGcategory"]=="",
#                                       colnames(TFmatrix)[func],
#                                       TFmatrix2[tf,"UniqueCOGcategory"])
# }
# View(df)
# # return sol