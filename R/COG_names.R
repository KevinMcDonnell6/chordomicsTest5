#' @export
# 
# getwd()
# categories <- readr::read_tsv("COG_data/cognames2003-2014.tab")
# funGroups <- readr::read_tsv("COG_data/fun2003-2014.tab")
# colnames(categories)[1] <- "COG"  
# colnames(funGroups)[1] <- "Code"  
# 
# View(categories)
# View(funGroups)
# 
# 
# View(test)

COG_names <- function(df,UniqueCOGs = "UniqueCOGs"){
  
  df <- as.data.frame(df)
  
  len <- nrow(df)
  df$predicted.function <- ""
  df$GroupLetters <- ""
  df$group.functions <- ""
  
  for(i in 1:len){
    # print(df$Group)
    if(as.character(df[,UniqueCOGs][i]) != "" & any(stringr::str_detect(categories$COG,as.character(df[,UniqueCOGs][i])))){
      # print(i)
      res <- categories[stringr::str_detect(categories$COG,as.character(df[,UniqueCOGs][i])),]
      df$predicted.function[i] <- res$name
      df$GroupLetters[i] <- res$func
      # print(df$GroupLetter[i])
      GL <- unlist(strsplit(df$GroupLetters[i],""))
      for(l in GL){
        
        res <- funGroups[stringr::str_detect(funGroups$Code,l),]
        # print(res$Name)
        df$group.functions[i] <- ifelse(df$group.functions[i]=="",
                                       res$Name,
                                       paste(df$group.functions[i],res$Name,sep = ";")
        )
        
      
        }
      
    }
    
  }
  
  df <- Unique_COG_cat(df)
  
  df$group.function <- ""
  
  for(i in 1:len){
    # print(df$Group)
    if(df$UniqueCOGcategory[i] != ""){
      res <- funGroups[stringr::str_detect(funGroups$Code,df$UniqueCOGcategory[i]),]
      # print(res$Name)
      df$group.function[i] <- res$Name
      
      }
    
    }
  
 return(df) 
}

# test.3 <- COG_names(test)
# View(test.3)
# test3 <- cbind(test.3,df1)
# test.31 <- test3[test3$group.function!="",]
# View(test3)#
# 
# write.csv(test3,"groupfuntestall.csv",row.names = F)
