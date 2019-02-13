# 
# df1 <- read.csv("day1R1.csv",stringsAsFactors = F)
# getwd()
# View(df1)

Get_COG <- function(df,UniprotColumn){
  len <- nrow(df)
  
  # df_tax <- data.frame(SuperKingdom=character(),Kingdom=character(),Phylum=character(),
  #                      Class=character(),Order=character(),
  #                      Family=character(),Genus=character(),
  #                      Species=character(),stringsAsFactors = F)
  df_cog <- data.frame(Uniprot = df[,UniprotColumn],
                       COG = "",
                       UniqueCOGs = "",
                       stringsAsFactors = F)
  # print(df_cog[,"Uniprot"])
  pb<- txtProgressBar(min = 0, max = len, style = 3)
  
  # UniprotColumn <- "Proteins"
  
  for(i in 1:len){
    setTxtProgressBar(pb, i)
    # i<-1
    
    UniprotIDs <- stringr::str_trim(unlist(strsplit(df_cog[i,"Uniprot"],",")))
    for(j in UniprotIDs)
    
      {
        # print(j)
        # LCA <- df$Lowest.Common.Ancestor[i]
        # Uni <- j
        url  <- "https://www.uniprot.org"
        columns <- paste("id","entry%20name","database(EGGNOG)",sep=",")
        
        path <- paste("uniprot/?query=",j,"&sort=score&columns=",columns,"&limit=1&format=tab",sep = "")
        
        
        # send request
        uni.raw.result <- httr::GET(url = url, path = path)
        # uni.raw.result
        
        if (uni.raw.result$status_code == 400){
          df_cog[i,COG] <- paste(df_cog[i,"COG"],"")
        }
        
        else{
          # convert returned data to charachters
          uni.this.raw.content <- rawToChar(uni.raw.result$content)
          # uni.this.raw.content
          # convert data to dataframe
          res <- read.delim(textConnection(uni.this.raw.content),sep="\t",stringsAsFactors = F)
          # res
          #df_cog[i,"COG"] <- paste(df_cog[i,"COG"],unlist(strsplit(as.character(res$Cross.reference..EGGNOG.),";"))[2])
          # print(paste(j,res$Cross.reference..EGGNOG.))
          if(stringr::str_detect(res$Cross.reference..EGGNOG.,"COG\\d{4}") & !is.na(res$Cross.reference..EGGNOG.)){
            df_cog[i,"COG"] <- ifelse(df_cog[i,"COG"] == "",
                                      stringr::str_extract(res$Cross.reference..EGGNOG.,"COG\\d{4}"),
                                      paste(df_cog[i,"COG"],stringr::str_extract(res$Cross.reference..EGGNOG.,"COG\\d{4}"))
            )
            
          
            }
          }
        
        # start_j <- str_which(as.character(df_tax[i,]),as.character(df$organism[i]))[1]#Lowest.Common.Ancestor[i]))[1]
        # if(!is.na(start_j) & start_j!=7){
        #   for(j in (start_j+1):7){df_tax[i,j]<- "Higher Taxa"}
        # }
    }
    
    df_cog[i,"UniqueCOGs"] <- ifelse(df_cog$COG[i]=="",
                                     "",
                                     unique(unlist(strsplit(df_cog$COG[i]," ")))
                                    )
  }
  
  
  
  return(df_cog)
  
}
# 
# test2 <- Get_COG(df1[52:57,],"Proteins")
# test <- Get_COG(df1,"Proteins")
# test
# View(test2)
