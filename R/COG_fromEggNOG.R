# df <- readr::read_tsv("new.faa.emapper.annotations",col_names = F)

Cog_from_emapper <- function(df){
  
  df$COG <- ""
  
  for ( i in 1:nrow(df)){
    Cog_index <- colnames(df)[stringr::str_detect(df[i,],"COG\\d{4}") & !is.na(df[i,])]
    
    if(!identical(Cog_index, character(0))){
    
      if(!is.na(Cog_index) & !is.null(Cog_index)){
  
        df$COG[i] <- stringr::str_extract(df[i,Cog_index],"COG\\d{4}")
        
      }  
    }
    
  }
  return(df)
}

# df1 <- Cog_from_emapper(df)
# df1 <- COG_names(df1,UniqueCOGs = "COG")
# View(df1)
