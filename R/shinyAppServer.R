
ChordshinyAppServer <- function(input, output) {
  
  
  
  #################################################################
  
  #################################################################
  #################### Get file names #######################
  # Reactive to store name of files
  file_name <- shiny::reactive({
    inFile <- input$files
    numberOfFiles <- length(input$files$datapath)
    if (is.null(inFile))
      return()
    
    names_ <- character()
    for(i in numberOfFiles){
      df_<- read.csv(input$files$datapath[i])
      names_ <- c(names_,stringi::stri_extract_first(str = inFile$name, regex = ".*(?=\\.)"))
      
    }
    return(names_)
  })
  
  
  taxonomicRanksList <- c("Superkingdom","Kingdom","Phylum","Class","Order","Family","Genus","Species")
  
 ######################### Dataset upload ######################### 
  
  # Reactive to store the Data as a list
  Data <- shiny::reactive({
    shiny::req(input$files)    
    # validate(
    #   need(input$files != "", "Please select a data set")
    # )
    Data <- list(All=data.frame())
    numberOfFiles <- length(input$files$datapath)
    
    # Loop through the datasets, loading successive ones into a list
    for(i in 1:numberOfFiles){
      name_ <- paste("df",i,sep = "")
      assign(name_, read.csv(input$files$datapath[i]))
      
      Data[[name_]] <- as.data.frame(get(name_))
      
      if(!is.null(Data[[name_]]$group.function)){
        Data[[name_]]$group.function[Data[[name_]]$group.function == ""] <- "No COG"
      }
      if(!is.null(Data[[name_]]$predicted.function)){
        Data[[name_]]$predicted.function[Data[[name_]]$predicted.function == ""] <- "No COG"
      }
      
      # Need to review!!
      Data[[name_]][,intersect(colnames(Data[[name_]]),
                               taxonomicRanksList)][is.na(Data[[name_]][,intersect(colnames(Data[[name_]]),
                                                                                   taxonomicRanksList)])]<-"No taxonomy"
      
      
      
      # Create dataset a concatenation of all the files
      if(i==1){
        Data[["All"]] <- as.data.frame(Data[[name_]])
      }
      else{
        Data[["All"]] <- rbind(Data[["All"]],as.data.frame(Data[[name_]]))
      }
    }
    
    return(Data)
  })

  ###########################################################################  
  
  
  ###################### Reactive Function and Phylogeny getters#############
  
  # Reactive to hold all taxonomic ranks in the dataset
  taxa_ranks <- shiny::reactive({
    col_names <- colnames(Data()[[1]])
    Ranks=c("Superkingdom","Kingdom","Phylum","Class","Order","Family","Genus","Species")
    return(intersect(col_names,Ranks))
  })
  
  # Reactive to hold function levels
  functionSelection <- shiny::reactive({
    col_names <- colnames(Data()[[1]])
    names_ <- c("group.function","predicted.function")
    return(intersect(names_,col_names))
  })
  
  
  ##################################################  
    
  
  # Assigning file names to an output
  output$myFileNames <- shiny::renderText({ file_name() })
  
  # Create reactive value Group
  # This will hold the selcted group the user chooses to look at
  Group <- shiny::reactiveVal(NULL)
  Grouptaxa <- shiny::reactiveVal(NULL)
  previoustaxa <- shiny::reactiveVal(NULL)
  previousrank <- shiny::reactiveVal(NULL)
  previousrankholder <- shiny::reactiveVal(NULL)
  
  # When Group is selected, assign name to Group (groupSelection comes from JS)
  shiny::observeEvent(input$groupSelection,{
    Group(input$groupSelection)
  })
  
  # When Group is selected, assign name to Group (groupSelection comes from JS)
  shiny::observeEvent(input$grouptaxaSelection,{
    Grouptaxa(input$grouptaxaSelection)
    if(is.null(Grouptaxa()) || Grouptaxa()!= "Other Taxa"){
      previousrank(taxa_ranks()[input$tbl_rows_selected])
      previoustaxa(Grouptaxa())
    }
    
  })
  
  # Initialse empty place holder for "Others" category
  others <- shiny::reactiveVal(character(0))
  
  # Initialse empty place holder for "Others" category
  othertaxa <- shiny::reactiveVal(character(0))
  
  # If group selected show their name
  output$SelectedGroupName <- shiny::renderText({
    shiny::req(Group())
    return(paste("<b>Selected Function:</b> ",Group()))})
  
  # If group selected show their name
  output$SelectedGrouptaxaName <- shiny::renderText({
    shiny::req(previoustaxa())
    return(paste("<b>Selected Taxa:</b> ",previousrank(),"-",previoustaxa()))})
  
 
  

  #################################################################
  ############# Selection Tables for plot ####################
  
  # Dataset selection table, defualt is All
  output$tbl2 = DT::renderDT(
    DT::datatable(data.frame(Data=c("All",file_name())),
                  selection = list(mode="single", selected=1),
                  options = list(sDom  = '<"top">rt<"bottom">i',
                                 lengthChange = FALSE)
    )
    #,    options = list(lengthChange = FALSE)
  )
  
  
  # Taxonomic rank selection table
  output$tbl = DT::renderDT(
    DT::datatable(data.frame("Taxonomy"=taxa_ranks()),#c("Kingdom","Phylum","Class","Order","Family","Genus","Species")),
              selection = list(mode="single", selected=1),
              options = list(sDom  = '<"top">rt<"bottom">i',
                             lengthChange = FALSE)
    )
    #,    options = list(lengthChange = FALSE)
  )
  
  # Function level selection table
  output$tbl3 = DT::renderDT(
    DT::datatable(data.frame("Function"=functionSelection()),#c("Group Function","Predicted Function")),
              selection = list(mode="single", selected=1),
              options = list(sDom  = '<"top">rt<"bottom">i',
                             lengthChange = FALSE)
    )
    #,    options = list(lengthChange = FALSE)
  )
  
  
  ##################################################################
  
  ################ Reset button ######################
  shiny::observeEvent(input$reset,{
    Group(NULL)
    Grouptaxa(NULL)
    previoustaxa(NULL)
    previousrank(taxa_ranks()[input$tbl_rows_selected])
    previousrankholder(NULL)
  }) 
  
  ######################################
  # function to create array of colours
  
  #getPalette = grDevices::colorRampPalette(brewer.pal(9, "Set1"))
  
  ##################################################################
  
  
  # reactive containg code to create plot
  Cplot <- shiny::reactive({
    numberOfFiles <- length(input$files$datapath)
    
    # selected dataset
    d<- input$tbl2_rows_selected
    shiny::req(d)
    
    # Assign selected datasets
    table1 <- as.data.frame(Data()[[d]])
    
    # level of selected taxonomic rank
    s<- input$tbl_rows_selected
    
    # level of function resolution
    f <- input$tbl3_rows_selected
    
    # Update function and phylogeny selection
    table1[,taxa_ranks()[s]] <- as.factor(stringr::str_trim(as.character(table1[,taxa_ranks()[s]])))
   
    if(!is.null(previoustaxa())){
      table1 <- table1[table1[,previousrank()] %in% previoustaxa(),]
      
    }
    
    table1$Predicted.Function <- as.factor(stringr::str_trim(as.character(table1[,functionSelection()[f]])))
    
    # If "other" functions are sected update the dataset
    if(!is.null(Group()) && Group() %in% c("Other")){
      
      table1 <- table1[table1[,functionSelection()[f]] %in% others(),]
      
    }
    
    # If one of the functional groups is selected update to higher resolution
    else if(!is.null(Group()) &&  Group() %in% unique(table1$group.function)){
      
      table1 <- table1[table1[,functionSelection()[f]]==input$groupSelection,]
      table1$Predicted.Function <- as.factor(stringr::str_trim(as.character(table1$predicted.function)))
      
    }
    
    else { Group(NULL)}
    
    
    ##################### Taxonomy if Statements ################
    
    if(!is.null(Grouptaxa()) && Grouptaxa() %in% c("Other Taxa") && all(othertaxa() %in% unique(table1[,taxa_ranks()[s]]))){
      
      table1 <- table1[table1[,taxa_ranks()[s]] %in% othertaxa(),]
      # Predicted.Function.holder <- stringr::str_trim(as.character(Data.holder[,functionSelection()[f]]))
      
    }
    
    
    # Update: Change data.holder according to selection
    else if(!is.null(Grouptaxa()) &&  Grouptaxa() %in% unique(table1[,taxa_ranks()[s]])){
      
      
      table1 <- table1[table1[,taxa_ranks()[s]]==input$grouptaxaSelection,]
      # Predicted.Function.holder <- stringr::str_trim(as.character(Data.holder$predicted.function))
      
    }else{ Grouptaxa(NULL)}
    
   ########################################### 
    # extract functions and taxonomy from dataset
    chord_table <- data.frame(functionCol=table1[,"Predicted.Function"],taxonomy=table1[,taxa_ranks()[s]])#Lowest.Common.Ancestor
    
    # encode NA's as factors
    chord_table$functionCol <- addNA(chord_table$functionCol)
    levels(chord_table$functionCol)[is.na(levels(chord_table$functionCol))]<- "N/A"
    chord_table[is.na(chord_table$functionCol),"functionCol"] <- "N/A"
    
    # remove NA's from analysis
    chord_table2<- chord_table[chord_table$functionCol!=""&chord_table$functionCol!="N/A",]
    
    # ensure functions are factors
    chord_table3 <- data.frame("functionCol" = as.factor(as.character(chord_table2$functionCol)),
                               "taxonomy"=as.factor(as.character(chord_table2$taxonomy)))
    
    # covert dataframe to tibble
    tib <- tibble::as_data_frame(chord_table3)
    
    # summarise the tibble
    mat_list<- tib %>% dplyr::group_by(taxonomy,functionCol) %>% dplyr::summarise(n=n())
    
    #########################################
    #NEW
    
    # Set those below threshold to "Other"
    Sum_state <- mat_list %>% dplyr::group_by(functionCol) %>% dplyr::summarise(N=sum(n))
    Sum_entity <- mat_list %>% dplyr::group_by(taxonomy) %>% dplyr::summarise(N=sum(n))
    
    threshold <- 0.02
    funcThreshold <- ifelse(!is.null(Group()) && Group()=="Other",0,threshold)
    taxThreshold <- ifelse(!is.null(Grouptaxa()) && Grouptaxa()=="Other Taxa",0,threshold)
    # print(!is.null(Group()) && Group()=="Other")
    # print(!is.null(Grouptaxa()) && Grouptaxa()=="Other Taxa")
    Total_entries <- sum(mat_list$n)
    
    # Change entriess to characters
    mat_list$functionCol<- as.character(mat_list$functionCol)
    
    # Create place holder for "other" functions
    others_holder <- character(0)
    
    # Check if functions account for less than 2% of data
    # If yes assign them to "other" and store their name
    for(i in 1:length(Sum_state$functionCol)){
      if(Sum_state$N[i]/Total_entries < funcThreshold){
        others_holder <- c(others_holder,as.character(Sum_state$functionCol[i]))
        mat_list$functionCol[mat_list$functionCol==Sum_state$functionCol[i]]<- "Other"
      }
    }
    
    # If group is selcted update others
    if(is.null(Group())){ #&& Group()!="Other"){
      others(others_holder)
      
    }
    
    # 
    mat_list$taxonomy<- as.character(mat_list$taxonomy)
    
    # Create place holder for "other taxa"
    othertaxa_holder <- character(0)
    
    
    for(i in 1:length(Sum_entity$taxonomy)){
      
      if(Sum_entity$N[i]/Total_entries < taxThreshold){
        othertaxa_holder <- c(othertaxa_holder,as.character(Sum_entity$taxonomy[i]))
        mat_list$taxonomy[mat_list$taxonomy==Sum_entity$taxonomy[i]]<- "Other Taxa"
      }
    }
    
    # If group is selcted update others
    if(is.null(Grouptaxa()) || Grouptaxa()!="Other Taxa"){
      othertaxa(othertaxa_holder)
      
    }
   
    
    mat_list$functionCol<- as.factor(mat_list$functionCol)
    mat_list$taxonomy<- as.factor(mat_list$taxonomy)
    mat_list <- mat_list %>% dplyr::group_by(taxonomy,functionCol) %>% dplyr::summarise(n=sum(n))
    
    
    # End New
    #############################################
    
    ###############################################
    
    # More new
    
    if(d==1 & numberOfFiles>1){
    
    # reactor data for bar charts
    taxa <- taxa_ranks()[s]
    
    
    ################################################
    # Update for any dataset
    ######################################################################################################
    all_df_sums <- list()
    for(i in 2:length(Data())){
      name <- paste("df",i-1,"sum",sep = "")
      
      ##########################
      #Update: Pre-process data as above given selcetion
      Data.holder <- Data()[[i]]
      Data.holder[,taxa_ranks()[s]] <- (stringr::str_trim(as.character(Data()[[i]][,taxa_ranks()[s]])))
      
      if(!is.null(previoustaxa())){
        Data.holder <- Data.holder[Data.holder[,previousrank()]%in%previoustaxa(),]
        
      }
      
      Predicted.Function.holder <- stringr::str_trim(as.character(Data.holder[,functionSelection()[f]]))
      
      # Update: Change data.holder according to selection
      if(!is.null(Group()) && Group() %in% c("Other")){
        
        Data.holder <- Data.holder[Data.holder[,functionSelection()[f]] %in% others(),]
        Predicted.Function.holder <- stringr::str_trim(as.character(Data.holder[,functionSelection()[f]]))
        
      }
      
      
      # Update: Change data.holder according to selection
      else if(!is.null(Group()) &&  Group() %in% unique(table1$group.function)){
        
        Data.holder <- Data.holder[Data.holder[,functionSelection()[f]]==input$groupSelection,]
        Predicted.Function.holder <- stringr::str_trim(as.character(Data.holder$predicted.function))
        
      }
      
      ############# Taxonomy Selection ##################
      # Update: Change data.holder according to selection
      if(!is.null(Grouptaxa()) && Grouptaxa() %in% c("Other Taxa") && all(othertaxa() %in% unique(table1[,taxa_ranks()[s]]))){
        
        Data.holder <- Data.holder[Data.holder[,taxa_ranks()[s]] %in% othertaxa(),]
        if(!is.null(Group()) &&  Group() %in% unique(table1$group.function)){
          Predicted.Function.holder <- stringr::str_trim(as.character(Data.holder$predicted.function))
          
        }else{
        Predicted.Function.holder <- stringr::str_trim(as.character(Data.holder[,functionSelection()[f]]))
        }
      }
      
      
      # Update: Change data.holder according to selection
      else if(!is.null(Grouptaxa()) &&  Grouptaxa() %in% unique(table1[,taxa_ranks()[s]])){
        
        
        Data.holder <- Data.holder[Data.holder[,taxa_ranks()[s]]==input$grouptaxaSelection,]
        if(!is.null(Group()) &&  Group() %in% unique(table1$group.function)){
          Predicted.Function.holder <- stringr::str_trim(as.character(Data.holder$predicted.function))
          
        }else{
        Predicted.Function.holder <- stringr::str_trim(as.character(Data.holder[,functionSelection()[f]]))
        }
      }
      
      ######################
      #Update: Use data.holder and predicted.function.holder to be consistent with earlier preprocessiing
      
      temp <- data.frame(taxa=Data.holder[,taxa],#taxa=stringr::str_trim(as.character(table1[,taxa])),#stringr::str_trim(as.character(Data()[[i]][,taxa])),
                         Predicted.Function = Predicted.Function.holder,#Predicted.Function.holder,#stringr::str_trim(as.character(table1[,functionSelection()[f]])),#stringr::str_trim(as.character(Data()[[i]][,functionSelection()[f]])),
                         stringsAsFactors = F) %>%
        dplyr::group_by(taxa, Predicted.Function) %>% dplyr::summarise(N = n())
      temp$N <- as.numeric(temp$N)
      colnames(temp)[3] <- paste(file_name()[i-1])
      assign(name, temp)
      all_df_sums[[i-1]] <- get(name)
    }
    
    all_df_sums_join <- suppressMessages(Reduce(dplyr::full_join,all_df_sums))
    
    all_df_sums_join <- all_df_sums_join %>% replace(is.na(.), 0)
    all_df_sums_join$SUM <- rowSums(all_df_sums_join[,c(3:(length(Data())+1))])
    
    
    all_df_sums_join <- dplyr::arrange(all_df_sums_join,taxa,Predicted.Function)
    
    
    
    Sum_fun <- all_df_sums_join %>% dplyr::group_by(Predicted.Function) %>% dplyr::summarise(N=sum(SUM))
    Sum_taxa <- all_df_sums_join %>% dplyr::group_by(taxa) %>% dplyr::summarise(N=sum(SUM))
    
    # threshold <- 0.02
    Total_entries <- sum(all_df_sums_join$SUM)
    
    all_df_sums_join <- suppressMessages(Reduce(dplyr::full_join,all_df_sums))
    
    all_df_sums_join <- all_df_sums_join %>% replace(is.na(.), 0)
    
    for(i in 1:length(Sum_fun$Predicted.Function)){
      
      if(Sum_fun$N[i]/Total_entries < funcThreshold){
        all_df_sums_join$Predicted.Function[all_df_sums_join$Predicted.Function == Sum_fun$Predicted.Function[i]] <- "Other"
      }
    }
    
    
    for(i in 1:length(Sum_taxa$taxa)){
      
      if(Sum_taxa$N[i]/Total_entries < taxThreshold){
        all_df_sums_join$taxa[all_df_sums_join$taxa==Sum_taxa$taxa[i]]<- "Other Taxa"
      }
    }
    
    df_all <- all_df_sums_join
    df_all$SUM <- rowSums(df_all[,c(3:(length(Data())+1))])
    
    df_all <- df_all %>% dplyr::group_by(taxa,Predicted.Function) %>%
      dplyr::summarise_at(c(3:(length(Data())+2)),sum)#+1 for SUM
    
    df_group_fun <- df_all %>% dplyr::group_by(Predicted.Function) %>%
      dplyr::summarise_at(c(3:(length(Data())+1)),sum)
    df_group_fun$N <- rowSums(df_group_fun[2:(length(Data()))])
    df_group_fun <- dplyr::arrange(df_group_fun,dplyr::desc(N))
    
    df_group_tax <- df_all %>% dplyr::group_by(taxa) %>% dplyr::summarise_at(c(3:(length(Data())+1)),sum)
    df_group_tax$N <- rowSums(df_group_tax[2:(length(Data()))])
    df_group_tax <- dplyr::arrange(df_group_tax,dplyr::desc(N))
    
    Group_sum <- jsonlite::toJSON(as.list(df_group_fun))
    
    df_all <- df_all %>% dplyr::arrange(match(taxa,df_group_tax$taxa),match(Predicted.Function,df_group_fun$Predicted.Function))
    
    ####################################################################################################
    # End Update
    ####################################################################################################
    
    
    l<-list()
    for(i in 1:nrow(df_all)){
      for(j in 1:numberOfFiles){
        if(j==1){
          l[[i]]<- as.numeric(df_all[i,j+2])
        }
        
        else{
          l[[i]][j]<- as.numeric(df_all[i,j+2])  
        }
        
      }
      
    }
    # l
    
    
    exportJson <- jsonlite::toJSON(l)
    
    }
    if(d!=1 | numberOfFiles==1){exportJson<-NULL
    Group_sum <- NULL}
    # 
    # End More new
    #############################################
    #############################################
    
    
    
    
    
    mat_list_groupFun <- mat_list %>% dplyr::group_by(functionCol) %>% dplyr::summarise(N=sum(n)) %>% dplyr::arrange(dplyr::desc(N))
    
    mat_list_groupTaxa <- mat_list %>% dplyr::group_by(taxonomy) %>% dplyr::summarise(N=sum(n)) %>% dplyr::arrange(dplyr::desc(N))
    
    mat_list <- mat_list %>% dplyr::arrange(match(taxonomy,mat_list_groupTaxa$taxonomy),match(functionCol,mat_list_groupFun$functionCol))
    
    
    ###########################################
    # Update: Order names based off of ordered group sums
    if(d==1 & numberOfFiles>1){
      x <- unique(df_group_fun$Predicted.Function)
      
      y<- unique(df_group_tax$taxa)
      
     
    }else{
      x <- unique(mat_list$functionCol)
      
      y<- unique(mat_list$taxonomy)
    }
    # create zero matrix of the dimensions of the functions and taxa
    m_1 <- matrix(0,nrow = length(y),ncol=length(x),dimnames = list(y,x))
    
    # convert the summary table back to a dataframe
    df<- as.data.frame(mat_list)
    
    # add the size of the links to the zero matrix
    for( i in 1:(nrow(df))){
      m_1[toString(df[i,1]),toString(df[i,2])]<-df[i,3]
    }
    
    
    ############################################
    
    # create the chord diagram
    return(
      chorddiag::chorddiag(m_1,type = "bipartite",
                            groupColors = substr(grDevices::rainbow(nrow(m_1)+ncol(m_1)),0,7),
                            groupnamePadding = 20,
                            groupnameFontsize = 10,
                            # categoryNames = T,
                            categorynamePadding = 200,
                            ticklabelFontsize = 10,
                            tickInterval = max(1,sum(mat_list$n)%/%200),
                            margin = 400-input$margin,
                            reactor = exportJson,
                            grouptotals = Group_sum,
                            firstfunindex = length(y))
                )
  })
  
  ##################################################################
  
  output$ChordPlot <- chorddiag::renderChorddiag({Cplot()})
  
  
}
