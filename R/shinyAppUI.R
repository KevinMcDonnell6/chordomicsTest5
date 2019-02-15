ChordshinyAppUI <- shiny::fluidPage(
  shiny::tags$head(shiny::tags$style(".rightAlign{float:right;}")),
  # Application title
  # titlePanel("CircosPro"),
  shiny::tabsetPanel(
    shiny::tabPanel(
        "File Upload",
      
        shiny::fluidRow( shiny::sidebarLayout(
          shiny::sidebarPanel(
            shiny::fileInput("files",
                        label="Upload CSVs here",
                        multiple = TRUE)#,
          ),
        
          shiny::mainPanel( 
                            shiny::h2("Welcome to Chordomics!"),
                            shiny::p("Please upload a dataset from the panel on the left."),
                            shiny::p("Ensure the file type is csv format and contains headings
                              of taxonomic rank and 'predicted.function'")
                            
                            )
                )
          )
      ),
    shiny::tabPanel("Plot",
             
             # Sidebar with a slider input for zoom and level of taxa 
             shiny::sidebarLayout(
               shiny::sidebarPanel(
                      width = 3,
                 
                      shiny::sliderInput("margin", "Zoom",  min = 0, max = 400, value = 200),
                 DT::DTOutput("tbl2"),
                 DT::DTOutput("tbl"),
                 DT::DTOutput("tbl3")
               ),
               
               # Show a plot of the Circos
               shiny::mainPanel( 
                 shiny::tags$style(type="text/css",
                            ".shiny-output-error { visibility: hidden; }",
                            ".shiny-output-error:before { visibility: hidden; }"
                 ),
                 # mainPanel( textOutput("myFileNames")),
                 #downloadButton('foo',class = "rightAlign"),
                 # uiOutput("CPlot"),
                 #verbatimTextOutput("summary"),
                 shiny::actionButton('reset',"Reset",class = "rightAlign"),
                 # uiOutput("CPlot"),
                 shiny::htmlOutput("SelectedGroupName"),
                 chorddiag::chorddiagOutput("ChordPlot",width="850px",height="850px")#,
                 
               )
             )
    )
    )
  )
