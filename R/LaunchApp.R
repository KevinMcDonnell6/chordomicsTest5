#' Launch Chordomics
#' 
#' Function launches Chordomics server and UI
#' @export
#' 

launchApp <- function(){
  shiny::shinyApp(server = ChordshinyAppServer, ui = ChordshinyAppUI)
}
launchApp()
