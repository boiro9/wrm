#-------------------------------------------------------------------------------
# SaA_shinyApp
#-------------------------------------------------------------------------------

#' Shiny App for the Selection and Allocation problem.
#'
#' @return ShinyApp
#' @export
#'
#' @examples
#' SaA_shinyApp()
shinyapp<-function(){
  if(requireNamespace("shiny", quietly = TRUE)){
    shiny::shinyApp(asa::asa_dashboard(), asa::asa_server)
  }
}
