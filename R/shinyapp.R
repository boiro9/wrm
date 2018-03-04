#-------------------------------------------------------------------------------
# SaA_shinyApp
#-------------------------------------------------------------------------------

#' Shiny App for the Selection and Allocation problem.
#'
#' @return ShinyApp
#' @export
#'
#' @examples
#' shinyapp()
shinyapp<-function(){
  shiny::shinyApp(asa::asa_dashboard(), asa::asa_server)
}
