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
  shiny::shinyApp(wrm::wrm_dashboard(), wrm::wrm_server)
}
