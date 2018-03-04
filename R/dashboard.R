#-------------------------------------------------------------------------------
# Dashboard
#-------------------------------------------------------------------------------

#' Dashboard to run the Aircraft Selection and Allocation (ASA) package on the shiny app
#'
#' @return shinydashboard ui.
#'
#' @import plotly
#' @import shinydashboard
#'
#' @export
#'
#' @examples
#' SaA_dashboard()
asa_dashboard <- function(){
  ui <- dashboardPage(header(), sidebar(), body())
  
  return(ui)
}

