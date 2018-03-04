#-------------------------------------------------------------------------------
# Server
#-------------------------------------------------------------------------------

#' Server to run the Selection and Allocation (SaA) package on the shiny app
#'
#' @return shiny server.
#'
#' @import plotly
#' @import slam
#' @export
#' 
#' @examples
#' asa_server
asa_server <- function(input, output, session) {
  # ===========================================================================
  # Initialization: Boxes
  # ===========================================================================
  asa::initialization(input, output, session)
  
  # ===========================================================================
  # Load Datas
  # ===========================================================================
  values <- asa::load_data(input, output, session)
  
  # ===========================================================================
  # Solve button
  # ===========================================================================
  asa::solve_button(input, output, session, values)

}




