
#' blank plot
#'
#' @return blank plot
#'
#' @export
blankplot <- function(){
  ggplot2::ggplot(data.frame())+
    theme(panel.background = element_blank())
}
# --------------------------------------------------------------------------- #


#' data.scheduling.selection
#'
#' @param WRF matrix of 0's 1's 2's and 3's that represent the no-working, 
#' working, resting, and flying.
#' @param input shiny input.
#'
#' @return data
#' @export
data.scheduling.selection <- function(WRF, input){
  sel.rows <- row.names(WRF) %in% input$WRF.rows
  sel.cols <- input$Period.slider[1]:input$Period.slider[2]
  sel.WRF <- matrix(WRF[sel.rows, sel.cols], ncol=length(sel.cols))
  row.names(sel.WRF) <- input$WRF.rows
  colnames(sel.WRF) <- sel.cols
  return(sel.WRF)
}
# --------------------------------------------------------------------------- #