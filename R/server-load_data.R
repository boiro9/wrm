
#' Load data
#'
#' @param input 
#' @param output 
#' @param session 
#'
#' @return
#' @export
#'
#' @examples
load_data <- function(input, output, session){
  # Resources
  load_resources(input, output, session)
  
  # Fire
  load_fire(input, output, session)
}
# --------------------------------------------------------------------------- #


# load_resources --------------------------------------------------------------
load_resources <- function(input, output, session){
  data_resources <- shiny::reactive({
    if(is.null(input$resources_table)){
      resources <- data.frame(
        Name = c(""),
        C = as.numeric(NA),
        P = as.numeric(NA),
        A = as.numeric(NA),
        CFP = as.numeric(NA),
        CRP = as.numeric(NA),
        CTFP = as.numeric(NA),
        BPR = as.numeric(NA),
        FBRP = as.numeric(NA),
        FP = as.numeric(NA),
        RP = as.numeric(NA),
        DFP = as.numeric(NA),
        ITW = as.logical(NA),
        IOW = as.logical(NA),
        G = c("")
      )
    }else{
      resources <- rhandsontable::hot_to_r(input$resources_table)
    }
    
    return(list(resources=resources))
  })
  
  v <- shiny::reactiveValues(from_file = FALSE)
  
  shiny::observeEvent(input$resources_file,{
    v$from_file <- TRUE
  })
  
  shiny::observeEvent(input$resources_table,{
    v$from_file <- FALSE
  })
  
  
  output$resources_table <- rhandsontable::renderRHandsontable({
    if(v$from_file){
      resources <- WildfireResources::load_resources_data(
        input$resources_file$datapath)
    }else{
      resources <- data_resources()[["resources"]]
    }
    rhandsontable::rhandsontable(resources)
  })
}
# --------------------------------------------------------------------------- #


# load_fire -------------------------------------------------------------------
load_fire <- function(input, output, session){
  data_fire <- shiny::reactive({
    
    if (is.null(input$fire_table)){
      fire <- data.frame(
        Period = as.numeric(1),
        PER = as.numeric(NA),
        NVC = as.numeric(NA))
    }else{
      fire <- rhandsontable::hot_to_r(input$fire_table)
    }
    
    return(list(fire=fire))
  })
  
  v <- shiny::reactiveValues(from_file = FALSE)
  
  shiny::observeEvent(input$fire_file,{
    v$from_file <- TRUE
  })
  
  shiny::observeEvent(input$fire_table,{
    v$from_file <- FALSE
  })
  
  
  output$fire_table <- rhandsontable::renderRHandsontable({
    if(v$from_file){
      fire <- WildfireResources::load_fire_data(input$fire_file$datapath)
    }else{
      fire <- data_fire()[["fire"]]
    }
    fire <- check_fire_columns(input, fire)
    rhandsontable::rhandsontable(fire)
  })
}
# --------------------------------------------------------------------------- #


# check_fire_columns ----------------------------------------------------------
check_fire_columns <- function(input, fire){
  if(!is.null(input$resources_table)){
    columns <- c('Period', 'PER', 'NVC')
    
    n_periods <- nrow(fire)
    
    resources <- rhandsontable::hot_to_r(input$resources_table)
    
    # EF
    for(n in resources[["Name"]]){
      col <- paste("EF", n, sep=".")
      columns <- c(columns, col)
      if(!(col %in% colnames(fire))){
        fire[col] <- rep(1, n_periods)
      }
    }
    
    # nMin and nMax
    max_resources <- length(resources[["Name"]])
    for(g in unique(resources[["G"]])){
      min_col <- paste("nMin", g, sep=".")
      columns <- c(columns, min_col)
      if(!(min_col %in% colnames(fire))){
        fire[min_col] <- as.integer(rep(0, n_periods))
      }
      
      max_col <- paste("nMax", g, sep=".")
      columns <- c(columns, max_col)
      if(!(max_col %in% colnames(fire))){
        fire[max_col] <- as.integer(rep(max_resources, n_periods))
      }
    }
    
    # Get right columns
    fire <- fire[, columns]
  }
  
  return(fire)
}
# --------------------------------------------------------------------------- #
