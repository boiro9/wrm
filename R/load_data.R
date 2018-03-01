# load_data -------------------------------------------------------------------
load_data <- function(input, output, session){
  # Resources
  data.aero <- shiny::reactive({
    aero.inFile <- input$aerofile
    if (is.null(aero.inFile)) {
      return(NULL)
    }
    aero <- read.csv(aero.inFile$datapath,
                     header = T,
                     sep = ";",
                     dec = ",",
                     stringsAsFactors =FALSE)
    return(list(aero=aero))
  })
  
  output$aero.table <- DT::renderDataTable({
    # require that data() is available
    if(!is.null(data.aero())){
      DT::datatable(data.aero()$aero)
    }
  })
  
  # Fire
  data.fire <- shiny::reactive({
    
    fire.inFile <- input$fire.file
    if (is.null(fire.inFile)) {
      return(NULL)
    }
    fire <- read.csv2(fire.inFile$datapath,
                      header = T,
                      sep = ";",
                      dec = ",",
                      stringsAsFactors =FALSE)
    return(list(fire=fire))
  })
  
  output$fire.table <- DT::renderDataTable({
    # require that data() is available
    if(!is.null(data.fire())){
      DT::datatable(data.fire()$fire)
    }
  })
  
  return(list(data.aero=data.aero, data.fire=data.fire))
}
# --------------------------------------------------------------------------- #


# load_resource_data ----------------------------------------------------------
load_resource_data  <- function(input, output, session){
  input$data.aero <- shiny::reactive({
    aero.inFile <- input$aerofile
    if (is.null(aero.inFile)) {
      return(NULL)
    }
    aero <- read.csv(aero.inFile$datapath,
                     header = T,
                     sep = ";",
                     dec = ",",
                     stringsAsFactors =FALSE)
    return(list(aero=aero))
  })
  
  output$aero.table <- DT::renderDataTable({
    # require that data() is available
    if(!is.null(data.aero())){
      DT::datatable(data.aero()$aero)
    }
  })
}
# --------------------------------------------------------------------------- #


# load_aero_data --------------------------------------------------------------
load_aero_data  <- function(input, output, session){
  input$data.fire <- shiny::reactive({
    
    fire.inFile <- input$fire.file
    if (is.null(fire.inFile)) {
      return(NULL)
    }
    fire <- read.csv2(fire.inFile$datapath,
                      header = T,
                      sep = ";",
                      dec = ",",
                      stringsAsFactors =FALSE)
    return(list(fire=fire))
  })
  
  output$fire.table <- DT::renderDataTable({
    # require that data() is available
    if(!is.null(data.fire())){
      DT::datatable(data.fire()$fire)
    }
  })
}
# --------------------------------------------------------------------------- #
