# sidebar ---------------------------------------------------------------------
sidebar <- function(){
  shinydashboard::dashboardSidebar(
    shinydashboard::sidebarMenu(
      # Home
      home(),
      
      # Datas
      datas(),
      
      # Solver Options
      solver_options(),
      
      # Status Icon
      status_solving("85%", "auto"),
      status_opt_feas("85%", "auto"),
      status_inf_lim("85%", "auto"),
      
      # Solve Button
      solve(),
      
      # Results
      results(),
      
      # Report
      report_sidebar()
    )
  )
}
# --------------------------------------------------------------------------- #


# home ------------------------------------------------------------------------
home <- function(){
  shinydashboard::menuItem(
    "Home", tabName = "Home", icon = shiny::icon("home"), selected = T
  )
}
# --------------------------------------------------------------------------- #


# datas ----------------------------------------------------------------------- 
datas <- function(){
  shinydashboard::menuItem(
    "Data", tabName = "datas", icon = shiny::icon("database"),
    
    # Aircraft data
    shinydashboard::menuSubItem(
      "Resources",
      tabName = "Aircraft",
      icon = shiny::icon("child")#, class="glyphicon", lib="glyphicon") #family 
    ),
    
    # Fire data
    shinydashboard::menuSubItem(
      "Fire",
      tabName = "Fire",
      icon = shiny::icon("fire", lib="glyphicon")
    )
  )
}
# --------------------------------------------------------------------------- #


# solver_options --------------------------------------------------------------
solver_options <- function(){
  
  shinydashboard::menuItem(
    "Options", tabName = "options", icon = shiny::icon("gear"),
    
    # Penalty
    # -------
    shiny::sliderInput(
      "M", "Penalty", 1000, 1000000, 1000
    ),
    
    # Solver
    # ------
    shiny:: radioButtons(
      "solver", "Solver",
      c(
        "gurobi" = "gurobi",
        "lpSolve" = "lpSolve",
        "Rsymphony" = "Rsymphony"
      ),
      inline = T
    ),
    
    # Method
    #-------
    shiny:: radioButtons(
      "method", "Method",
      c(
        "Exact" = "exact",
        "Heuristic" = "heuristic"
      ),
      inline = T,
      selected = "exact"
    ),
    
    # Maximum number of iterations
    #-----------------------------
    shiny::conditionalPanel(
      condition = "input.method == 'heuristic'",
      shiny::numericInput(
        "IterMax", "Maximum number of iterations", 10
      )
    )
  )
}
# --------------------------------------------------------------------------- #


# status ----------------------------------------------------------------------
status_solving <- function(left, top){
  shinyjs::hidden(
    shiny::absolutePanel(
      id="status_load" ,left=left, top=top,
      shiny::h4(shiny::icon("spinner", class = "fa-pulse"))
    )
  )
}
# --------------------------------------------------------------------------- #


# status_opt_feas -------------------------------------------------------------
status_opt_feas <- function(left, top){
  shinyjs::hidden(
    shiny::absolutePanel(
      id="status_opt", left=left, top=top,
      shiny::h4(shiny::icon("check"))
    )
  )
}
# --------------------------------------------------------------------------- #


# status_inf_lim --------------------------------------------------------------
status_inf_lim <- function(left, top){
  shinyjs::hidden(
    shiny::absolutePanel(
      id="status_inf", left=left, top=top,
      shiny::h4(shiny::icon("remove"))
    )
  )
}
# --------------------------------------------------------------------------- #


# solve -----------------------------------------------------------------------
solve <- function(){
  shiny::tags$div(
    class="header", id="solve_div",
    shiny::absolutePanel(
      left="10%",
      shinydashboard::menuItem(
        tabName = "solve_optimal",
        shiny::actionButton(
          "solve", "Solve", shiny::icon("calculator")
        )
      )
    ),
    shiny::br(),shiny::br() # empty lines
  )
}
# --------------------------------------------------------------------------- #


# results ---------------------------------------------------------------------
results <- function(){
  shinydashboard::menuItem(
    "Results", tabName = "results", icon = shiny::icon("briefcase")
  )
}
# --------------------------------------------------------------------------- #


# report ----------------------------------------------------------------------
report_sidebar <- function(){
  shiny::tags$div(
    shiny::absolutePanel(
      bottom = "5%",
      left="10%",
      
      shiny::downloadButton("report", "Generate report")
    )
  )
}
# --------------------------------------------------------------------------- #
