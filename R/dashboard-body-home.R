# home_body -------------------------------------------------------------------
home_body <- function(){
  shinydashboard::tabItem(
    tabName = "Home",
    shiny::h2("Wildfire Resources Management."),
    shiny::br(),
    "Determine optimal planning that includes the number and type of",
    "resources needed to extinguishing a forest fire is a difficult task",
    "In this app, a general integer programming model is programed",
    "which also includes the allocation of resources to different periods of",
    "time in a day embedded in extinguishing a fire, with the goal of meeting",
    "the Spanish regulations regarding non-neglect of fronts and periods of",
    "rest of the pilots. We consider two types to get a solution, the exact",
    "method and also an heuristic algorithm designed specifically for",
    "this problem which allows to obtain a quality solution quickly in",
    "real problems.",
    shiny::br(),
    shiny::br(),
    "For more information",
    shiny::tags$a(
      href=paste("http://eio.usc.es/pub/mte/descargas",
                 "/ProyectosFinMaster/Proyecto_1095.pdf", sep=""), 
      "click here!"
    ),
    shiny::HTML(
      "<head>",
      "<script type='text/x-mathjax-config'>",
      "MathJax.Hub.Config({tex2jax: {inlineMath: [['$','$']]}});",
      "</script>",
      "<script type='text/javascript' async",
      paste("src='https://cdn.mathjax.org/mathjax/latest",
            "/MathJax.js?config=TeX-AMS_CHTML'>", sep=""),
      "</script>",
      "</head>"
    )
  )
}
# --------------------------------------------------------------------------- #