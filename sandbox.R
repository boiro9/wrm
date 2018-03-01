aero = read.csv("example/new_data/Aeronaves1.csv",
         header = T,
         sep = ";",
         dec = ",",
         stringsAsFactors =FALSE)

fire = read.csv("example/new_data/Incendio4.csv",
                header = T,
                sep = ";",
                dec = ",",
                stringsAsFactors =FALSE)

#-------------------------------------------------------------------------
# get data
#-------------------------------------------------------------------------
input <- list(PeriodTime=10)
problem.info <- get_data(aero, fire, input)

#-------------------------------------------------------------------------
# Solve model
#-------------------------------------------------------------------------
results <- asa::asa(problem.info,
                    M_prime=10000,
                    'exact',
                    niters=100,
                    solver='gurobi')

results <- results$st_model
#=========================================================================
# Scheduling
#-------------------------------------------------------------------------

# Get data
#-------------------------------------------------------------------------

WRF <- asa::data.scheduling(results)

# Data selection
#-------------------------------------------------------------------------
updateCheckboxGroupInput(session,
                         "WRF.rows",
                         label = "Selected aircraft:",
                         choices = row.names(WRF),
                         selected = row.names(WRF)
)

updateSliderInput(session,
                  "Period.slider",
                  label = "Period range:",
                  min = 1,
                  max = dim(WRF)[2],
                  value = c(1, min(5, dim(WRF)[2]))
)

# Data output
#-------------------------------------------------------------------------
output$WRF.data <- DT::renderDataTable({
  DT::datatable(WRF)
})

# Graph output
#-------------------------------------------------------------------------
output$WRF.plot <- renderPlotly({
  asa::plotscheduling(WRF)
})


#=========================================================================
# Contention
#-------------------------------------------------------------------------

# Get data
#-------------------------------------------------------------------------
contention.data <- data.contention(problem.info, results)

# Data output
#-------------------------------------------------------------------------
print(6)
output$contention.data <- DT::renderDataTable({
  DT::datatable(contention.data)
})

# Graph output
#-------------------------------------------------------------------------
print(7)
output$contention.plot <- renderPlotly({
  asa::plotcontention(contention.data)
})


#=========================================================================
# Number of Aircraft
#-------------------------------------------------------------------------

# Get data
#-------------------------------------------------------------------------
print(8)
num.aircraft.data <- asa::data.num.aircraft(results)

# Data output
#-------------------------------------------------------------------------
print(9)
output$num.aircraft.data <- DT::renderDataTable({
  DT::datatable(num.aircraft.data)
})

# Graph output
#-------------------------------------------------------------------------
print(10)
output$num.aircraft.plot <- renderPlotly({
  asa::plotnumaircraft(num.aircraft.data)
})


#=========================================================================
# Yield
#-------------------------------------------------------------------------

# Get data
#-------------------------------------------------------------------------
print(11)
yield.data <- data.yield(problem.info, results)

# Data output
#-------------------------------------------------------------------------
print(12)
output$yield.data <- DT::renderDataTable({
  DT::datatable(yield.data)
})

# Graph output
#-------------------------------------------------------------------------
print(13)
output$yield.plot <- renderPlotly({
  asa::plotyield(yield.data)
})

