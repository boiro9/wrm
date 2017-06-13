#-------------------------------------------------------------------------------
# blank plot
#-------------------------------------------------------------------------------
#' blank plot
#'
#' @return blank plot
#'
#' @export
blankplot <- function(){
  if(requireNamespace("ggplot2", quietly = TRUE)){
    ggplot2::ggplot(data.frame())+
      theme(panel.background = element_blank())
  }
}


#-------------------------------------------------------------------------------
# get data
#-------------------------------------------------------------------------------

get_data <- function(aeronaves, incendio, input){
  # Law
  #-----------------------------------------------------------------------------
  PeriodTime = input$PeriodTime
  FP_min = input$FP_min
  RP_min = input$RP_min
  DFP_min = input$DFP_min
  FBRP_min = input$FBRP_min
  nMax = input$nMax

  # Aircraft
  #-----------------------------------------------------------------------------
  I = aeronaves[,1]

  C_hour = as.double(aeronaves[,2])
  P = as.double(aeronaves[,3])
  A_min = as.integer(aeronaves[,4])
  CFP_min = as.integer(aeronaves[,5])
  CRP_min = as.integer(aeronaves[,6])
  CTFP_min = as.integer(aeronaves[,7])
  BPR_hour = as.double(aeronaves[,8])

  # Fire
  #-------------------------------------------------------------------------------
  Periods = incendio[,1]

  SP = as.double(incendio[,2])
  NVC = as.double(incendio[,3])
  EF = as.double(incendio[,4])
  nMin = as.integer(incendio[,5])

  # Tiempos a Periodos
  #-------------------------------------------------------------------------------
  FP = FP_min/PeriodTime
  RP = RP_min/PeriodTime
  DFP = DFP_min/PeriodTime
  FBRP = FBRP_min/PeriodTime

  C = C_hour*PeriodTime/60
  A = A_min/PeriodTime
  CFP = CFP_min/PeriodTime
  CRP = CRP_min/PeriodTime
  CTFP = CTFP_min/PeriodTime
  BPR = BPR_hour*PeriodTime/60


  # Input para las funciones
  #-------------------------------------------------------------------------------
  return(list(I=I,                # Set of aircraft to select.
              Periods=Periods,  # Set of time Periods.
              FP=FP,              # Maximum number of time periods with no rests.
              RP=RP,            # Number of time periods of rest.
              DFP=DFP,            # Maximum number time periods working.
              FBRP=FBRP,            # Number of time periods flying from fire to
              #   rest place and vice versa.
              A=A,                # Number of time periods to arrive to the
              #   wildfire.
              CFP=CFP,              # Number of time periods worked currently with
              #   no rests.
              CRP=CRP,          # Number of time periods rested currently.
              CTFP=CTFP,            # Number of time periods worked currently.
              C=C,                # Cost per period of the aircraft.
              P=P,                # Cost of select the aircraft.
              BPR=BPR,# Base yield of the aircraft in each time
              #   period.
              SP=SP,              # Perimeter of the wildfire in each time period.
              NVC=NVC,            # Incremental cost of the wildfire in each time
              #   period.
              EF=EF,              # Efficience of the aircraft in each time
              #   period.
              nMax=nMax,          # Maximum number of aircraft working in the
              #   wildfire in each time period.
              nMin=nMin           # Minimum number of aircraft working in the
              #   wildfire in each time period.
  ))
}


#-------------------------------------------------------------------------------
# Plot Scheduling
#-------------------------------------------------------------------------------

#' data.scheduling
#'
#' @param sol
#'
#' @return data
#' @export
data.scheduling <- function(sol){
  WRF<-(sol$Work+2*sol$Rest+3*sol$Fly)
  WRF <- WRF[sol$Selection==1, 1:max(which(WRF!=0,arr.ind = T)[,2])]
  if(sol$model=="rest_model"){
    WRF <- WRF[,-dim(WRF)[2]]
  }
  return(WRF)
}

#' data.scheduling.selection
#'
#' @param WRF matrix of 0's 1's 2's and 3's that represent the no-working, working, resting, and flying.
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

#' Scheduling plot
#'
#' @param WRF work, rest and fly matrix.
#'
#' @return scheduling plot.
#'
#' @import plotly
#' @import colorRamps
#'
#' @export
plotscheduling <- function(WRF){
  if(requireNamespace("plotly", quietly = TRUE) &
     requireNamespace("colorRamps", quietly = TRUE)){

    df <- data.frame(aero=numeric(), start=numeric(), end=numeric(), do=numeric())

    for(i in 1:dim(WRF)[1]){
      cont=T
      for(j in 1:dim(WRF)[2]){
        if(WRF[i,j]==1){
          df[dim(df)[1]+1,] = c(row.names(WRF)[i], j-1, j, "Work")
        }else if(WRF[i,j]==2){
          df[dim(df)[1]+1,] = c(row.names(WRF)[i], j-1, j, "Rest")
        }else if(WRF[i,j]==3){
          df[dim(df)[1]+1,] = c(row.names(WRF)[i], j-1, j, "Fly")
        }
      }
    }
    df$start<-as.numeric(df$start)
    df$end<-as.numeric(df$end)

    colors=c()
    if(sum(df$do=="Fly")>0){
      colors <- c(colors, "deepskyblue")
    }
    if(sum(df$do=="Rest")>0){
      colors <- c(colors, "green3")
    }
    if(sum(df$do=="Work")>0){
      colors <- c(colors, "firebrick1")
    }

    plotly::plot_ly(df, x =~start, xend=~end,
            y=~aero, yend=~aero,
            colors = c("dodgerblue3","green3","firebrick1")) %>%
      add_segments(color = ~do, type="scatter",
                   mode = 'lines', line = list(width = 3))%>%
      layout(xaxis = list(title = '<b>Periods</b>',
                          zeroline=F,
                          autotick=F),
             yaxis = list (title = '',
                           zeroline=F,
                           #showline=F,
                           showticklabels=T),
             margin=list(
               autoexpand=F,
               l=100,
               r=100,
               t=40)
      )
  }
}


#-------------------------------------------------------------------------------
# Plot Containment
#-------------------------------------------------------------------------------

#' data.contention
#'
#' @param data data of asa problem
#' @param sol solution of asa problem
#'
#' @return data
#' @export
data.contention <- function(data, sol){
  yield <- cumsum(col_sums(sol$Work*(data$BPR%*%t(data$EF))))
  contention <- ((data$SP-yield)*sol$Y[-1]/data$SP)[1:min(max(which(sol$Y==1)),length(sol$Y)-1)]
  df.contention <- data.frame(periods=as.numeric(names(contention)),
                              contention = contention)

  if(sol$model=="rest_model"){
    df.contention <- df.contention[-dim(df.contention)[1],]
  }

  return(df.contention)
}


#' Contention plot
#'
#' @param df
#'
#' @return contention plot
#' @import plotly
#'
#' @export
plotcontention <- function(df){
  if(requireNamespace("plotly", quietly = TRUE)){
    plot_ly(df, x=~periods, y=~contention, type="bar") %>%
      layout(xaxis = list(title = '<b>Periods</b>',
                          zeroline=F,
                          autotick=F),
             yaxis = list (title = '<b>No contention (proportion)</b>'
             ),
             margin=list(
               autoexpand=F,
               l=100,
               r=100,
               t=40)
      )
  }
}


#-------------------------------------------------------------------------------
# Plot Aircraft per period
#-------------------------------------------------------------------------------

#' Title
#'
#' @param sol solution of asa problem.
#'
#' @return the number of aircraft in each period.
#' @export
data.num.aircraft <- function(sol){
  n_aero_period = col_sums(sol$Work)[1:min(max(which(sol$Y==1)),length(sol$Y)-1)]
  df.n_aero_period <- data.frame(periods=as.numeric(names(n_aero_period)),
                              num = n_aero_period)

  if(sol$model=="rest_model"){
    df.n_aero_period <- df.n_aero_period[-dim(df.n_aero_period)[1],]
  }
  return(df.n_aero_period)
}


#' Aircraft plot
#'
#' @param df
#'
#' @return aircraft plot
#' @import plotly
#'
#' @export
plotnumaircraft <- function(df){
  if(requireNamespace("plotly", quietly = TRUE)){
    plotly::plot_ly(df, x=~periods, y=~num, type="bar") %>%
      layout(xaxis = list(title = '<b>Periods</b>',
                          zeroline=F,
                          autotick=F),
             yaxis = list (title = '<b>Number of aircraft</b>'
             ),
             margin=list(
               autoexpand=F,
               l=100,
               r=100,
               t=40)
      )
  }
}


#-------------------------------------------------------------------------------
# Plot Yield per period
#-------------------------------------------------------------------------------

#' Title
#'
#' @param data data of asa problem.
#' @param sol solution of asa problem.
#'
#' @export
data.yield <- function(data, sol){
  yield = col_sums(sol$Work*(data$BPR%*%t(data$EF)))[1:min(max(which(sol$Y==1)),length(sol$Y)-1)]
  df.yield <- data.frame(periods=as.numeric(names(yield)),
                              num = yield)

  if(sol$model=="rest_model"){
    df.yield <- df.yield[-dim(df.yield)[1],]
  }

  return(df.yield)
}


#' Yield plot
#'
#' @param df data frame
#'
#' @export
plotyield <- function(df){
  if(requireNamespace("plotly", quietly = TRUE)){
    plotly::plot_ly(df, x=~periods, y=~num, type="bar") %>%
      layout(xaxis = list(title = '<b>Periods</b>',
                          zeroline=F,
                          autotick=F),
             yaxis = list (title = '<b>Yield</b>'
             ),
             margin=list(
               autoexpand=F,
               l=100,
               r=100,
               t=40)
      )
  }
}

