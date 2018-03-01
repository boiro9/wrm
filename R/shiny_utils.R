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
#' Get data
#'
#' @param aeronaves 
#' @param incendio 
#' @param input 
#'
#' @return
#' @export
#'
#' @examples
get_data <- function(aeronaves, incendio, input){
  # Law
  #-----------------------------------------------------------------------------
  PeriodTime <- input$PeriodTime

  # Aircraft
  #-----------------------------------------------------------------------------
  I_col <- which(names(aeronaves) == 'Name')
  I <- aeronaves[,I_col]
  
  C_col <- which(names(aeronaves) == 'C')
  C_min <- as.double(aeronaves[,C_col])
  names(C_min) <- I
  
  P_col <- which(names(aeronaves) == 'P')
  P <- as.double(aeronaves[,P_col])
  names(P) <- I
  
  A_col <- which(names(aeronaves) == 'A')
  A_min <- as.integer(aeronaves[,A_col])
  names(A_min) <- I
  
  CFP_col <- which(names(aeronaves) == 'CFP')
  CFP_min <- as.integer(aeronaves[,CFP_col])
  names(CFP_min) <- I
  
  CRP_col <- which(names(aeronaves) == 'CRP')
  CRP_min <- as.integer(aeronaves[,CRP_col])
  names(CRP_min) <- I
  
  CTFP_col <- which(names(aeronaves) == 'CTFP')
  CTFP_min <- as.integer(aeronaves[,CTFP_col])
  names(CTFP_min) <- I
  
  BPR_col <- which(names(aeronaves) == 'BPR')
  BPR_hour <- as.double(aeronaves[,BPR_col])
  names(BPR_hour) <- I
  
  FBRP_col <- which(names(aeronaves) == 'FBRP')
  FBRP_min <- as.double(aeronaves[,FBRP_col])
  names(FBRP_min) <- I
  
  FP_col <- which(names(aeronaves) == 'FP')
  FP_min <- as.double(aeronaves[,FP_col])
  names(FP_min) <- I
  
  RP_col <- which(names(aeronaves) == 'RP')
  RP_min <- as.double(aeronaves[,RP_col])
  names(RP_min) <- I
  
  DFP_col <- which(names(aeronaves) == 'DFP')
  DFP_min <- as.double(aeronaves[,DFP_col])
  names(DFP_min) <- I
  
  ITW_col <- which(names(aeronaves) == 'ITW')
  ITW <- as.double(aeronaves[,ITW_col])
  names(ITW) <- I
  
  IOW_col <- which(names(aeronaves) == 'IOW')
  IOW <- as.double(aeronaves[,IOW_col])
  names(IOW) <- I
  
  I_G_col <- which(names(aeronaves) == 'G')
  I_G <- aeronaves[,I_G_col]
  names(I_G) <- I
  
  G <- unique(I_G)
  
  G_I <- list()
  for(g in G){
    G_I[[g]] <- I[which(I_G == g)]
  } 
  
  # Fire
  #----------------------------------------------------------------------------
  info_rows <- seq(2,dim(incendio)[1])
    
  TP_col <- which(names(incendio) == 'Period')
  PER_col <- which(names(incendio) == 'PER')
  NVC_col <- which(names(incendio) == 'NVC')
  EF_ini_col <- which(names(incendio) == 'EF')
  nMin_ini_col <- which(names(incendio) == 'nMin')
  nMax_ini_col <- which(names(incendio) == 'nMax')
  
  EF_col <- seq(EF_ini_col, nMin_ini_col-1)
  nMin_col <- seq(nMin_ini_col, nMax_ini_col-1)
  nMax_col <- seq(nMax_ini_col, dim(incendio)[2])
  
  TP <- incendio[info_rows,TP_col]
  
  PER <- as.double(incendio[info_rows,PER_col])
  names(PER) <- TP
  
  NVC <- as.double(incendio[info_rows,NVC_col])
  names(NVC) <- TP

  EF <- matrix(0, nrow = length(I), ncol = length(TP))
  row.names(EF) <- I
  colnames(EF) <- TP
  j <- 0
  for(i in I){
    j <- j+1
    EF[i, TP] <- as.double(sub(",", ".", incendio[info_rows,EF_col[j]], 
                             fixed = TRUE))
  }
  
  nMin <- matrix(0, nrow = length(G), ncol = length(TP))
  row.names(nMin) <- G
  colnames(nMin) <- TP
  nMax <- matrix(0, nrow = length(G), ncol = length(TP))
  row.names(nMax) <- G
  colnames(nMax) <- TP
  i <- 0
  for(g in G){
    i <- i+1
    nMin[g, TP] <- as.integer(incendio[info_rows,nMin_col[i]])
    nMax[g, TP] <- as.integer(incendio[info_rows,nMax_col[i]])
  }
  
  # Tiempos a Periodos
  #----------------------------------------------------------------------------
  FP <- FP_min/PeriodTime
  RP <- RP_min/PeriodTime
  DFP <- DFP_min/PeriodTime
  FBRP <- FBRP_min/PeriodTime

  C <- C_min*PeriodTime
  A <- A_min/PeriodTime
  CFP <- CFP_min/PeriodTime
  CRP <- CRP_min/PeriodTime
  CTFP <- CTFP_min/PeriodTime
  BPR <- BPR_hour*PeriodTime/60

  data <- list(I=I,        # Set of aircraft to select.
               G=G,        # Group of resources.
               G_I=G_I,    # Groups with resources information.
               T=TP,       # Set of time Periods.
               FP=FP,      # Maximum number of time periods with no rests.
               RP=RP,      # Number of time periods of rest.
               DFP=DFP,    # Maximum number time periods working.
               FBRP=FBRP,  # Number of time periods flying from fire to rest
                           #   place and vice versa.
               A=A,        # Number of time periods to arrive to the wildfire.
               CFP=CFP,    # Number of time periods worked currently with no 
                           #   rests.  
               CRP=CRP,    # Number of time periods rested currently.
               CTFP=CTFP,  # Number of time periods worked currently.
               C=C,        # Cost per period of the aircraft.
               P=P,        # Cost of select the aircraft.
               BPR=BPR,    # Base yield of the aircraft in each time period.
               PER=PER,    # Increment of the perimeter of the wildfire in each 
                           #   time period.
               NVC=NVC,    # Incremental cost of the wildfire in each time
                           #   period.
               EF=EF,      # Efficience of the aircraft in each time period.
               nMax=nMax,  # Maximum number of aircraft working in the wildfire
                           #   in each time period.
               nMin=nMin,  # Minimum number of aircraft working in the wildfire
                           #   in each time period.
               ITW=ITW,    # 1 if the resource is working in this wildfire.
               IOW=IOW     # 1 if the resource is working in other wildfire. 
  ) 
  
  
  # Input para las funciones
  #----------------------------------------------------------------------------
  return(data)
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
  
  indices = sol$Selection==1
  
  columns = 1:max(which(WRF!=0,arr.ind = T)[,2])
  rows = names(indices)[indices]
  
  WRF <- matrix(WRF[indices, columns], 
                   ncol=length(columns), 
                   nrow=length(rows),
                   dimnames=list(rows,columns)
                   )
  
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
  
  periods <- 1:(max(which(sol$Y==1))+1)
  
  contention <- numeric(length(periods))
  for(t in periods){
    contention[t] <- max(0,
      sum(data$PER[1:t])
      - sum(sol$Work[,1:t]*data$BPR*data$EF[,1:t])
      )
  }
  
  df.contention <- data.frame(periods=periods,
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
  n_aero_period = col_sums(sol$Work)[1:min(max(which(sol$Y==1))+1,length(sol$Y))]
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
  
  periods <- 1:(max(which(sol$Y==1))+1)
  
  yield <- numeric(length(periods))
  for(t in periods){
    yield[t] <- sum(sol$Work[,t]*data$BPR*data$EF[, t])
  }
  
  df.yield <- data.frame(periods=periods, yield = yield)

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
    plotly::plot_ly(df, x=~periods, y=~yield, type="bar") %>%
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

