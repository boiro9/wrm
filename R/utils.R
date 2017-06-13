#-------------------------------------------------------------------------------
# get_model_params
#-------------------------------------------------------------------------------
#' get model paramas
#'
#' @param data data with the aircraft, law and fire information
#'
#' @return SaA model params
#'
#' @examples
#' data <-example_data()
#' get_model_params(data)
get_model_params <- function(data){
  I=data$I
  Periods=data$Periods
  FP=data$FP
  RP=data$RP
  DFP=data$DFP
  FBRP=data$FBRP
  A=data$A
  CFP=data$CFP
  CRP=data$CRP
  CTFP=data$CTFP
  C=data$C
  P=data$P
  BPR=data$BPR
  SP=data$SP
  NVC=data$NVC
  EF=data$EF
  nMax=data$nMax
  nMin=data$nMin

  #-----------------------------------------------------------------------------
  # Numero aeronaves y periodos
  #-----------------------------------------------------------------------------
  n<-length(I)        # Number of aircrafts
  m<-length(Periods) # Number of periods

  #-----------------------------------------------------------------------------
  # Compute model params
  #-----------------------------------------------------------------------------
  # Aircrafts yields
  PR = BPR%*%t(EF)

  # Perimeter increment
  PER = compute_PER(SP,m)

  #-----------------------------------------------------------------------------
  # Important params
  #-----------------------------------------------------------------------------
  params <- list(I=I, Periods=Periods,
                 FP=FP, RP=RP, DFP=DFP, FBRP=FBRP,
                 A=A, CFP=CFP, CRP=CRP, CTFP=CTFP,
                 C=C, P=P, SP=SP, NVC=NVC,
                 PR=PR, PER=PER,
                 nMax=nMax, nMin=nMin)

  return(params)
}

# ---------------------------------------------------------------------------- #
# Update params
# ---------------------------------------------------------------------------- #
#' Compute the perimeter increment of the wildfire in each period.
#'
#' @param SP perimeter of the wildfire in each period.
#' @param m number of periods.
#'
#' @return perimeter increment.
#'
#' @examples
#' data <-example_data()
#' compute_PER(data$SP, length(data$SP))
compute_PER <- function(SP, m){
  PER = c()
  for(t in 1:m){
    if(t==1){
      PER[t] = SP[t]
    }else{
      PER[t] = SP[t]-SP[t-1]
    }
  }

  return(PER)
}

#' Update the minimum number of aircrafts in each period.
#'
#' @param nMin  Minimum number of aircrafts working in the wildfire in each time period.
#' @param W_fix work matrix.
#' @param FBRP Number of time periods flying from fire to rest place and vice versa.
#' @param m number of periods.
#'
#' @return updated nMin.
#'
update_nMin <- function(nMin, W_fix, FBRP, m){
  # Miramos a partir de que periodo entra la primera aeronave. Antes de esa,
  #   nMin pasa a ser 0.
  if(requireNamespace("slam", quietly = TRUE)){
    if((m-FBRP+1)<=m){
      W_fix[,(m-FBRP+1):m] <- 0
    }
    num <- which(col_sums(W_fix)!=0)
    if(length(num)>0){
      nMin[1:(min(num)-1)] <- 0
    }
    
    return(nMin)
  }
}

update_work <- function(I_select, m, S_fix, W_fix, FL_fix, R_fix, params){

  FP=params$FP
  RP=params$RP
  FBRP=params$FBRP
  CFP=params$CFP
  CRP=params$CRP
  A=params$A

  who_starts = which(S_fix==1, arr.ind = T)
  Start <- who_starts[order(who_starts[,1]),][,2]

  for(i in which(I_select)){
    time_fly = CFP[i]+1
    time_rest = CRP[i]
    for(t in 1:m){
      if(Start[i] <= t){
        if(t<Start[i]+A[i]){
          time_fly <- time_fly+1
          W_fix[i,t] = 0
          FL_fix[i,t] = 1
        }else{
          if(time_rest==0){
            if(time_fly<FP-FBRP){
              time_fly <- time_fly+1
              W_fix[i,t] = 1
            }else if(time_fly==FP-FBRP){
              time_fly <- 0
              time_rest <- 1
              W_fix[i,t] = 1
            }else if(time_fly==FP){
              time_fly <- 0
              time_rest <- 2 # Ya está realizando el vuelo de descanso
              W_fix[i,t] <- 0
              FL_fix[i,t] <- 1
            }
          }else if(time_rest<=FBRP){
            time_rest <- time_rest+1
            W_fix[i,t] = 0
            FL_fix[i,t] = 1
          }else if(time_rest<=FBRP+RP){
            time_rest <- time_rest+1
            W_fix[i,t] = 0
            R_fix[i,t] = 1
          }else if(time_rest<RP+2*FBRP){
            time_rest <-time_rest+1
            W_fix[i,t] = 0
            FL_fix[i,t] = 1
          }else if(time_rest==RP+2*FBRP){
            time_rest <- 0
            time_fly <- 1
            W_fix[i,t] = 0
            FL_fix[i,t] = 1
          }
        }

      }else{
        W_fix[i,t] = 0
        if(CFP[i]>0){
          Start[i] <- max(Start[i], RP)
          time_fly <- 1
          time_rest <- 0
        }else if(CRP[i]>0){
          Start[i] <- max(Start[i], CRP[i])
          time_fly <- 1
          time_rest <- 0
        }
      }
    }
  }
  return(list(W_fix=W_fix, FL_fix=FL_fix, R_fix=R_fix))
}



# ---------------------------------------------------------------------------- #
# Utilities
# ---------------------------------------------------------------------------- #
varh1_i <- function(var, index, n, m){
  if(var=="E"){
    if(length(index)==2){
      return(index[2]+(index[1]-1)*m)
    }else{
      print("Wrong number of index.")
      return()
    }
  }else if(var=="W"){
    if(length(index)==2){
      return(n*m+index[2]+(index[1]-1)*m)
    }else{
      print("Wrong number of index.")
      return()
    }
  }else if(var=="U"){
    if(length(index)==2){
      return(2*n*m+index[2]+(index[1]-1)*m)
    }else{
      print("Wrong number of index.")
      return()
    }
  }else if(var=="Z"){
    if(length(index)==1){
      return(3*n*m+index[1])
    }else{
      print("Wrong number of index.")
      return()
    }
  }else if(var=="mu"){
    if(length(index)==1){
      return(3*n*m+n+index[1])
    }else{
      print("Wrong number of index.")
      return()
    }
  }else if(var=="MU"){
    if(length(index)==1){
      return(3*n*m+n+m+index[1])
    }else{
      print("Wrong number of index.")
      return()
    }
  }else if(var=="Y"){
    if(length(index)==1){
      return(3*n*m+n+2*m+index[1]+1)
    }else{
      print("Wrong number of index.")
      return()
    }
  }
}

varh2_i <- function(var, index, n, m){
  if(var=="S"){
    if(length(index)==2){
      return(index[2]+(index[1]-1)*m)
    }else{
      print("Wrong number of index.")
      return()
    }
  }else if(var=="E"){
    if(length(index)==2){
      return(n*m+index[2]+(index[1]-1)*m)
    }else{
      print("Wrong number of index.")
      return()
    }
  }else if(var=="FL"){
    if(length(index)==2){
      return(2*n*m+index[2]+(index[1]-1)*m)
    }else{
      print("Wrong number of index.")
      return()
    }
  }else if(var=="R"){
    if(length(index)==2){
      return(3*n*m+index[2]+(index[1]-1)*m)
    }else{
      print("Wrong number of index.")
      return()
    }
  }else if(var=="ER"){
    if(length(index)==2){
      return(4*n*m+index[2]+(index[1]-1)*m)
    }else{
      print("Wrong number of index.")
      return()
    }
  }else if(var=="U"){
    if(length(index)==2){
      return(5*n*m+index[2]+(index[1]-1)*m)
    }else{
      print("Wrong number of index.")
      return()
    }
  }else if(var=="W"){
    if(length(index)==2){
      return(6*n*m+index[2]+(index[1]-1)*m)
    }else{
      print("Wrong number of index.")
      return()
    }
  }else if(var=="mu"){
    if(length(index)==1){
      return(7*n*m+index[1])
    }else{
      print("Wrong number of index.")
      return()
    }
  }else if(var=="MU"){
    if(length(index)==1){
      return(7*n*m+m+index[1])
    }else{
      print("Wrong number of index.")
      return()
    }
  }else{
    print("Variable doesn't found.")
    return()
  }
}


var_i <- function(var, index, n, m){
  if(var=="S"){
    if(length(index)==2){
      return(index[2]+(index[1]-1)*m)
    }else{
      print("Wrong number of index.")
      return()
    }
  }else if(var=="FL"){
    if(length(index)==2){
      return(n*m+index[2]+(index[1]-1)*m)
    }else{
      print("Wrong number of index.")
      return()
    }
  }else if(var=="R"){
    if(length(index)==2){
      return(2*n*m+index[2]+(index[1]-1)*m)
    }else{
      print("Wrong number of index.")
      return()
    }
  }else if(var=="ER"){
    if(length(index)==2){
      return(3*n*m+index[2]+(index[1]-1)*m)
    }else{
      print("Wrong number of index.")
      return()
    }
  }else if(var=="E"){
    if(length(index)==2){
      return(4*n*m+index[2]+(index[1]-1)*m)
    }else{
      print("Wrong number of index.")
      return()
    }
  }else if(var=="U"){
    if(length(index)==2){
      return(5*n*m+index[2]+(index[1]-1)*m)
    }else{
      print("Wrong number of index.")
      return()
    }
  }else if(var=="W"){
    if(length(index)==2){
      return(6*n*m+index[2]+(index[1]-1)*m)
    }else{
      print("Wrong number of index.")
      return()
    }
  }else if(var=="Z"){
    if(length(index)==1){
      return(7*n*m+index[1])
    }else{
      print("Wrong number of index.")
      return()
    }
  }else if(var=="MU"){
    if(length(index)==1){
      return(7*n*m+n+index[1])
    }else{
      print("Wrong number of index.")
      return()
    }
  }else if(var=="Y"){
    if(length(index)==1){
      return(7*n*m+n+m+index[1]+1)
    }else{
      print("Wrong number of index.")
      return()
    }
  }
}
