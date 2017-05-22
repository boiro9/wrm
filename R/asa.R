
# ---------------------------------------------------------------------------- #
# Exact model
# ---------------------------------------------------------------------------- #

#' Model to solve the exact problem of Aircraft Selection and Allocation (ASA) for the containment of a wildfire.
#'
#' @param data a list with the needed information about aircrafts and wildfire. For more information see \code{\link{example_data}} function.
#' @param M_prime penalization for the breach of the minimum aircrafts on a wildfire.
#' @param solver solver name, 'gurobi', 'Rsymphony' or 'lpSolve'.
#'
#' @return information about the selection and allocation of the aircrafts.
#'
#' @import lpSolveAPI
#' @import Rsymphony
#'
#' @export
#'
#' @examples
#' data <- asa::example_data()
#' asa::asa(data, solver="lpSolveAPI")
asa <- function(data, M_prime=0, solver="gurobi"){
  #-----------------------------------------------------------------------------
  # Start time
  #-----------------------------------------------------------------------------

  start.time <- Sys.time()

  #-----------------------------------------------------------------------------
  # Load data
  #-----------------------------------------------------------------------------
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

  n<-length(I)                                          # numero de recursos
  m<-length(Periods)                                   # numero de periodos

  #-----------------------------------------------------------------------------
  # Calculos
  #-----------------------------------------------------------------------------
  PR = BPR%*%t(EF)
  PER = compute_PER(SP, m)

  M_prime <- max(100*(sum(C)+sum(NVC)),M_prime)
  M <- max(SP)+sum(EF%*%t(PR))

  l<-c() #numero de periodos que forman el tiempo de vuelo si sale en otro periodo
  for(i in 1:n){
    l[i] <- A[i]+floor((A[i]+CFP[i])/FP)*(RP+2*FBRP)+CRP[i]
  }

  #-----------------------------------------------------------------------------
  # Modelo matemático
  #-----------------------------------------------------------------------------

  # Orden de las variables de decision:
  #   S[i,t] : 0*(n*m)+t+(i-1)*m
  #  FL[i,t] : 1*(n*m)+t+(i-1)*m
  # R[i,t] : 2*(n*m)+t+(i-1)*m
  #  ER[i,t] : 3*(n*m)+t+(i-1)*m
  #   E[i,t] : 4*(n*m)+t+(i-1)*m
  #   U[i,t] : 5*(n*m)+t+(i-1)*m
  #   W[i,t] : 6*(n*m)+t+(i-1)*m
  #   Z[i]   : 7*(n*m)+i
  #  MU[t]   : 7*(n*m)+n+t
  #   Y[t-1] : 7*(n*m)+n+m+t
  #   Y[m]   : 7*(n*m)+n+m+m+1

  n_var<-7*(n*m)+n+2*m+1                               # numero de variables
  n_cons<-(n*m)+(n*m)+1+1+(n*m)+n+n+(n*m)+(n*m)+(n*m)+  #
    (n*m)+(n*m)+(n*m)+m+n+n+n+(n*m)+m+m                 # numero de restricciones

  # Type
  type = c(rep("B", n*m),  #   S[i,t]
           rep("B", n*m),  #  FL[i,t]
           rep("B", n*m),  # R[i,t]
           rep("B", n*m),  #  ER[i,t]
           rep("B", n*m),  #   E[i,t]
           rep("B", n*m),  #   U[i,t]
           rep("B", n*m),  #   W[i,t]
           rep("B", n)  ,  #   Z[i]
           rep("C", m)  ,  #  MU[t]
           rep("B", m)  ,  #   Y[t-1]
           rep("B", 1)  )  #   Y[m]

  # Objective function
  cost <- numeric(n_var)
  penalization <- numeric(n_var)

  # Constraints
  constr_R <- matrix(0, nrow = n*m, ncol = n_var)
  sense_R <- rep("=", n*m)
  rhs_R <- numeric(n*m)

  constr_W <- matrix(0, nrow = n*m, ncol = n_var)
  sense_W <- rep("=", n*m)
  rhs_W <- numeric(n*m)

  constr_no_content <- numeric(n_var)
  sense_no_content <- "="
  rhs_no_content <- 1

  constr_content <- numeric(n_var)
  sense_content <- "<="
  rhs_content <- numeric(1)

  constr_flights <- matrix(0, nrow = n*m, ncol = n_var)
  sense_flights <- rep("<=", n*m)
  rhs_flights <- numeric(n*m)

  constr_aircrafts_select <- matrix(0, nrow = n, ncol = n_var)
  sense_aircrafts_select <- rep("=", n)
  rhs_aircrafts_select <- numeric(n)

  constr_start_work <- matrix(0, nrow = n, ncol = n_var)
  sense_start_work <- rep("<=", n)
  rhs_start_work <- numeric(n)

  constr_rests_1 <- matrix(0, nrow = n*m, ncol = n_var)
  sense_rests_1 <- rep("<=", n*m)
  rhs_rests_1 <- rep(FP, n*m)

  constr_rests_2 <- matrix(0, nrow = n*m, ncol = n_var)
  sense_rests_2 <- rep(">=", n*m)
  rhs_rests_2 <- numeric(n*m)

  constr_rests_3 <- matrix(0, nrow = n*m, ncol = n_var)
  sense_rests_3 <- rep(">=", n*m)
  rhs_rests_3 <- numeric(n*m)

  constr_rests_4 <- matrix(0, nrow = n*m, ncol = n_var)
  sense_rests_4 <- rep("<=", n*m)
  rhs_rests_4 <- numeric(n*m)

  constr_rests_5 <- matrix(0, nrow = n*m, ncol = n_var)
  sense_rests_5 <- rep(">=", n*m)
  rhs_rests_5 <- numeric(n*m)

  constr_no_rest_when_flight <- matrix(0, nrow = n*m, ncol = n_var)
  sense_no_rest_when_flight <- rep("<=", n*m)
  rhs_no_rest_when_flight <- numeric(n*m)

  constr_content_yes_no <- matrix(0, nrow = m, ncol = n_var)
  sense_content_yes_no <- rep(">=", m)
  rhs_content_yes_no <- numeric(m)

  constr_start_end <- matrix(0, nrow = n, ncol = n_var)
  sense_start_end <- rep(">=", n)
  rhs_start_end <- numeric(n)

  constr_flight_time <- matrix(0, nrow = n, ncol = n_var)
  sense_flight_time <- rep("<=", n)
  rhs_flight_time <- DFP-CTFP

  constr_work <- matrix(0, nrow = n, ncol = n_var)
  sense_work <- rep(">=", n)
  rhs_work <- numeric(n)

  constr_fly_after_end <- matrix(0, nrow = n*m, ncol = n_var)
  sense_fly_after_end <- rep(">=", n*m)
  rhs_fly_after_end <- numeric(n*m)

  constr_n_aircraft_min <- matrix(0, nrow = m, ncol = n_var)
  sense_n_aircraft_min <- rep(">=", m)
  rhs_n_aircraft_min <- numeric(m)

  constr_n_aircraft_max <- matrix(0, nrow = m, ncol = n_var)
  sense_n_aircraft_max <- rep("<=", m)
  rhs_n_aircraft_max <- numeric(m)


  for(i in 1:n){
    for(t in 1:m){
      #=========================================================================
      # var U {i in I, t in T} = sum{j in 1..t} S[i,j] - sum{j in 1..(t-1)} E[i,j]
      #-------------------------------------------------------------------------
      constr_R[t+(i-1)*m, var_i("U",c(i,t),n,m)]   <-  1 # U[i,t]
      for(j in 1:t){
        constr_R[t+(i-1)*m, var_i("S",c(i,j),n,m)] <- -1 # - sum{j in 1..t} S[i,j]
      }
      for(j in 1:(t-1)){
        if(t-1>=1){
          constr_R[t+(i-1)*m, var_i("E",c(i,j),n,m)] <-  1 # + sum{j in 1..(t-1)} E[i,j]
        }
      }
      #=========================================================================

      #=========================================================================
      # var W {i in I, t in T} = U[i,t] - R[i,t] - FL[i,t]
      #-------------------------------------------------------------------------
      constr_W[t+(i-1)*m, var_i("W",c(i,t),n,m)]   <-  1 # W[i,t]
      constr_W[t+(i-1)*m, var_i("U",c(i,t),n,m)]   <- -1 # - U[i,t]
      constr_W[t+(i-1)*m, var_i("R",c(i,t),n,m)] <-  1 # + R[i,t]
      constr_W[t+(i-1)*m, var_i("FL",c(i,t),n,m)]  <-  1 # + FL[i,t]
      #=========================================================================

      #=========================================================================
      # minimize Cost: Coste + Penalization =
      #                sum{i in I, t in T} C[i]*U[i,t] + sum{t in T} NVC[t]*Y[t-1]
      #                + Y[m] + sum{i in I} P[i]*Z[i]
      #                + sum{t in T} M_prime*mu[t]
      #-------------------------------------------------------------------------
      cost[var_i("U",c(i,t),n,m)] <- C[i]     # sum{i in I, t in T} C[i]*U[i,t]
      cost[var_i("Y",c(t-1),n,m)] <- NVC[t]   # + sum{t in T} NVC[t]*Y[t-1]
      cost[var_i("Z",c(i),n,m)]   <- P[i]     # + sum{i in I} P[i]*Z[i]

      penalization[var_i("Y",c(m),n,m)]   <- 1        # + Y[m]
      penalization[var_i("MU",c(t),n,m)]  <- M_prime  # + sum{t in T} M_prime*mu[t]
      #=========================================================================

      #=========================================================================
      # subject to no_content:
      #   Y[0] = 1;
      #-------------------------------------------------------------------------
      constr_no_content[var_i("Y", 0,n,m)] <- 1     # Y[0]
      #=========================================================================

      #=========================================================================
      # subject to content:
      #   sum{t in T} PER[t]*Y[t-1] <= sum{i in I, t in T} PR[i,t]*W[i,t]
      #-------------------------------------------------------------------------
      constr_content[var_i("Y",c(t-1),n,m)] <-  PER[t]  #   sum{t in T} PER[t]*Y[t-1]
      constr_content[var_i("W",c(i,t),n,m)] <- -PR[i,t] # - sum{i in I, t in T} PR[i,t]*W[i,t]
      #=========================================================================

      #=========================================================================
      # subject to flights {i in I, t in T}:
      # sum{j in 1..t} S[i,j] - R[i,t] - sum{j in 1..(t-l[i])} S[i,j] <= FL[i,t]
      #-------------------------------------------------------------------------
      for(j in 1:t){
        constr_flights[t+(i-1)*m, var_i("S",c(i,j),n,m)] <- 1  # sum{j in 1..t} S[i,j]
      }
      constr_flights[t+(i-1)*m, var_i("R",c(i,t),n,m)] <- -1 # - R[i,t]
      if(t-l[i]>0){
        for(j in 1:(t-l[i])){
          constr_flights[t+(i-1)*m, var_i("S",c(i,j),n,m)] <- -1 +
            constr_flights[t+(i-1)*m, var_i("S",c(i,j),n,m)] # - sum{j in 1..(t-l[i])} S[i,j]
        }
      }
      constr_flights[t+(i-1)*m, var_i("FL",c(i,t),n,m)] <- -1   # - FL[i,t]
      #=========================================================================

      #=========================================================================
      # subject to aircrafts_select {i in I}:
      # sum{t in T} E[i,t] = Z[i];
      #-------------------------------------------------------------------------
      for(j in 1:m){
        constr_aircrafts_select[i, var_i("E",c(i,j),n,m)] <-  1 # sum{j in T} E[i,j]
      }
      constr_aircrafts_select[i, var_i("Z",c(i),n,m)]     <- -1 # - Z[i]
      #=========================================================================

      #=========================================================================
      # subject to start_work {i in I}:
      #   if A[i] == 0 then
      #   S[i,1]+sum{t in 2..m} (m+1)*S[i,t]
      #   else
      #     sum{t in T} S[i,t]
      #   <=
      #     if A[i] == 0 then
      #   m*Z[i]
      #   else
      #     Z[i]
      #-------------------------------------------------------------------------
      if(A[i]==0){
        for(j in 1:m){
          if(j==1){
            constr_start_work[i, var_i("S",c(i,1),n,m)] <- 1   # S[i,1]
          }else{
            constr_start_work[i, var_i("S",c(i,j),n,m)] <- m+1 # + sum{j in 2..m} (m+1)*S[i,j]
          }
        }
        constr_start_work[i, var_i("Z",c(i),n,m)]       <- -m # - m*Z[i]

      }else{
        for(j in 1:m){
          constr_start_work[i, var_i("S",c(i,j),n,m)]   <- 1 # sum{j in T} S[i,j]
        }
        constr_start_work[i, var_i("Z",c(i),n,m)]       <- -1 # - Z[i]
      }
      #=========================================================================

      #=========================================================================
      # subject to rests_1 {i in I, t in T}:
      #   FP >=
      #   if (CFP[i] == 0) or (CFP[i] >= 2) then
      # sum{j in 1..t} (t+1-j+CFP[i]-CRP[i])*S[i,j]
      # - sum{j in 1..t} (t-j)*E[i,j]
      # - sum{j in 1..t} R[i,j]
      # - sum{j in 1..t} FP*ER[i,j]
      # else
      #   (t+CFP[i]-CRP[i])*S[i,1]
      # + sum{j in 2..t} (FP+1)*S[i,j]
      # - sum{j in 1..t} (t-j)*E[i,j]
      # - sum{j in 1..t} R[i,j]
      # - sum{j in 1..t} FP*ER[i,j]
      #-------------------------------------------------------------------------
      if(CFP[i]==0 | CFP[i] >= 2){
        for(j in 1:t){
          constr_rests_1[t+(i-1)*m, var_i("S",c(i,j),n,m)]   <- t+1-j+CFP[i]-CRP[i] # sum{j in 1..t} (t+1-j+CFP[i]-CRP[i])*S[i,j]
          constr_rests_1[t+(i-1)*m, var_i("E",c(i,j),n,m)]   <- -t+j        # - sum{j in 1..t} (t-j)*E[i,j]
          constr_rests_1[t+(i-1)*m, var_i("R",c(i,j),n,m)] <- -1          # - sum{j in 1..t} R[i,j]
          constr_rests_1[t+(i-1)*m, var_i("ER",c(i,j),n,m)]  <- -FP         # - sum{j in 1..t} FP*ER[i,j]
        }
      }else{
        for(j in 1:t){
          if(j==1){
            constr_rests_1[t+(i-1)*m, var_i("S",c(i,1),n,m)] <- t+CFP[i]-CRP[i] # (t+CFP[i]-CRP[i])*S[i,1]
          }else{
            constr_rests_1[t+(i-1)*m, var_i("S",c(i,j),n,m)] <- FP+1            # + sum{j in 2..t} (FP+1)*S[i,j]
          }
          constr_rests_1[t+(i-1)*m, var_i("E",c(i,j),n,m)]   <- -t+j            # - sum{j in 1..t} (t-j)*E[i,j]
          constr_rests_1[t+(i-1)*m, var_i("R",c(i,j),n,m)] <- -1              # - sum{j in 1..t} R[i,j]
          constr_rests_1[t+(i-1)*m, var_i("ER",c(i,j),n,m)]  <- -FP             # - sum{j in 1..t} FP*ER[i,j]
        }
      }
      #=========================================================================

      #=========================================================================
      # subject to rests_2 {i in I, t in T}:
      #   0 <=
      #   if (CFP[i] == 0) or (CFP[i] >= 2) then
      # sum{j in 1..t} (t+1-j+CFP[i]-CRP[i])*S[i,j]
      # - sum{j in 1..t} (t-j)*E[i,j]
      # - sum{j in 1..t} R[i,j]
      # - sum{j in 1..t} FP*ER[i,j]
      # else
      #   (t+CFP[i]-CRP[i])*S[i,1]
      # + sum{j in 2..t} (FP+1)*S[i,j]
      # - sum{j in 1..t} (t-j)*E[i,j]
      # - sum{j in 1..t} R[i,j]
      # - sum{j in 1..t} FP*ER[i,j]
      #-------------------------------------------------------------------------
      if(CFP[i]==0 | CFP[i] >= 2){
        for(j in 1:t){
          constr_rests_2[t+(i-1)*m, var_i("S",c(i,j),n,m)]   <- t+1-j+CFP[i]-CRP[i] # sum{j in 1..t} (t+1-j+CFP[i]-CRP[i])*S[i,j]
          constr_rests_2[t+(i-1)*m, var_i("E",c(i,j),n,m)]   <- -t+j                # - sum{j in 1..t} (t-j)*E[i,j]
          constr_rests_2[t+(i-1)*m, var_i("R",c(i,j),n,m)] <- -1                  # - sum{j in 1..t} R[i,j]
          constr_rests_2[t+(i-1)*m, var_i("ER",c(i,j),n,m)]  <- -FP                 # - sum{j in 1..t} FP*ER[i,j]
        }
      }else{
        for(j in 1:t){
          if(j==1){
            constr_rests_2[t+(i-1)*m, var_i("S",c(i,1),n,m)] <- t+CFP[i]-CRP[i] # (t+CFP[i]-CRP[i])*S[i,1]
          }else{
            constr_rests_2[t+(i-1)*m, var_i("S",c(i,j),n,m)] <- FP+1            # + sum{j in 2..t} (FP+1)*S[i,j]
          }
          constr_rests_2[t+(i-1)*m, var_i("E",c(i,j),n,m)]   <- -t+j            # - sum{j in 1..t} (t-j)*E[i,j]
          constr_rests_2[t+(i-1)*m, var_i("R",c(i,j),n,m)] <- -1              # - sum{j in 1..t} R[i,j]
          constr_rests_2[t+(i-1)*m, var_i("ER",c(i,j),n,m)]  <- -FP             # - sum{j in 1..t} FP*ER[i,j]
        }
      }
      #=========================================================================

      #=========================================================================
      # subject to rests_3 {i in I, t in T}
      #   if t-RP>=0 then
      # sum{j in (t-RP+1)..t} R[i,j]
      # else
      #   sum{j in 1..t} R[i,j]
      # >=
      #   if t-RP>=0 then
      # RP*ER[i,t]
      # else
      #   (RP-CRP[i])*ER[i,t]
      #-------------------------------------------------------------------------
      if(t-RP >= 0){
        for(j in (t-RP+1):t){
          constr_rests_3[t+(i-1)*m, var_i("R",c(i,j),n,m)] <- 1              # sum{j in (t-RP+1)..t} R[i,j]
        }
        constr_rests_3[t+(i-1)*m, var_i("ER",c(i,t),n,m)]    <- -RP           # - RP*ER[i,t]
      }else{
        for(j in 1:t){
          constr_rests_3[t+(i-1)*m, var_i("R",c(i,j),n,m)] <- 1              # sum{j in 1..t} R[i,j]
        }
        constr_rests_3[t+(i-1)*m, var_i("ER",c(i,t),n,m)]    <- -(RP-CRP[i]) # - (RP-CRP[i])*ER[i,t]
      }
      #=========================================================================

      #=========================================================================
      # subject to rests_4 {i in I, t in T}:
      #   R[i,t] <=
      #   if t+RP <= m then
      # sum{j in t..(t+RP)} ER[i,j]
      # else
      #   0
      #-------------------------------------------------------------------------
      constr_rests_4[t+(i-1)*m, var_i("R",c(i,t),n,m)]    <- 1      # R[i,t]

      if(t+RP <= m){
        for(j in t:(t+RP)){
          constr_rests_4[t+(i-1)*m, var_i("ER",c(i,j),n,m)] <- -1 # - sum{j in t..(t+RP)} ER[i,j]
        }
      }
      #=========================================================================

      #=========================================================================
      # subject to rests_5 {i in I, t in T}:
      #   if t <= FBRP then
      # (-1-FBRP-(t-1))*R[i,t] + sum{j in 1..(t+FBRP)} (R[i,j] + FL[i,j])
      # else if t >= m-FBRP then
      # (-1-FBRP-(m-t))*R[i,t] + sum{j in (t-FBRP)..m} (R[i,j] + FL[i,j])
      # else
      #   (-1-2*FBRP)*R[i,t] + sum{j in (t-FBRP)..(t+FBRP)} (R[i,j] + FL[i,j])
      # >=
      #   0
      #-------------------------------------------------------------------------
      if(t <= FBRP){
        constr_rests_5[t+(i-1)*m, var_i("R",c(i,t),n,m)]     <- -1-FBRP-(t-1) # (-1-FBRP-(t-1))*R[i,t]
        for(j in 1:(t+FBRP)){
          constr_rests_5[t+(i-1)*m, var_i("R",c(i,j),n,m)]   <-  1 +
            constr_rests_5[t+(i-1)*m, var_i("R",c(i,j),n,m)]                 # + sum{j in 1..(t+FBRP)} R[i,j]
          constr_rests_5[t+(i-1)*m, var_i("FL",c(i,j),n,m)]    <-  1           # + sum{j in 1..(t+FBRP)}  FL[i,j]
        }
      }else if(t >= m-FBRP){
        constr_rests_5[t+(i-1)*m, var_i("R",c(i,t),n,m)]     <- -1-FBRP-(m-t) # (-1-FBRP-(m-t))*R[i,t]
        for(j in (t-FBRP):m){
          constr_rests_5[t+(i-1)*m, var_i("R",c(i,j),n,m)]   <-  1 +
            constr_rests_5[t+(i-1)*m, var_i("R",c(i,j),n,m)]                 # + sum{j in (t-FBRP)..m} R[i,j]
          constr_rests_5[t+(i-1)*m, var_i("FL",c(i,j),n,m)]    <-  1           # + sum{j in (t-FBRP)..m}  FL[i,j]
        }
      }else{
        constr_rests_5[t+(i-1)*m, var_i("R",c(i,t),n,m)]     <- -1-2*FBRP     # (-1-2*FBRP)*R[i,t]
        for(j in (t-FBRP):(t+FBRP)){
          constr_rests_5[t+(i-1)*m, var_i("R",c(i,j),n,m)]   <-  1 +
            constr_rests_5[t+(i-1)*m, var_i("R",c(i,j),n,m)]                 # + sum{j in (t-FBRP)..(t+FBRP)} R[i,j]
          constr_rests_5[t+(i-1)*m, var_i("FL",c(i,j),n,m)]    <-  1           # + sum{j in (t-FBRP)..(t+FBRP)}  FL[i,j]
        }
      }
      #=========================================================================

      #=========================================================================
      # subject to no_rest_when_flight {i in I, t in T}:
      #   R[i,t] + FL[i,t] <= U[i,t]
      #-------------------------------------------------------------------------
      constr_no_rest_when_flight[t+(i-1)*m, var_i("R",c(i,t),n,m)] <-   1 # R[i,t]
      constr_no_rest_when_flight[t+(i-1)*m, var_i("FL",c(i,t),n,m)]  <-   1 #  FL[i,t]
      constr_no_rest_when_flight[t+(i-1)*m, var_i("U",c(i,t),n,m)]   <- - 1 #   U[i,t]
      #=========================================================================

      #=========================================================================
      # subject to content_yes_no {t in T}:
      #  sum{i in I, j in 1..t} PR[i,j]*W[i,j] >= SP[t]*Y[t-1] - M*Y[t]
      #-------------------------------------------------------------------------
      for(i1 in 1:n){
        for(j in 1:t){
          constr_content_yes_no[t, var_i("W",c(i1,j),n,m)] <-  PR[i1, j] # sum{i1 in I, j in 1..t} PR[i1,j]*W[i1,j]
        }
      }
      constr_content_yes_no[t, var_i("Y",c(t-1),n,m)]      <- -SP[t]     # - SP[t]*Y[t-1]
      constr_content_yes_no[t, var_i("Y",c(t),n,m)]        <-  M         # + M*Y[t]
      #=========================================================================

      #=========================================================================
      # subject to start_end {i in I}:
      #   sum{t in T} t*E[i,t] >= sum{t in T} t*S[i,t]
      #-------------------------------------------------------------------------
      for(j in 1:m){
        constr_start_end[i, var_i("E",c(i,j),n,m)] <-  j # sum{j in T} j*E[i,j]
        constr_start_end[i, var_i("S",c(i,j),n,m)] <- -j # - sum{j in T} j*S[i,j]
      }
      #=========================================================================

      #=========================================================================
      # subject to flight_time {i in I}:
      #   sum{t in T} U[i,t] <= DFP-CTFP[i]
      #-------------------------------------------------------------------------
      for(j in 1:m){
        constr_flight_time[i, var_i("U",c(i,j),n,m)] <- 1 # sum{j in T} U[i,j]
      }
      #=========================================================================

      #=========================================================================
      # subject to work {i in I}:
      #   sum{t in T} W[i,t] >= Z[i]
      #-------------------------------------------------------------------------
      for(j in 1:m){
        constr_work[i, var_i("U",c(i,j),n,m)] <-  1 # sum{j in T} U[i,j]
        constr_work[i, var_i("Z",c(i),n,m)]   <- -1 # - Z[i]
      }
      #=========================================================================

      #=========================================================================
      # subject to fly_after_end {i in I, t in T}:
      #   sum{j in (t-FBRP+1)..t: (t-FBRP+1)>=1 and (t-FBRP+1)<=t} FL[i,j] >= FBRP*E[i,t]
      #-------------------------------------------------------------------------
      if((t-FBRP+1)>=1 & (t-FBRP+1)<=t){
        for(j in (t-FBRP+1):t){
          constr_fly_after_end[t+(i-1)*m, var_i("FL",c(i,j),n,m)] <-  1   # sum{j in (t-FBRP+1)..t: (t-FBRP+1)>=1 and (t-FBRP+1)<=t} FL[i,j]
        }
      }
      constr_fly_after_end[t+(i-1)*m, var_i("E",c(i,t),n,m)]      <- -FBRP # - FBRP*E[i,t]
      #=========================================================================

      #=========================================================================
      # subject to n_aircraft_min {t in T}:
      #   sum{i in I} W[i,t] >= nMin[t]*Y[t-1] - mu[t]
      #-------------------------------------------------------------------------
      for(i1 in 1:n){
        constr_n_aircraft_min[t, var_i("W",c(i1,t),n,m)] <-  1       # sum{i1 in I} W[i1,t]
      }
      constr_n_aircraft_min[t, var_i("Y",c(t-1),n,m)]    <- -nMin[t] # - nMin[t]*Y[t-1]
      constr_n_aircraft_min[t, var_i("MU",c(t),n,m)]     <-  1       # + mu[t]
      #=========================================================================

      #=========================================================================
      # subject to n_aircraft_max {t in T}:
      #   sum{i in I} W[i,t] <= nMax*Y[t-1]
      #-------------------------------------------------------------------------
      for(i1 in 1:n){
        constr_n_aircraft_max[t, var_i("W",c(i1,t),n,m)] <- 1      # sum{i1 in I} W[i1,t]
      }
      constr_n_aircraft_max[t, var_i("Y",c(t-1),n,m)]    <- -nMax  # - nMax*Y[t-1]
      #=========================================================================
    }
  }

  obj = cost+penalization

  constr = rbind(constr_R,
                 constr_W,
                 constr_no_content,
                 constr_content,
                 constr_flights,
                 constr_aircrafts_select,
                 constr_start_work,
                 constr_rests_1,
                 constr_rests_2,
                 constr_rests_3,
                 constr_rests_4,
                 constr_rests_5,
                 constr_no_rest_when_flight,
                 constr_content_yes_no,
                 constr_start_end,
                 constr_flight_time,
                 constr_work,
                 constr_fly_after_end,
                 constr_n_aircraft_min,
                 constr_n_aircraft_max)

  sense = c(sense_R,
            sense_W,
            sense_no_content,
            sense_content,
            sense_flights,
            sense_aircrafts_select,
            sense_start_work,
            sense_rests_1,
            sense_rests_2,
            sense_rests_3,
            sense_rests_4,
            sense_rests_5,
            sense_no_rest_when_flight,
            sense_content_yes_no,
            sense_start_end,
            sense_flight_time,
            sense_work,
            sense_fly_after_end,
            sense_n_aircraft_min,
            sense_n_aircraft_max)

  rhs = c(rhs_R,
          rhs_W,
          rhs_no_content,
          rhs_content,
          rhs_flights,
          rhs_aircrafts_select,
          rhs_start_work,
          rhs_rests_1,
          rhs_rests_2,
          rhs_rests_3,
          rhs_rests_4,
          rhs_rests_5,
          rhs_no_rest_when_flight,
          rhs_content_yes_no,
          rhs_start_end,
          rhs_flight_time,
          rhs_work,
          rhs_fly_after_end,
          rhs_n_aircraft_min,
          rhs_n_aircraft_max)

  require_gurobi=require("gurobi")
  if(solver=="gurobi" &
     require_gurobi &
     requireNamespace("slam", quietly = TRUE)){

    S_rec<-list()
    S_rec$A<-constr
    S_rec$obj<-obj
    S_rec$sense<-sense
    S_rec$rhs<-rhs
    S_rec$vtypes<-type
    S_rec$lb<-numeric(n_var)
    S_rec$modelsense<-"min"

    sol<-gurobi(S_rec, list(OutputFlag = 0))
    x<-sol$x
    obj_value <- sol$objval
    sol_result<-sol$status

  }else if(solver=="lpSolve" &
           requireNamespace("lpSolveAPI", quietly = TRUE)){
    S_rec<-make.lp(n_cons, n_var)

    set.objfn(S_rec, obj)

    for(j in 1:n_cons) set.row(S_rec, j, constr[j,])
    set.rhs(S_rec, rhs)
    set.constr.type(S_rec, sense)

    type_C <- which(type=="C")
    type_B <- which(type=="S")

    set.type(S_rec, type_C, "real")
    set.type(S_rec, type_B, "binary")

    resolver<-solve(S_rec)
    if(resolver==0){
      sol_result <-"OPTIMAL"
    }else{
      sol_result <- "INFEASIBLE"
    }
    obj_value <- get.objective(S_rec)
    x<-get.variables(S_rec)
  }else if(requireNamespace("Rsymphony", quietly = TRUE)){
    sense[sense=="="] <- "=="

    sol <- Rsymphony_solve_LP(obj, constr, sense, rhs, types = type, max = F)

    obj_value <- sol$objval
    x <- sol$solution
    resolver <- sol$status

    if(resolver==0){
      sol_result <-"OPTIMAL"
    }else{
      sol_result <- "INFEASIBLE"
    }
  }

  if(sol_result=="OPTIMAL"){
    #   S[i,t] : 0*(n*m)+t+(i-1)*m
    S = matrix(x[1:(n*m)],nrow = n, ncol = m, byrow = T)
    row.names(S) <- I
    colnames(S) <- Periods

    #  FL[i,t] : 1*(n*m)+t+(i-1)*m
    FL = matrix(x[1*(n*m)+1:(n*m)],nrow = n, ncol = m, byrow = T)
    row.names(FL) <- I
    colnames(FL) <- Periods

    # R[i,t] : 2*(n*m)+t+(i-1)*m
    R = matrix(x[2*(n*m)+1:(n*m)],nrow = n, ncol = m, byrow = T)
    row.names(R) <- I
    colnames(R) <- Periods

    #  ER[i,t] : 3*(n*m)+t+(i-1)*m
    ER = matrix(x[3*(n*m)+1:(n*m)],nrow = n, ncol = m, byrow = T)
    row.names(ER) <- I
    colnames(ER) <- Periods

    #   E[i,t] : 4*(n*m)+t+(i-1)*m
    E = matrix(x[4*(n*m)+1:(n*m)],nrow = n, ncol = m, byrow = T)
    row.names(E) <- I
    colnames(E) <- Periods

    #   U[i,t] : 5*(n*m)+t+(i-1)*m
    U = matrix(x[5*(n*m)+1:(n*m)],nrow = n, ncol = m, byrow = T)
    row.names(U) <- I
    colnames(U) <- Periods

    #   W[i,t] : 6*(n*m)+t+(i-1)*m
    W = matrix(x[6*(n*m)+1:(n*m)],nrow = n, ncol = m, byrow = T)
    row.names(W) <- I
    colnames(W) <- Periods

    #   Z[i]   : 7*(n*m)+i
    Z = matrix(x[7*(n*m)+1:(n)],nrow = n, byrow = T)
    row.names(Z) <- I

    #  MU[t]   : 7*(n*m)+n+t
    MU = matrix(x[7*(n*m)+n+1:(m)], ncol = m, byrow = T)
    colnames(MU) <- Periods

    #   Y[t-1] : 7*(n*m)+n+m+t
    #   Y[m]   : 7*(n*m)+n+m+m+1
    Y = matrix(x[7*(n*m)+n+m+1:(m+1)], ncol = m+1, byrow = T)
    colnames(Y) <- c("0",Periods)

    results <- list(sol_result=sol_result,
                    time = Sys.time() - start.time,
                    obj=obj_value,
                    cost=t(cost)%*%x,
                    penalty=t(penalization)%*%x,
                    Start=S,
                    Fly=FL,
                    Rest=R,
                    End=E,
                    Scheduling=U,
                    Work=W,
                    Selection=Z,
                    mu=MU,
                    Y=Y)
  }else{
    params <- get_model_params(data)
    results<-infeasible_solution(params, solver, start.time, n, m)
  }

  return(results)
}


