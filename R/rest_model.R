# ---------------------------------------------------------------------------- #
# Model 2: Start period of selected aircrafts
# ---------------------------------------------------------------------------- #
#' Build and solve the rest model.
#'
#' @description Mathematical model to solve the aircraft scheduling problem.
#'
#' @param I_select selected aircraft.
#' @param m_select last period to estabish the aircraft rests.
#' @param S_fix_hist aircraft start period.
#' @param params common model params.
#' @param M_prime penalty for the aircraft number breach.
#' @param solver solver name: 'gurobi', 'lpSolve' or 'Rsymphony'.
#' @param solver_params list of gurobi options. Defaults to list(TimeLimit=600, OutputFlag = 0).
#' 
#' @return list with the optimal value of the variables, objective function, ...
#'
#' @import lpSolveAPI
#' @import Rsymphony
#' @import slam
#'
#' @export
#'
#' @examples
#' data <- example_data()
#' params <- get_model_params(data)
#' I_select <- rep(T, length(params$I))
#' m_select <- length(params$Periods)
#' S_fix_hist = list()
#' M_prime = 1000000
#' rest_model(I_select, m_select, S_fix_hist, params, M_prime, solver="lpSolve")
#'
rest_model <- function(I_select, m_select, S_fix_hist, params, M_prime, solver="gurobi", solver_params=list(TimeLimit=600, OutputFlag=0)){
  #-----------------------------------------------------------------------------
  # Datos
  #-----------------------------------------------------------------------------

  FP=params$FP
  RP=params$RP
  DFP=params$DFP
  FBRP=params$FBRP

  I=params$I[I_select]
  A=params$A[I_select]
  CFP=params$CFP[I_select]
  CRP=params$CRP[I_select]
  CTFP=params$CTFP[I_select]
  C=params$C[I_select]
  P=params$P[I_select]

  Periods=params$Periods[1:m_select]
  SP=params$SP[1:m_select]
  NVC=params$NVC[1:m_select]
  PER=params$PER[1:m_select]
  nMin = params$nMin[1:m_select]
  nMax = params$nMax

  PR=params$PR[I_select, 1:m_select]

  n = length(I)
  m = m_select


  #-----------------------------------------------------------------------------
  # Modelo matemático 2
  #-----------------------------------------------------------------------------
  # Orden de las variables de decision:
  #  S[i,t] : 0*(n*m)+t+(i-1)*m
  #  E[i,t] : 1*(n*m)+t+(i-1)*m
  #  FL[i,t] : 2*(n*m)+t+(i-1)*m
  #  R[i,t] : 3*(n*m)+t+(i-1)*m
  # ER[i,t] : 4*(n*m)+t+(i-1)*m
  #  U[i,t] : 5*(n*m)+t+(i-1)*m
  #  W[i,t] : 6*(n*m)+t+(i-1)*m
  # mu[t]   : 7*(n*m)+t
  # MU[t]   : 7*(n*m)+m+t

  n_var <- 7*(n*m)+2*m                                                # Number of variables
  n_cons <- (n*m)+(n*m)+3*n+8*(n*m)+2*n+(n*m)+2*m+length(S_fix_hist)  # Number of constraints

  # Type
  type = c(rep("B", n*m),  #  S[i,t]
            rep("B", n*m),  #  E[i,t]
            rep("B", n*m),  #  FL[i,t]
            rep("B", n*m),  #  R[i,t]
            rep("B", n*m),  # ER[i,t]
            rep("B", n*m),  #  U[i,t]
            rep("B", n*m),  #  W[i,t]
            rep("C", m)  ,  # mu[t]
            rep("C", m)     # MU[t]
  )

  # Objective function
  yield <- numeric(n_var)
  penalty <- numeric(n_var)

  # Constraints
  constr_C <- matrix(0, nrow = n*m, ncol = n_var)
  sense_C <- rep("=", n*m)
  rhs_C <- numeric(n*m)

  constr_W <- matrix(0, nrow = n*m, ncol = n_var)
  sense_W <- rep("=", n*m)
  rhs_W <- numeric(n*m)

  constr_one_start <- matrix(0, nrow = n, ncol = n_var)
  sense_one_start <- rep("<=", n)
  rhs_one_start <- rep(1,n)

  constr_one_end <- matrix(0, nrow = n, ncol = n_var)
  sense_one_end <- rep("<=", n)
  rhs_one_end <- rep(0,n)

  constr_start_before_end <- matrix(0, nrow = n, ncol = n_var)
  sense_start_before_end <- rep("<=", n)
  rhs_start_before_end <- numeric(n)

  constr_fly_rest_betwen_work <- matrix(0, nrow = n*m, ncol = n_var)
  sense_fly_rest_betwen_work <- rep("<=", n*m)
  rhs_fly_rest_betwen_work <- numeric(n*m)

  constr_start_flight <- matrix(0, nrow = n*m, ncol = n_var)
  sense_start_flight <- rep(">=", n*m)
  rhs_start_flight <- numeric(n*m)

  constr_flights <- matrix(0, nrow = n*m, ncol = n_var)
  sense_flights <- rep(">=", n*m)
  rhs_flights <- numeric(n*m)

  constr_rest1 <- matrix(0, nrow = n*m, ncol = n_var)
  sense_rest1 <- rep("<=", n*m)
  rhs_rest1 <- numeric(n*m)

  constr_rest2 <- matrix(0, nrow = n*m, ncol = n_var)
  sense_rest2 <- rep(">=", n*m)
  rhs_rest2 <- numeric(n*m)

  constr_rest3 <- matrix(0, nrow = n*m, ncol = n_var)
  sense_rest3 <- rep(">=", n*m)
  rhs_rest3 <- numeric(n*m)

  constr_rest4 <- matrix(0, nrow = n*m, ncol = n_var)
  sense_rest4 <- rep("<=", n*m)
  rhs_rest4 <- numeric(n*m)

  constr_rest5 <- matrix(0, nrow = n*m, ncol = n_var)
  sense_rest5 <- rep(">=", n*m)
  rhs_rest5 <- numeric(n*m)

  constr_flight_time <- matrix(0, nrow = n, ncol = n_var)
  sense_flight_time <- rep("<=", n)
  rhs_flight_time <- DFP-CTFP

  constr_start_activity <- matrix(0, nrow = n, ncol = n_var)
  sense_start_activity <- rep("<=", n)
  rhs_start_activity <- numeric(n)

  constr_fly_after_end <- matrix(0, nrow = n*m, ncol = n_var)
  sense_fly_after_end <- rep(">=", n*m)
  rhs_fly_after_end <- numeric(n*m)

  constr_n_aero_min <- matrix(0, nrow = m, ncol = n_var)
  sense_n_aero_min <- rep(">=", m)
  rhs_n_aero_min <- nMin

  constr_n_aero_max <- matrix(0, nrow = m, ncol = n_var)
  sense_n_aero_max <- rep("<=", m)
  rhs_n_aero_max <- rep(nMax,m)

  constr_change <- matrix(0, nrow = length(S_fix_hist), ncol = n_var)
  sense_change <- rep(">=", length(S_fix_hist))
  rhs_change <- numeric(length(S_fix_hist))


  for(i in 1:n){
    for(t in 1:m){
      #=========================================================================
      # var U {i in I_select, t in T_select} = sum{j in 1..t} S[i,j] - sum{j in 1..(t-1)} E[i,j]
      #-------------------------------------------------------------------------
      constr_C[t+(i-1)*m, varh2_i("U",c(i,t),n,m)]   <-  1  # U[i,t]
      for(j in 1:t){
        constr_C[t+(i-1)*m, varh2_i("S",c(i,j),n,m)] <- -1  # - sum{j in 1..t} S[i,j]
      }
      if(1<=t-1){
        for(j in 1:(t-1)){
          constr_C[t+(i-1)*m, varh2_i("E",c(i,j),n,m)] <- 1 # + sum{j in 1..(t-1)} E[i,j]
        }
      }
      #=========================================================================

      #=========================================================================
      # var W {i in I_select, t in T_select} = U[i,t] - R[i,t] - FL[i,t]
      #-------------------------------------------------------------------------
      constr_W[t+(i-1)*m, varh2_i("W",c(i,t),n,m)]   <-  1 # var C1 {i in I, t in T}
      constr_W[t+(i-1)*m, varh2_i("U",c(i,t),n,m)]   <- -1 # - U[i,t]
      constr_W[t+(i-1)*m, varh2_i("R",c(i,t),n,m)]   <-  1 # + R[i,t]
      constr_W[t+(i-1)*m, varh2_i("FL",c(i,t),n,m)]   <-  1 # + FL[i,t]
      #=========================================================================

      #=========================================================================
      # maximize Total_Yield: Yield - Penalty =
      #   sum{i in I_select, t in T_select} PR[i,t]*W[i,t]
      #   - sum{i in I_select, t in T_select} 0.001*U[i,t]
      #   - sum{t in T_select} M_prime*mu[t] - sum{t in T_select} M_prime/2*MU[t]
      #-------------------------------------------------------------------------
      yield[varh2_i("W",c(i,t),n,m)] <- PR[i,t]  # sum{i in I_select, t in T_select} PR[i,t]*W[i,t]

      penalty[varh2_i("U",c(i,t),n,m)] <- - 0.001    # - sum{i in I_select, t in T_select} 0.001*U[i,t]
      penalty[varh2_i("mu",c(t),n,m)] <- - M_prime   # - sum{t in T_select} M_prime*mu[t]
      penalty[varh2_i("MU",c(t),n,m)] <- - M_prime/2 # - sum{t in T_select} M_prime/2*MU[t]
      #=========================================================================

      #=========================================================================
      # subject to one_start {i in I_select}:
      #   sum{t in T_select} S[i,t] <= 1
      #-------------------------------------------------------------------------
      constr_one_start[i, varh2_i("S", c(i,t),n,m)] <- 1 # sum{t in T_select} S[i,t]
      #=========================================================================

      #=========================================================================
      # subject to one_end {i in I_select}:
      #   sum{t in T_select} E[i,t] <= sum{t in T_select} S[i,t]
      #-------------------------------------------------------------------------
      constr_one_end[i, varh2_i("E",c(i,t),n,m)] <-  1 #  sum{t in T_select} E[i,t]
      constr_one_end[i, varh2_i("S",c(i,t),n,m)] <- -1 # -sum{t in T_select} E[i,t]
      #=========================================================================

      #=========================================================================
      # subject to start_before_end {i in I_select}:
      #   sum{t in T_select} t*S[i,t] <= sum{t in T_select} t*E[i,t]
      #-------------------------------------------------------------------------
      constr_start_before_end[i, varh2_i("S",c(i,t),n,m)] <- t  # sum{t in T_select} t*S[i,t]
      constr_start_before_end[i, varh2_i("E",c(i,t),n,m)] <- -t # - sum{t in T_select} t*E[i,t]
      #=========================================================================

      #=========================================================================
      # subject to fly_rest_betwen_work {i in I_select, t in T_select}:
      #   FL[i,t] + R[i,t] <= sum{j in 1..t} S[i,j] - sum{j in 1..(t-1)} E[i,j]
      #-------------------------------------------------------------------------
      constr_fly_rest_betwen_work[t+(i-1)*m, varh2_i("FL",c(i,t),n,m)]   <-  1 # FL[i,t]
      constr_fly_rest_betwen_work[t+(i-1)*m, varh2_i("R",c(i,t),n,m)]   <- -1 # + R[i,t]
      for(j in 1:t){
        constr_fly_rest_betwen_work[t+(i-1)*m, varh2_i("S",c(i,j),n,m)] <- -1 # -sum{j in 1..t} S[i,j]
      }
      if(1<=t-1){
        for(j in 1:(t-1)){
          constr_fly_rest_betwen_work[t+(i-1)*m, varh2_i("E",c(i,j),n,m)] <-  1 # +sum{j in 1..(t-1)} E[i,j]
        }
      }
      #=========================================================================

      #=========================================================================
      # subject to start_flight {i in I_select, t in T_select}:
      #   sum{j in t..(t+A[i]-1): t+A[i]-1 <= m_select} FL[i,j] - A[i]*S[i,t] >= 0
      #-------------------------------------------------------------------------
      if(t<=t+A[i]-1){
        for(j in t:min(t+A[i]-1, m)){
          constr_start_flight[t+(i-1)*m, varh2_i("FL",c(i,j),n,m)] <- 1 # sum{j in t..(t+A[i]-1): t+A[i]-1 <= m_select} FL[i,j]
        }
      }

      constr_start_flight[t+(i-1)*m, varh2_i("S",c(i,t),n,m)] <- -A[i] # -A[i]*W[i,t]
      #=========================================================================

      #=========================================================================
      # subject to flights {i in I_select, t in T_select}:
      #   - FL[i,t]
      #   + sum{j in (t-FBRP)..(t+FBRP): j > 0 and j <= m_select} R[i,j]
      #   + sum{j in (t-A[i])..t: j > 0} S[i,j]
      #   + sum{j in t..(t+FBRP-1): j <= m_select} E[i,j] >= 0
      #-------------------------------------------------------------------------
      constr_flights[t+(i-1)*m, varh2_i("FL",c(i,t),n,m)] <- -1 # - FL[i,t]
      for(j in max(t-FBRP, 1):min(t+FBRP, m)){
        constr_flights[t+(i-1)*m, varh2_i("R",c(i,j),n,m)] <- 1 # + sum{j in (t-FBRP)..(t+FBRP): j > 0 and j <= m_select} R[i,j]
      }
      if(t-A[i]<=t){
        for(j in (t-A[i]):t){
          constr_flights[t+(i-1)*m, varh2_i("S",c(i,j),n,m)] <- 1 # + sum{j in (t-A[i])..t: j > 0} S[i,j]
        }
      }
      if(t<=t+FBRP-1){
        for(j in t:(t+FBRP-1)){
          if(j<=m){
            constr_flights[t+(i-1)*m, varh2_i("E",c(i,j),n,m)] <- 1 # + sum{j in (t-FBRP)..(t+FBRP): j > 0 and j <= m_select} R[i,j]
          }
        }
      }
      #=========================================================================

      #=========================================================================
      # subject to rest1 {i in I_select, t in T_select}:
      #   sum{j in 1..t} U[i,j] - sum{j in 1..t} R[i,j] - sum{j in 1..t} FP*ER[i,j]
      #   <= FP - CFP[i] + CRP[i]
      #-------------------------------------------------------------------------
      for(j in 1:t){
        constr_rest1[t+(i-1)*m, varh2_i("U",c(i,j),n,m)] <-  1   # sum{j in 1..t} U[i,j]
        constr_rest1[t+(i-1)*m, varh2_i("R",c(i,j),n,m)] <- -1   # - sum{j in 1..t} R[i,j]
        constr_rest1[t+(i-1)*m, varh2_i("ER",c(i,j),n,m)] <- -FP # - sum{j in 1..t} FP*ER[i,j]
      }
      rhs_rest1[t+(i-1)*m] <- FP-CFP[i]+CRP[i] # FP-CFP[i]+CRP[i]
      #=========================================================================

      #=========================================================================
      # subject to rest {i in I_select, t in T_select}:
      #   sum{j in 1..t} U[i,j] - sum{j in 1..t} R[i,j] - sum{j in 1..t} FP*ER[i,j]
      #   >= CRP[i]-CFP[i]
      #-------------------------------------------------------------------------
      for(j in 1:t){
        constr_rest2[t+(i-1)*m, varh2_i("U",c(i,j),n,m)] <-  1   # sum{j in 1..t} U[i,j]
        constr_rest2[t+(i-1)*m, varh2_i("R",c(i,j),n,m)] <- -1   # - sum{j in 1..t} R[i,j]
        constr_rest2[t+(i-1)*m, varh2_i("ER",c(i,j),n,m)] <- -FP # - sum{j in 1..t} FP*ER[i,j]
      }
      rhs_rest2[t+(i-1)*m] <- CRP[i]-CFP[i] # FP-CFP[i]+CRP[i]
      #=========================================================================

      #=========================================================================
      # subject to rest3 {i in I_select, t in T_select}:
      #   if t-RP>=0 then
      #     sum{j in (t-RP+1)..t} R[i,j]-RP*ER[i,t]
      #   else
      #     sum{j in 1..t} R[i,j]-(RP-CRP[i])*ER[i,t]
      #   >=
      #   0
      #-------------------------------------------------------------------------
      if(t-RP+1<=t){
        for(j in max(t-RP+1,1):t){
          constr_rest3[t+(i-1)*m, varh2_i("R",c(i,j),n,m)] <-  1          #    sum{j in (t-RP+1)..t} R[i,j]
        }
        constr_rest3[t+(i-1)*m, varh2_i("ER",c(i,t),n,m)] <- -RP           #    -RP*ER[i,t]
      }else{
        for(j in 1:t){                                                       # else
          constr_rest3[t+(i-1)*m, varh2_i("R",c(i,j),n,m)] <-  1            #    sum{j in 1..t} R[i,j]
        }
        constr_rest3[t+(i-1)*m, varh2_i("ER",c(i,t),n,m)] <- -(RP-CRP[i]) #    -(RP-CRP[i])*ER[i,t]
      }
      #=========================================================================

      #=========================================================================
      # subject to rest4 {i in I_select, t in T_select}:
      #   R[i,t] - sum{j in t..min(t+RP-1, m_select)} ER[i,j] <= 0
      #-------------------------------------------------------------------------
      constr_rest4[t+(i-1)*m, varh2_i("R",c(i,t),n,m)] <- 1    # R[i,t]
      if(t<=t+RP-1){
        for(j in t:min(t+RP-1, m)){
          constr_rest4[t+(i-1)*m, varh2_i("ER",c(i,j),n,m)] <- -1 # - sum{j in t..min(t+RP, m_select)} ER[i,j]
        }
      }

      #=========================================================================

      #=========================================================================
      # subject to rest5 {i in I_select, t in T_select}:
      #   if t <= FBRP then
      #     (-1-FBRP-(t-1))*R[i,t] + sum{j in 1..(t+FBRP)} (R[i,j] + FL[i,j])
      #   else if t >= m_select-FBRP then
      #     (-1-FBRP-(m_select-t))*R[i,t] + sum{j in (t-FBRP)..m_select} (R[i,j] + FL[i,j])
      #   else
      #     (-1-2*FBRP)*R[i,t] + sum{j in (t-FBRP)..(t+FBRP)} (R[i,j] + FL[i,j])
      #   >=
      #     0
      #-------------------------------------------------------------------------
      if(t<=FBRP){
        constr_rest5[t+(i-1)*m, varh2_i("R",c(i,t),n,m)] <- -FBRP-t     # (-1-FBRP-(t-1))*R[i,t]
        for(j in 1:(t+FBRP)){
          constr_rest5[t+(i-1)*m, varh2_i("R",c(i,j),n,m)] <- 1 +      # + sum{j in 1..(t+FBRP)} R[i,j]
            constr_rest5[t+(i-1)*m, varh2_i("R",c(i,j),n,m)]
          constr_rest5[t+(i-1)*m, varh2_i("FL",c(i,j),n,m)] <- 1        # + sum{j in 1..(t+FBRP)} FL[i,j]
        }
      }else if(t>=m-FBRP){
        constr_rest5[t+(i-1)*m, varh2_i("R",c(i,t),n,m)] <- -1-FBRP-m+t # (-1-FBRP-(m_select-t))*R[i,t]
        for(j in (t-FBRP):m){
          constr_rest5[t+(i-1)*m, varh2_i("R",c(i,j),n,m)] <- 1 +      # + sum{j in (t-FBRP)..m_select} R[i,j]
            constr_rest5[t+(i-1)*m, varh2_i("R",c(i,j),n,m)]
          constr_rest5[t+(i-1)*m, varh2_i("FL",c(i,j),n,m)] <- 1        # + sum{j in (t-FBRP)..m_select} FL[i,j]
        }
      }else{
        constr_rest5[t+(i-1)*m, varh2_i("R",c(i,t),n,m)] <- -1-2*FBRP   # (-1-2*FBRP)*R[i,t]
        for(j in (t-FBRP):(t+FBRP)){
          constr_rest5[t+(i-1)*m, varh2_i("R",c(i,j),n,m)] <- 1 +      # + sum{j in (t-FBRP)..(t+FBRP)} R[i,j]
            constr_rest5[t+(i-1)*m, varh2_i("R",c(i,j),n,m)]
          constr_rest5[t+(i-1)*m, varh2_i("FL",c(i,j),n,m)] <- 1        # + sum{j in (t-FBRP)..(t+FBRP)} FL[i,j]
        }
      }
      #=========================================================================

      #=========================================================================

      #=========================================================================
      # subject to flight_time {i in I_select}:
      #   sum{t in T_select} U[i,t] <= DFP-CTFP[i]
      #-------------------------------------------------------------------------
      for(j in 1:m){
        constr_flight_time[i, varh2_i("U",c(i,j),n,m)] <- 1 # sum{t in T_select} U[i,t]
      }
      #=========================================================================

      #=========================================================================
      # subject to start_activity {i in I_select}:
      #   if A[i] == 0 then
      #     S[i,1]+sum{t in 2..m_select} (m_select+1)*S[i,t]
      #   else
      #     sum{t in T_select} S[i,t]
      #   <=
      #   if A[i] == 0 then
      #     m
      #   else
      #     1
      #-------------------------------------------------------------------------
      if(A[i]==0){
        constr_start_activity[i, varh2_i("S",c(i,1),n,m)] <- 1     # S[i,t]
        for(j in 2:m){
          constr_start_activity[i, varh2_i("S",c(i,j),n,m)] <- m+1 # +sum{t in 2..m_select} (m_select+1)*S[i,t]
        }
        rhs_start_activity[i] <- m
      }else{
        for(j in 1:m){
          constr_start_activity[i, varh2_i("S",c(i,j),n,m)] <- 1 # +sum{t in 2..m_select} (m_select+1)*S[i,t]
        }
        rhs_start_activity[i] <- 1
      }
      #=========================================================================

      #=========================================================================
      # subject to fly_after_end {i in I_select, t in T_select}:
      #   sum{j in (t-FBRP+1)..t: j>=1 and j<=t} FL[i,j] >= FBRP*E[i,t]
      #-------------------------------------------------------------------------
      if(t-FBRP+1<=t){
        for(j in max(t-FBRP+1,1):t){
          constr_fly_after_end[t+(i-1)*m, varh2_i("FL",c(i,j),n,m)] <- 1 # sum{j in (t-FBRP+1)..t: (t-FBRP+1)>=1 and (t-FBRP+1)<=t} FL[i,j]
        }
      }
      constr_fly_after_end[t+(i-1)*m, varh2_i("E",c(i,t),n,m)] <- -FBRP # -FBRP*E[i,t]
      #=========================================================================

      #=========================================================================
      # subject to n_aero_min {t in T_select}:
      #   sum{i in I_select} W[i,t] >= nMin[t] - mu[t]
      #-------------------------------------------------------------------------
      for(i1 in 1:n){
        constr_n_aero_min[t, varh2_i("W",c(i1,t),n,m)] <- 1 # sum{i in I_select} W[i,t]
      }
      constr_n_aero_min[t, varh2_i("mu",c(t),n,m)] <- 1     # + mu[t]
      #=========================================================================

      #=========================================================================
      # subject to n_aero_max {t in T_select}:
      #   sum{i in I_select} W[i,t] <= nMax + MU[t]
      #-------------------------------------------------------------------------
      for(i1 in 1:n){
        constr_n_aero_max[t, varh2_i("W",c(i1,t),n,m)] <- 1 # sum{i in I_select} W[i,t]
      }
      constr_n_aero_max[t, varh2_i("MU",c(t),n,m)] <- -1     # + MU[t]
      #=========================================================================

      #=========================================================================
      # subject to change {S_fix_hist}:
      #  + sum{i in I_select, j in T_select: S1[i,j]==0} S[i,j]
      #  - sum{i in I_select, j in T_select: S1[i,j]==1} S[i,j]
      #   >= 1 - sum{i in I_select, j in T_select: S1[i,j]==1} 1
      #-------------------------------------------------------------------------
      if(length(S_fix_hist)>=1){
        for(s in 1:length(S_fix_hist)){
          if(S_fix_hist[[s]][I_select,][i,t]==0){
            constr_change[s, varh2_i("S",c(i,t),n,m)] <-  1 # + sum{i in I_select, j in T_select: S1[i,j]==0} S[i,j]
          }else{
            constr_change[s, varh2_i("S",c(i,t),n,m)] <- -1 # - sum{i in I_select, j in T_select: S1[i,j]==1} S[i,j]
          }

          rhs_change[s] <- 1-sum(S_fix_hist[[s]])
        }
      }
      #=========================================================================
    }
  }

  obj = yield+penalty

  constr = rbind(constr_C,
                  constr_W,
                  constr_one_start,
                  constr_one_end,
                  constr_start_before_end,
                  constr_fly_rest_betwen_work,
                  constr_start_flight,
                  constr_flights,
                  constr_rest1,
                  constr_rest2,
                  constr_rest3,
                  constr_rest4,
                  constr_rest5,
                  constr_flight_time,
                  constr_start_activity,
                  constr_fly_after_end,
                  constr_n_aero_min,
                  constr_n_aero_max,
                  constr_change
  )

  sense = c(sense_C,
             sense_W,
             sense_one_start,
             sense_one_end,
             sense_start_before_end,
             sense_fly_rest_betwen_work,
             sense_start_flight,
             sense_flights,
             sense_rest1,
             sense_rest2,
             sense_rest3,
             sense_rest4,
             sense_rest5,
             sense_flight_time,
             sense_start_activity,
             sense_fly_after_end,
             sense_n_aero_min,
             sense_n_aero_max,
             sense_change
  )

  rhs = c(rhs_C,
           rhs_W,
           rhs_one_start,
           rhs_one_end,
           rhs_start_before_end,
           rhs_fly_rest_betwen_work,
           rhs_start_flight,
           rhs_flights,
           rhs_rest1,
           rhs_rest2,
           rhs_rest3,
           rhs_rest4,
           rhs_rest5,
           rhs_flight_time,
           rhs_start_activity,
           rhs_fly_after_end,
           rhs_n_aero_min,
           rhs_n_aero_max,
           rhs_change
  )

  require_gurobi=require("gurobi")
  if(solver=="gurobi" &
     requireNamespace("slam", quietly = TRUE) &
     require_gurobi){

    heuristic<-list()
    heuristic$A<-constr
    heuristic$obj<-obj
    heuristic$sense<-sense
    heuristic$rhs<-rhs
    heuristic$vtypes<-type
    heuristic$lb<-numeric(n_var)
    heuristic$modelsense<-"max"

    sol<-gurobi(heuristic, solver_params)
    x<-sol$x
    obj_value <- sol$objval
    sol_result<-sol$status

  }else if(solver=="lpSolve" &
           requireNamespace("lpSolveAPI", quietly = TRUE)){

    SP_mod<-make.lp(n_cons,n_var)

    set.objfn(SP_mod, -obj)

    for(j in 1:n_cons) set.row(SP_mod, j, constr[j,])
    set.rhs(SP_mod, rhs)
    set.constr.type(SP_mod, sense)

    type_C <- which(type=="C")
    type_B <- which(type=="B")

    set.type(SP_mod, type_C, "real")
    set.type(SP_mod, type_B, "binary")

    resolver<-solve(SP_mod)
    if(resolver==0){
      sol_result <-"OPTIMAL"
    }else{
      sol_result <- "INFEASIBLE"
    }
    obj_value <- -get.objective(SP_mod)
    x <- get.variables(SP_mod)
  }else if(solver=="Rsymphony" &
           requireNamespace("Rsymphony", quietly = TRUE)){

    sense[sense=="="] <- "=="

    sol <- Rsymphony_solve_LP(obj, constr, sense, rhs, types = type, max = T)

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
    #  S[i,t]
    S = matrix(x[1:(n*m)],nrow = n, ncol = m, byrow = T)
    row.names(S) <- I
    colnames(S) <- Periods

    #  E[i,t]
    E = matrix(x[(n*m+1):(2*n*m)],nrow = n, ncol = m, byrow = T)
    row.names(E) <- I
    colnames(E) <- Periods

    #  FL[i,t]
    FL = matrix(x[(2*n*m+1):(3*n*m)],nrow = n, ncol = m, byrow = T)
    row.names(FL) <- I
    colnames(FL) <- Periods

    #  R[i,t]
    R = matrix(x[(3*n*m+1):(4*n*m)],nrow = n, ncol = m, byrow = T)
    row.names(R) <- I
    colnames(R) <- Periods

    #  ER[i,t]
    ER = matrix(x[(4*n*m+1):(5*n*m)],nrow = n, ncol = m, byrow = T)
    row.names(ER) <- I
    colnames(ER) <- Periods

    #  U[i,t]
    U = matrix(x[(5*n*m+1):(6*n*m)],nrow = n, ncol = m, byrow = T)
    row.names(U) <- I
    colnames(U) <- Periods

    #  W[i,t]
    W = matrix(x[(6*n*m+1):(7*n*m)],nrow = n, ncol = m, byrow = T)
    row.names(W) <- I
    colnames(W) <- Periods

    #  mu[t]
    mu = matrix(x[(7*n*m+1):(7*n*m+m)], ncol = m, byrow = T)
    colnames(mu) <- Periods

    #  MU[t]
    MU = matrix(x[(7*n*m+m+1):(7*n*m+2*m)], ncol = m, byrow = T)
    colnames(MU) <- Periods

    results <- list(sol_result=sol_result,
                    obj=obj_value,
                    yield=t(yield)%*%x,
                    penalty=t(penalty)%*%x,
                    S = S,
                    E = E,
                    FL = FL,
                    R = R,
                    ER = ER,
                    U = U,
                    W = W,
                    mu = mu,
                    MU = MU
    )

    return(results)
  }else{
    return(return(list(sol_result="INFEASIBLE")))
  }
}
