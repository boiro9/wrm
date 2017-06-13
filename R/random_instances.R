#-------------------------------------------------------------------------------
#' Generate random set of aircrafts.
#'
#' @param num_air number of aircrafts.
#' @param min_A minimum number of periods spending flying to the wildfire.
#' @param max_A maximum number of periods spending flying to the wildfire.
#' @param min_CFP minimum number of periods that aircraft flies without breaks.
#' @param max_CFP maximum number of periods that aircraft flies without breaks.
#' @param min_CTFP minimum number of periods that aircraft work in the day.
#' @param max_CTFP maximum number of periods that aircraft work in the day.
#'
#' @return list of aircrafts.
#'
#' @examples
#' random_aircrafts(10)
random_aircrafts <- function(num_air, 
                             min_A=0, max_A=10,
                             min_CFP=0, max_CFP=10,
                             min_CTFP=0, max_CTFP=40){
  I = as.character(1:num_air)
  A = sample(x = seq(min_A, max_A), size = num_air, replace = T)
  CFP = sample(x = seq(min_CFP, max_CFP), size = num_air, replace = T)
  CRP = numeric(num_air)
  CTFP = sample(x = seq(min_CTFP, max_CTFP), size = num_air, replace = T)
  aircrafts = data.frame(name=c("BellB412",
                                "BellB212",
                                "Ka32",
                                "BellB407"),
                         C=c(210,150,120,150),
                         P=c(1000,700,1000,1000),
                         BPR=c(0.4,0.25,0.22,0.3))
  air_type = sample(seq(dim(aircrafts)[1]), size=num_air, replace = T)
  C = aircrafts[air_type, "C"]
  P = aircrafts[air_type, "P"]
  BPR = aircrafts[air_type, "BPR"]
  return(list(I=I, A=A, CFP=CFP, CRP=CRP, CTFP=CTFP, C=C, P=P, BPR=BPR))
}
# ---------------------------------------------------------------------------- #


#-------------------------------------------------------------------------------
#' Generate random set of wildfire periods.
#'
#' @param num_per number of periods.
#' @param ini_SP_min minimum value for the perimeter (SP) on the first period.
#' @param ini_SP_max maximum value for the perimeter (SP) on the first period.
#' @param ini_NVC_min minimum value for the wildfire cost (NVC) on the first period.
#' @param ini_NVC_max maximum value for the wildfire cost (NVC) on the first period.
#' @param rate_min minimum rate value to represent the evolution of the wildfire from a period to another.
#' @param rate_max maximum rate value to represent the evolution of the wildfire from a period to another.
#' @param EF_min minimum efficiency value of aircraft in each period.
#' @param EF_max maximum efficiency value of aircraft in each period.
#' @param nMin_min vector with the minimum number of minimum number of aircraft (nMin) in each period.
#' @param nMin_max vector with the maximum number of minimum number of aircraft (nMin) in each period.
#'
#' @return list of wildfire periods.
#'
#' @examples
#' random_fire(10)
random_fire <- function(num_per,
                        ini_SP_min=1, ini_SP_max=3,
                        ini_NVC_min = 500, ini_NVC_max=2000,
                        rate_min=1, rate_max=1.3,
                        EF_min=0.5, EF_max=1,
                        nMin_min=numeric(num_per), nMin_max=rep(2,num_per)){
  Periods = as.character(1:num_per)
  ini_SP = runif(1, ini_SP_min, ini_SP_max)
  SP = c(ini_SP)
  ini_NVC = runif(1, ini_NVC_min, ini_NVC_max)
  NVC = c(ini_NVC)
  nMin = c()
  for(p in 1:num_per){
    if(p>1){
      rate = runif(1, rate_min, rate_max)
      SP[p] = rate*SP[p-1] 
      NVC[p] = rate*NVC[p-1]
    }
    nMin[p] = round(runif(1, nMin_min[p], nMin_max[p]))
  }
  EF = runif(num_per, EF_min, EF_max)
  return(list(Periods=Periods, SP=SP, NVC=NVC, EF=EF, nMin=nMin))
}
# ---------------------------------------------------------------------------- #


# ------------------------------------------------------------------------------
#' Generate random law.
#'
#' @param nMax_min minimum number of maximum number of aircraft (nMax).
#' @param nMax_max maximum number of maximum number of aircraft (nMax).
#'
#' @return list with law information
#'
#' @examples
#' random_law()
random_law <- function(nMax_min=2, nMax_max=8){
  nMax = ceiling(runif(1, nMax_min, nMax_max))
  return(list(FP=12, RP=4, DFP=48, FBRP=1, nMax=nMax))
}
# ---------------------------------------------------------------------------- #


# ------------------------------------------------------------------------------
#' Generate random data.
#'
#' @param num_air number of aircrafts.
#' @param num_per number of periods.
#'
#' @return list with data information.
#' @export
#'
#' @examples
#' random_instance(5, 10)
random_instance <- function(num_air, num_per){
  air_data = random_aircrafts(num_air, min_A = 0, max_A = num_per/4)
  nMin_max = c(max(sum(air_data$A==0)-1, 0))
  for(p in 2:num_per){
    nMin_max[p] = nMin_max[p-1]+max(sum(air_data$A==p-1)-1, 0)
  }
  fire_data = random_fire(num_per, nMin_max=nMin_max)
  nMax_min = max(fire_data$nMin)
  law_data = random_law(nMax_min = nMax_min, nMax_max = max(num_air/2,nMax_min))
  
  data = c(air_data, fire_data, law_data)
  
  return(data)
}
# ---------------------------------------------------------------------------- #
