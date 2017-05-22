#-------------------------------------------------------------------------------
# Example
#-------------------------------------------------------------------------------

#' Data example for the Selection and Allocation problem.
#'
#' @return list with the following information:
#' \tabular{ll}{
#' \code{I}        \tab Set of aircrafts to select.\cr
#' \code{Periods}  \tab Set of time Periods.\cr
#' \code{FP}       \tab Maximum number of time periods with no rests.\cr
#' \code{RP}       \tab Number of time periods of rest.\cr
#' \code{DFP}      \tab Maximum number time periods working.\cr
#' \code{FBRP}     \tab Number of time periods flying from fire to rest place and vice versa.\cr
#' \code{A}        \tab Number of time periods to arrive to the wildfire.\cr
#' \code{CFP}      \tab Number of time periods worked currently with no rests.\cr
#' \code{CRP}      \tab Number of time periods rested currently.\cr
#' \code{CTFP}     \tab Number of time periods worked currently.\cr
#' \code{C}        \tab Cost per period of the aircrafts.\cr
#' \code{P}        \tab Cost of select the aircrafts.\cr
#' \code{BPR}      \tab Base yield of the aircrafts in each time period.\cr
#' \code{SP}       \tab Perimeter of the wildfire in each time period.\cr
#' \code{NVC}      \tab Incremental cost of the wildfire in each time period.\cr
#' \code{EF}       \tab Efficience of the aircrafts in each time period.\cr
#' \code{nMax}     \tab Maximum number of aircrafts working in the wildfire in each time period.\cr
#' \code{nMin}     \tab Minimum number of aircrafts working in the wildfire in each time period.\cr
#' }
#' @export
#'
#' @examples
#' example_data()
#'
example_data <- function(){
  # Set of aircraft to select.
  I=c("BellB412_1","BellB412_2")

  # Set of time Periods.
  Periods=c("1","2","3","4")

  # Maximum number of time periods with no rests.
  FP=12

  # Number of time periods of rest.
  RP=4

  # Maximum number time periods working.
  DFP=48

  # Number of time periods flying from fire to rest place and vice versa.
  FBRP=1

  # Number of time periods to arrive to the wildfire.
  A=c(2,2)

  # Number of time periods worked currently with no rests.
  CFP=c(0,0)

  # Number of time periods rested currently.
  CRP=c(0,0)

  # Number of time periods worked currently.
  CTFP=c(0,0)

  # Cost per period of aircraft.
  C=c(33.33,33.33)

  # Cost of select aircraft.
  P=c(100,100)

  # Base yield of aircraft.
  BPR=c(4.34,4.34)

  # Perimeter of the wildfire in each time period.
  SP=c(5.6,5.7,5.8,6)

  # Incremental cost of the wildfire in each time period.
  NVC=c(70,140,200,270)

  # Efficience of the aircrafts in each time period.
  EF=c(1,1,1,1)

  # Maximum number of aircrafts working in the wildfire in each time period.
  nMax=2

  # Minimum number of aircrafts working in the wildfire in each time period.
  nMin=c(0,0,0,0)

  data = list(I=I,
              Periods=Periods,
              FP=FP,
              RP=RP,
              DFP=DFP,
              FBRP=FBRP,
              A=A,
              CFP=CFP,
              CRP=CRP,
              CTFP=CTFP,
              C=C,
              P=P,
              BPR=BPR,
              SP=SP,
              NVC=NVC,
              EF=EF,
              nMax=nMax,
              nMin=nMin
  )

  return(data)
}
